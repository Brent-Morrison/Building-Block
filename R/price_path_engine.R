library(nloptr)

# ==============================================================================
# Price path optimisation engine
# ==============================================================================
# Every constraint "mode" below reduces to: build a tariff x year matrix of
# period-over-period price deltas from a small parameter vector theta, turn
# that into prices/revenue, and require some function of that revenue to hit
# a target (an equality constraint solved via nloptr). Modes differ only in
# how theta maps to the delta matrix and what the target is.

# NPV of a vector of annual cashflows, discounted with a mid-year adjustment
pp_npv <- function(x, r) {
  sum(x / (1 + r) ^ seq_along(x)) * (1 + r) ^ 0.5
}

# tariff x year delta matrix -> tariff x year price matrix
pp_build_prices <- function(p0, delta) {
  cum <- t(apply(delta, 1, function(x) cumprod(1 + x)))
  cum * p0[, 1]
}

# tariff x year price matrix -> revenue matrix + annual totals
pp_simulate_revenue <- function(prices, q) {
  rev <- prices * q
  list(revenue = rev, by_year = colSums(rev))
}

# Inequality constraint enforcing every simulated price is non-negative.
# nloptr requires g(x) <= 0, so this returns -price for every tariff/year cell.
pp_price_nonneg_ineq <- function(p0, delta_builder) {
  function(theta) {
    prices <- pp_build_prices(p0, delta_builder(theta))
    as.vector(-prices)
  }
}

# Solve a pure-feasibility problem: find theta satisfying eval_g_eq(theta) = 0
# within bounds. Modes here have as many equality constraints as free
# parameters, so there is no real "optimisation" - COBYLA is just used as a
# generic nonlinear equation solver with a flat objective. `spec$p0`/`spec$q`
# are used (rather than relying on caller-scope globals) so a spec built for
# one price-setting period can't accidentally pick up another period's data.
pp_solve_mode <- function(spec, opts = list()) {
  default_opts <- list(
    algorithm = "NLOPT_LN_COBYLA",
    xtol_rel  = 1e-10,
    maxeval   = 20000
  )
  opts <- modifyList(default_opts, opts)

  nloptr_result <- nloptr(
    x0          = spec$x0,
    eval_f      = function(theta) 0,
    lb          = spec$lb,
    ub          = spec$ub,
    eval_g_eq   = spec$eval_g_eq,
    eval_g_ineq = spec$eval_g_ineq,
    opts        = opts
  )

  theta   <- nloptr_result$solution
  delta   <- spec$delta_builder(theta)
  prices  <- pp_build_prices(spec$p0, delta)
  revenue <- pp_simulate_revenue(prices, spec$q)

  # COBYLA is driven by a flat eval_f (this is a feasibility problem, not a
  # true optimisation), so if the equality and inequality constraints are
  # mutually incompatible (e.g. a revenue/price target that can only be met
  # with negative prices) it has no gradient to trade them off and can
  # silently return a point that satisfies neither. Check explicitly rather
  # than trust the solver status.
  feasible <- TRUE
  if (!is.null(spec$eval_g_eq)) {
    eq_violation <- max(abs(spec$eval_g_eq(theta)))
    if (eq_violation > 1) {
      feasible <- FALSE
      warning(sprintf(
        "Revenue target not met (NPV off by %.0f) - constraints may be infeasible together.",
        eq_violation
      ))
    }
  }
  if (!is.null(spec$eval_g_ineq)) {
    ineq_violation <- max(spec$eval_g_ineq(theta))
    if (ineq_violation > 1e-6) {
      feasible <- FALSE
      warning(sprintf(
        "Non-negative price constraint violated (min price %.2f) - constraints may be infeasible together.",
        -ineq_violation
      ))
    }
  }

  list(
    nloptr     = nloptr_result,
    theta      = theta,
    delta      = delta,
    prices     = prices,
    revenue    = revenue,
    npv_target = spec$npv_target,
    npv_actual = pp_npv(revenue$by_year, spec$rrr),
    feasible   = feasible
  )
}

# ==============================================================================
# Mode 1 - single price delta applied to all tariffs, all years
# ==============================================================================
pp_mode_uniform_all_years <- function(p0, q, rrr, npv_target, n_year = 5, lb = -0.5, ub = 0.5) {
  n_tariff <- nrow(p0)

  delta_builder <- function(theta) {
    matrix(theta[1], nrow = n_tariff, ncol = n_year)
  }

  eval_g_eq <- function(theta) {
    prices <- pp_build_prices(p0, delta_builder(theta))
    pp_npv(pp_simulate_revenue(prices, q)$by_year, rrr) - npv_target
  }

  list(
    p0 = p0, q = q, rrr = rrr, npv_target = npv_target, x0 = 0.01, lb = lb, ub = ub,
    eval_g_eq = eval_g_eq, eval_g_ineq = pp_price_nonneg_ineq(p0, delta_builder),
    delta_builder = delta_builder
  )
}

# ==============================================================================
# Mode 2 - single price delta applied to all tariffs, one specified year
# ==============================================================================
# The bump is applied once (in `year`) and persists in all subsequent years
# because it is compounded into price via cumprod - there is no reversion.
pp_mode_uniform_single_year <- function(p0, q, rrr, npv_target, year, n_year = 5, lb = -0.9, ub = 5) {
  n_tariff <- nrow(p0)

  delta_builder <- function(theta) {
    d <- matrix(0, nrow = n_tariff, ncol = n_year)
    d[, year] <- theta[1]
    d
  }

  eval_g_eq <- function(theta) {
    prices <- pp_build_prices(p0, delta_builder(theta))
    pp_npv(pp_simulate_revenue(prices, q)$by_year, rrr) - npv_target
  }

  list(
    p0 = p0, q = q, rrr = rrr, npv_target = npv_target, x0 = 0.05, lb = lb, ub = ub,
    eval_g_eq = eval_g_eq, eval_g_ineq = pp_price_nonneg_ineq(p0, delta_builder),
    delta_builder = delta_builder
  )
}

# ==============================================================================
# Mode 3 - per-service-type deltas hitting target revenue shares
# ==============================================================================
# Each group's NPV revenue depends only on that group's own delta (quantities
# are fixed/exogenous), so with every group explicitly targeted (shares sum to
# 1) this is a fully determined, independent system: one equality constraint
# per group, one free parameter per group.
pp_mode_group_shares <- function(p0, q, rrr, npv_target, group_labels, targets, n_year = 5, lb = -0.5, ub = 0.5) {
  groups <- names(targets)
  stopifnot(all(group_labels %in% groups))
  stopifnot(abs(sum(targets) - 1) < 1e-9)

  group_index <- match(group_labels, groups)
  n_tariff <- nrow(p0)
  G <- length(groups)

  delta_builder <- function(theta) {
    matrix(theta[group_index], nrow = n_tariff, ncol = n_year)
  }

  eval_g_eq <- function(theta) {
    prices <- pp_build_prices(p0, delta_builder(theta))
    rev <- pp_simulate_revenue(prices, q)$revenue
    grp_npv <- sapply(seq_len(G), function(g) {
      pp_npv(colSums(rev[group_index == g, , drop = FALSE]), rrr)
    })
    grp_npv - targets * npv_target
  }

  list(
    p0 = p0, q = q, rrr = rrr, npv_target = npv_target, x0 = rep(0.03, G), lb = rep(lb, G), ub = rep(ub, G),
    eval_g_eq = eval_g_eq, eval_g_ineq = pp_price_nonneg_ineq(p0, delta_builder),
    delta_builder = delta_builder, groups = groups
  )
}

# ==============================================================================
# Mode 4 - variable tariffs pinned to an LRMC price target (year 5), fixed
#   tariffs solved as the residual against the total revenue requirement
# ==============================================================================
# Variable tariff prices follow a constant compounding path from p0 to the
# supplied LRMC target so that price in year `n_year` equals the target
# exactly - this is closed-form and needs no optimisation. Fixed tariffs then
# share a single delta, solved so total NPV revenue meets the requirement.
pp_mode_lrmc_residual <- function(p0, q, rrr, npv_target, variable_mask, lrmc_target, n_year = 5, lb = -0.5, ub = 0.5) {
  n_tariff <- nrow(p0)
  stopifnot(length(lrmc_target) == sum(variable_mask))
  stopifnot(all(rownames(p0)[variable_mask] %in% names(lrmc_target)))
  lrmc_target <- lrmc_target[rownames(p0)[variable_mask]]  # name-match rather than rely on order

  var_delta <- (lrmc_target / p0[variable_mask, 1]) ^ (1 / n_year) - 1

  delta_builder <- function(theta) {
    d <- matrix(0, nrow = n_tariff, ncol = n_year)
    d[variable_mask, ]  <- var_delta
    d[!variable_mask, ] <- theta[1]
    d
  }

  eval_g_eq <- function(theta) {
    prices <- pp_build_prices(p0, delta_builder(theta))
    pp_npv(pp_simulate_revenue(prices, q)$by_year, rrr) - npv_target
  }

  list(
    p0 = p0, q = q, rrr = rrr, npv_target = npv_target, x0 = 0.03, lb = lb, ub = ub,
    eval_g_eq = eval_g_eq, eval_g_ineq = pp_price_nonneg_ineq(p0, delta_builder),
    delta_builder = delta_builder
  )
}

# ==============================================================================
# Mode 5 - variable tariffs pinned to an LRMC price target (year 5), fixed
#   tariffs solved per group to hit each group's target revenue share
# ==============================================================================
# Combines mode 4's LRMC-anchored variable tariffs with mode 3's per-group
# revenue-share targets: within a group, all fixed tariffs move on one shared
# delta (rather than one delta shared across every fixed tariff economy-wide,
# as in pp_mode_lrmc_residual). With every group explicitly targeted (shares
# sum to 1) this is fully determined - one equality constraint per group, one
# free parameter per group - exactly like pp_mode_group_shares, so no
# smoothing objective is needed.
pp_mode_lrmc_group_shares <- function(p0, q, rrr, npv_target, variable_mask, lrmc_target,
                                       group_labels, targets, n_year = 5, lb = -0.5, ub = 0.5) {
  n_tariff <- nrow(p0)
  stopifnot(length(lrmc_target) == sum(variable_mask))
  stopifnot(all(rownames(p0)[variable_mask] %in% names(lrmc_target)))
  lrmc_target <- lrmc_target[rownames(p0)[variable_mask]]  # name-match rather than rely on order

  groups <- names(targets)
  stopifnot(all(group_labels %in% groups))
  stopifnot(abs(sum(targets) - 1) < 1e-9)
  group_index <- match(group_labels, groups)
  G <- length(groups)

  var_delta  <- (lrmc_target / p0[variable_mask, 1]) ^ (1 / n_year) - 1
  fixed_mask <- !variable_mask

  delta_builder <- function(theta) {
    d <- matrix(0, nrow = n_tariff, ncol = n_year)
    d[variable_mask, ] <- var_delta
    d[fixed_mask, ]    <- theta[group_index[fixed_mask]]  # one shared delta per group's fixed tariffs
    d
  }

  eval_g_eq <- function(theta) {
    prices <- pp_build_prices(p0, delta_builder(theta))
    rev <- pp_simulate_revenue(prices, q)$revenue
    grp_npv <- sapply(seq_len(G), function(g) {
      pp_npv(colSums(rev[group_index == g, , drop = FALSE]), rrr)
    })
    grp_npv - targets * npv_target
  }

  list(
    p0 = p0, q = q, rrr = rrr, npv_target = npv_target,
    x0 = rep(0.03, G), lb = rep(lb, G), ub = rep(ub, G),
    eval_g_eq = eval_g_eq, eval_g_ineq = pp_price_nonneg_ineq(p0, delta_builder),
    delta_builder = delta_builder, groups = groups
  )
}
