library(nloptr)

# ==============================================================================
# 1. Data
# ==============================================================================
# The first text prior to the period is the Service.  The last text after the
# period is fixed/variable.

df <- data.frame(
  tariff = c(
    "Rural.All.Fixed",
    "Rural.All.Variable",
    "Sewerage.Non-residential.Fixed",
    "Sewerage.Non-residential.Variable",
    "Trade-Waste.Non-residential.Fixed",
    "Trade-Waste.Non-residential.Variable",
    "Water.Non-residential.Fixed",
    "Water.Non-residential.Variable",
    "Water.Residential.Fixed",
    "Water.Residential.Variable"
  ),
  p0 = c(
    180.1002,
    0.2653,
    696.4600,
    0.9926,
    163.1300,
    0.6565,
    414.4120,
    2.2860,
    234.4724,
    2.2860
  ),
  q0 = c(
    14213,
    4443792,
    6004,
    1386924,
    1463,
    3281205,
    7712,
    6627518,
    74383,
    14275036
  ),
  stringsAsFactors = FALSE
)

n_year <- 5

p0 <- as.matrix(df[, "p0"])
rownames(p0) <- df$tariff

q0 <- as.matrix(df[, "q0"])
rownames(q0) <- df$tariff

# Quantities held flat across the horizon (period 1 - 5)
q <- q0 %*% cumprod(1 + rep(0, n_year))

rrr <- 0.05
rev_req <- c(83393000, 86812000, 90371000, 94076000, 97932000)

# ==============================================================================
# 2. Core engine
# ==============================================================================
# Every constraint "mode" below reduces to: build a tariff x year matrix of
# period-over-period price deltas from a small parameter vector theta, turn
# that into prices/revenue, and require some function of that revenue to hit
# a target (an equality constraint solved via nloptr). Modes differ only in
# how theta maps to the delta matrix and what the target is.

# NPV of a vector of annual cashflows, discounted with a mid-year adjustment
npv <- function(x, r) {
  sum(x / (1 + r) ^ seq_along(x)) * (1 + r) ^ 0.5
}
npv_rev_req <- npv(rev_req, rrr)

# tariff x year delta matrix -> tariff x year price matrix
build_prices <- function(p0, delta) {
  cum <- t(apply(delta, 1, function(x) cumprod(1 + x)))
  cum * p0[, 1]
}

# tariff x year price matrix -> revenue matrix + annual totals
simulate_revenue <- function(prices, q) {
  rev <- prices * q
  list(revenue = rev, by_year = colSums(rev))
}

# Inequality constraint enforcing every simulated price is non-negative.
# nloptr requires g(x) <= 0, so this returns -price for every tariff/year cell.
price_nonneg_ineq <- function(p0, delta_builder) {
  function(theta) {
    prices <- build_prices(p0, delta_builder(theta))
    as.vector(-prices)
  }
}

# Solve a pure-feasibility problem: find theta satisfying eval_g_eq(theta) = 0
# within bounds. Modes here have as many equality constraints as free
# parameters, so there is no real "optimisation" - COBYLA is just used as a
# generic nonlinear equation solver with a flat objective.
solve_mode <- function(spec, opts = list()) {
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

  theta  <- nloptr_result$solution
  delta  <- spec$delta_builder(theta)
  prices <- build_prices(p0, delta)
  revenue <- simulate_revenue(prices, q)

  # COBYLA is driven by a flat eval_f (this is a feasibility problem, not a
  # true optimisation - see comment above), so if the equality and inequality
  # constraints are mutually incompatible (e.g. a revenue/price target that
  # can only be met with negative prices) it has no gradient to trade them
  # off and can silently return a point that satisfies neither. Check
  # explicitly rather than trust the solver status.
  if (!is.null(spec$eval_g_eq)) {
    eq_violation <- max(abs(spec$eval_g_eq(theta)))
    if (eq_violation > 1) {
      warning(sprintf(
        "Revenue target not met (NPV off by %.0f) - constraints may be infeasible together.",
        eq_violation
      ))
    }
  }
  if (!is.null(spec$eval_g_ineq)) {
    ineq_violation <- max(spec$eval_g_ineq(theta))
    if (ineq_violation > 1e-6) {
      warning(sprintf(
        "Non-negative price constraint violated (min price %.2f) - constraints may be infeasible together.",
        -ineq_violation
      ))
    }
  }

  list(
    nloptr  = nloptr_result,
    theta   = theta,
    delta   = delta,
    prices  = prices,
    revenue = revenue
  )
}

print_result <- function(label, result, npv_target) {
  npv_actual <- npv(result$revenue$by_year, rrr)
  cat("\n==============================================================\n")
  cat(label, "\n")
  cat("==============================================================\n")
  cat("Status:            ", result$nloptr$message, "\n")
  cat("Theta:             ", paste(round(result$theta, 6), collapse = ", "), "\n")
  cat("NPV target:        ", format(npv_target, big.mark = ","), "\n")
  cat("NPV actual:        ", format(round(npv_actual), big.mark = ","), "\n")
  cat("NPV difference:    ", round(npv_actual - npv_target, 4), "\n")
}

# ==============================================================================
# 3. Mode 1 - single price delta applied to all tariffs, all 5 years
# ==============================================================================
mode_uniform_all_years <- function(p0, q, rrr, npv_target, n_year = 5) {
  n_tariff <- nrow(p0)

  delta_builder <- function(theta) {
    matrix(theta[1], nrow = n_tariff, ncol = n_year)
  }

  eval_g_eq <- function(theta) {
    prices <- build_prices(p0, delta_builder(theta))
    npv(simulate_revenue(prices, q)$by_year, rrr) - npv_target
  }

  list(
    x0 = 0.01, lb = -0.5, ub = 0.5,
    eval_g_eq = eval_g_eq, eval_g_ineq = price_nonneg_ineq(p0, delta_builder),
    delta_builder = delta_builder
  )
}

result_1 <- solve_mode(mode_uniform_all_years(p0, q, rrr, npv_rev_req))
print_result("Mode 1: uniform delta, all years", result_1, npv_rev_req)

# ==============================================================================
# 4. Mode 2 - single price delta applied to all tariffs, one specified year
# ==============================================================================
# The bump is applied once (in `year`) and persists in all subsequent years
# because it is compounded into price via cumprod - there is no reversion.
mode_uniform_single_year <- function(p0, q, rrr, npv_target, year, n_year = 5) {
  n_tariff <- nrow(p0)

  delta_builder <- function(theta) {
    d <- matrix(0, nrow = n_tariff, ncol = n_year)
    d[, year] <- theta[1]
    d
  }

  eval_g_eq <- function(theta) {
    prices <- build_prices(p0, delta_builder(theta))
    npv(simulate_revenue(prices, q)$by_year, rrr) - npv_target
  }

  list(
    x0 = 0.05, lb = -0.9, ub = 5,
    eval_g_eq = eval_g_eq, eval_g_ineq = price_nonneg_ineq(p0, delta_builder),
    delta_builder = delta_builder
  )
}

result_2 <- solve_mode(mode_uniform_single_year(p0, q, rrr, npv_rev_req, year = 1))
print_result("Mode 2: uniform delta, single year (year 1)", result_2, npv_rev_req)

# ==============================================================================
# 5. Mode 3 - per-service-type deltas hitting target revenue shares
# ==============================================================================
# Each group's NPV revenue depends only on that group's own delta (quantities
# are fixed/exogenous), so with every group explicitly targeted (shares sum to
# 1) this is a fully determined, independent system: one equality constraint
# per group, one free parameter per group.
mode_group_shares <- function(p0, q, rrr, npv_target, group_labels, targets, n_year = 5) {
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
    prices <- build_prices(p0, delta_builder(theta))
    rev <- simulate_revenue(prices, q)$revenue
    grp_npv <- sapply(seq_len(G), function(g) {
      npv(colSums(rev[group_index == g, , drop = FALSE]), rrr)
    })
    grp_npv - targets * npv_target
  }

  list(
    x0 = rep(0.03, G), lb = rep(-0.5, G), ub = rep(0.5, G),
    eval_g_eq = eval_g_eq, eval_g_ineq = price_nonneg_ineq(p0, delta_builder),
    delta_builder = delta_builder, groups = groups
  )
}

group_labels <- sub("\\..*", "", df$tariff)
group_targets <- c(Rural = 0.035, Sewerage = 0.075, "Trade-Waste" = 0.02, Water = 1 - 0.035 - 0.075 - 0.02)

spec_3 <- mode_group_shares(p0, q, rrr, npv_rev_req, group_labels, group_targets)
result_3 <- solve_mode(spec_3)
print_result("Mode 3: per-service-type revenue-share targets", result_3, npv_rev_req)
cat("Group deltas: ", paste(spec_3$groups, round(result_3$theta, 6), sep = " = ", collapse = ", "), "\n")
grp_npv_actual <- sapply(seq_along(spec_3$groups), function(g) {
  npv(colSums(result_3$revenue$revenue[group_labels == spec_3$groups[g], , drop = FALSE]), rrr)
})
cat("Group share (actual vs target):\n")
print(data.frame(
  group  = spec_3$groups,
  actual = round(grp_npv_actual / npv(result_3$revenue$by_year, rrr), 4),
  target = round(group_targets[spec_3$groups], 4)
))

# ==============================================================================
# 6. Mode 4 - variable tariffs pinned to an LRMC price target (year 5), fixed
#    tariffs solved as the residual against the total revenue requirement
# ==============================================================================
# Variable tariff prices follow a constant compounding path from p0 to the
# supplied LRMC target so that price in year `n_year` equals the target
# exactly - this is closed-form and needs no optimisation. Fixed tariffs then
# share a single delta, solved so total NPV revenue meets the requirement.
mode_lrmc_residual <- function(p0, q, rrr, npv_target, variable_mask, lrmc_target, n_year = 5) {
  n_tariff <- nrow(p0)
  stopifnot(length(lrmc_target) == sum(variable_mask))

  var_delta <- (lrmc_target / p0[variable_mask, 1]) ^ (1 / n_year) - 1

  delta_builder <- function(theta) {
    d <- matrix(0, nrow = n_tariff, ncol = n_year)
    d[variable_mask, ]  <- var_delta
    d[!variable_mask, ] <- theta[1]
    d
  }

  eval_g_eq <- function(theta) {
    prices <- build_prices(p0, delta_builder(theta))
    npv(simulate_revenue(prices, q)$by_year, rrr) - npv_target
  }

  list(
    x0 = 0.03, lb = -0.5, ub = 0.5,
    eval_g_eq = eval_g_eq, eval_g_ineq = price_nonneg_ineq(p0, delta_builder),
    delta_builder = delta_builder
  )
}

variable_mask <- grepl("Variable", df$tariff)
# Example LRMC target prices (year 5) for each variable tariff, in tariff order
lrmc_target <- c(
  "Rural.All.Variable"                   = 0.35,
  "Sewerage.Non-residential.Variable"    = 1.40,
  "Trade-Waste.Non-residential.Variable" = 1.10,
  "Water.Non-residential.Variable"       = 3.20,
  "Water.Residential.Variable"           = 3.20
)
lrmc_target <- lrmc_target[df$tariff[variable_mask]]

spec_4 <- mode_lrmc_residual(p0, q, rrr, npv_rev_req, variable_mask, lrmc_target)
result_4 <- solve_mode(spec_4)
print_result("Mode 4: variable tariffs at LRMC target, fixed tariffs residual", result_4, npv_rev_req)
cat("Fixed tariff delta:", round(result_4$theta[1], 6), "\n")
cat("Variable tariff prices (year 5) vs LRMC target:\n")
print(data.frame(
  tariff = names(lrmc_target),
  price_yr5 = round(result_4$prices[variable_mask, n_year], 4),
  target = round(lrmc_target, 4)
))
