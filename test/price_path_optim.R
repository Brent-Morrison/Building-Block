source("R/price_path_engine.R")

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
npv_rev_req <- pp_npv(rev_req, rrr)

# ==============================================================================
# 2. Demo helper
# ==============================================================================
print_result <- function(label, result, npv_target) {
  npv_actual <- pp_npv(result$revenue$by_year, rrr)
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
result_1 <- pp_solve_mode(pp_mode_uniform_all_years(p0, q, rrr, npv_rev_req))
print_result("Mode 1: uniform delta, all years", result_1, npv_rev_req)

# ==============================================================================
# 4. Mode 2 - single price delta applied to all tariffs, one specified year
# ==============================================================================
result_2 <- pp_solve_mode(pp_mode_uniform_single_year(p0, q, rrr, npv_rev_req, year = 1))
print_result("Mode 2: uniform delta, single year (year 1)", result_2, npv_rev_req)

# ==============================================================================
# 5. Mode 3 - per-service-type deltas hitting target revenue shares
# ==============================================================================
group_labels <- sub("\\..*", "", df$tariff)
group_targets <- c(Rural = 0.035, Sewerage = 0.075, "Trade-Waste" = 0.02, Water = 1 - 0.035 - 0.075 - 0.02)

spec_3 <- pp_mode_group_shares(p0, q, rrr, npv_rev_req, group_labels, group_targets)
result_3 <- pp_solve_mode(spec_3)
print_result("Mode 3: per-service-type revenue-share targets", result_3, npv_rev_req)
cat("Group deltas: ", paste(spec_3$groups, round(result_3$theta, 6), sep = " = ", collapse = ", "), "\n")
grp_npv_actual <- sapply(seq_along(spec_3$groups), function(g) {
  pp_npv(colSums(result_3$revenue$revenue[group_labels == spec_3$groups[g], , drop = FALSE]), rrr)
})
cat("Group share (actual vs target):\n")
print(data.frame(
  group  = spec_3$groups,
  actual = round(grp_npv_actual / pp_npv(result_3$revenue$by_year, rrr), 4),
  target = round(group_targets[spec_3$groups], 4)
))

# ==============================================================================
# 6. Mode 4 - variable tariffs pinned to an LRMC price target (year 5), fixed
#    tariffs solved as the residual against the total revenue requirement
# ==============================================================================
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

spec_4 <- pp_mode_lrmc_residual(p0, q, rrr, npv_rev_req, variable_mask, lrmc_target)
result_4 <- pp_solve_mode(spec_4)
print_result("Mode 4: variable tariffs at LRMC target, fixed tariffs residual", result_4, npv_rev_req)
cat("Fixed tariff delta:", round(result_4$theta[1], 6), "\n")
cat("Variable tariff prices (year 5) vs LRMC target:\n")
print(data.frame(
  tariff = names(lrmc_target),
  price_yr5 = round(result_4$prices[variable_mask, n_year], 4),
  target = round(lrmc_target, 4)
))
