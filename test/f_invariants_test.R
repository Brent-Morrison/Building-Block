# Automated structural invariant checks for f().
#
# Unlike test/f_test.R (manual/interactive eyeballing against reference workbooks), this script
# does NOT assert on point values — f()'s correct values legitimately change as functionality is
# built out, so there is no fixed set of numbers to compare against. Instead it checks structural
# invariants that must hold regardless of what the specific numbers are: double-entry balance, RAB
# roll-forward arithmetic, absence of NA/NaN/Inf, and non-negative prices.
#
# Run via: Rscript test/f_invariants_test.R
# Exits 0 if all checks pass, non-zero (via stop()) if any fail.

source("test/f_test_setup.R")

m <- sim[[1]]

tolerance <- 1e-6

failures <- character(0)

check <- function(label, ok) {
  status <- if (isTRUE(ok)) "PASS" else "FAIL"
  cat(sprintf("[%s] %s\n", status, label))
  if (!isTRUE(ok)) failures[[length(failures) + 1]] <<- label
}


# 1. Double-entry balance ---------------------------------------------------------------------
# Every txn-type posting is constructed as debit/credit pairs, so summing across accounts for a
# given txn-type and month should be ~0 (generalises the debug line at R/f.R:461).

txn_balance <- apply(m$txns, c(2, 3), sum)
check(
  "double-entry balance: every txn-type sums to ~0 across accounts, every month",
  all(abs(txn_balance) < tolerance)
)


# 2. RAB roll-forward reconciliation -----------------------------------------------------------
# Mirrors the construction logic at R/f.R:237-241, re-derived independently as a check.

rab <- m$rab
rab_n_yrs <- ncol(rab)

close_reconciles <- sapply(seq_len(rab_n_yrs), function(y) {
  abs(sum(rab[1:6, y]) - rab["close", y]) < tolerance
})
check("RAB roll-forward: open + capex + cust_cont + gov_cont + reg_depn + disp == close, every year", all(close_reconciles))

open_carries_forward <- sapply(2:rab_n_yrs, function(y) {
  abs(rab["open", y] - rab["close", y - 1]) < tolerance
})
check("RAB roll-forward: each year's opening balance equals prior year's closing balance", all(open_carries_forward))


# 3. No NA/NaN/Inf in key outputs --------------------------------------------------------------

check("m$txns has no NA/NaN/Inf", !any(is.na(m$txns)) && !any(is.infinite(m$txns)))
check("m$prices has no NA/NaN/Inf", !any(is.na(m$prices)) && !any(is.infinite(m$prices)))
check("m$rev_req has no NA/NaN/Inf", !any(is.na(m$rev_req)) && !any(sapply(m$rev_req, is.infinite)))
check("m$rab has no NA/NaN/Inf", !any(is.na(m$rab)) && !any(is.infinite(m$rab)))
check("m$tariff_rev has no NA/NaN/Inf", !any(is.na(m$tariff_rev)) && !any(is.infinite(m$tariff_rev)))


# 4. Non-negative prices -----------------------------------------------------------------------

check("m$prices are all non-negative", all(m$prices >= -tolerance))

if (!is.null(m$price_path_check$feasible)) {
  check("m$price_path_check$feasible is TRUE (first price-setting period)", isTRUE(m$price_path_check$feasible))
}


# Summary ---------------------------------------------------------------------------------------

cat("\n")
if (length(failures) == 0) {
  cat("All invariant checks passed.\n")
} else {
  cat(sprintf("%d invariant check(s) failed:\n", length(failures)))
  for (f in failures) cat(sprintf("  - %s\n", f))
  stop(sprintf("%d invariant check(s) failed", length(failures)), call. = FALSE)
}
