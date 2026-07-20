library(slider)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(scales)

source("test/f_test_setup.R")

m          <- sim[[1]]
tb_summary <- tb(sim, chart_df)   # named to avoid shadowing the tb() function - plot_fins() below calls tb() internally
txns       <- m$txns
prices     <- m$prices
tariff_rev <- m$tariff_rev
rab        <- m$rab
rev_req    <- m$rev_req
loans      <- m$loans

plot_tariffs(sim)
plot_kpi(sim, initial_fcast_yr = 2024)
plot_fins(sim, chart=chart_df, ref=ref_df, sel=c("FY2024","FY2025","FY2028","FY2033","FY2038","FY2043"))
plot_opex_capex(sim)
trial_balance <- tb(sim, chart_df)
fy_cols <- grep("^FY", names(trial_balance))
trial_balance[fy_cols] <- round(trial_balance[fy_cols], 3)
cf(sim, ref_df, 2024)


# --------------------------------------------------------------------------------------------------------------------------
# Scratch pad
# --------------------------------------------------------------------------------------------------------------------------

loan_test_df <- data.frame( gl = colSums(txns[c("4100","4500"),"clos",]), loan_sched = rowSums(loans[,,1]) )
loan_test_df$diff <- loan_test_df$gl + loan_test_df$loan_sched
loan_test_df
z <- rowSums(loans[,,3])

txns[,c("aidb","incm","cshd"), 12 * 1:5]

txns[
  which(rowSums(txns[,c("aidb","incm","cshd"), ]) != 0),  # TB accounts for these txns that are not nil
  c("aidb","incm","cshd"),                                # Transaction types
  1:12                                                    # Months
  ]

txns[
  "4500",                              # TB accounts 
  c("open","repy","borr","clos"),     # Transaction types
  1:240                                  # Months
]

txns["3050", "clos", 12 * 1:20]
txns["3050", , 12 * 1:20]
round(txns["3100", , 1:12],1)
