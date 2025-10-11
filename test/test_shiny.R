library(dplyr)
library(tidyr)
library(slider)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(scales)

source("./R/funs.R")
source("./R/f.R")

# Read data ----------------------------------------------------------------------------------------------------------------

d <- get_data()
dat_df    <- d$dat
chart_df  <- d$chart 
ref_df    <- d$ref
txn_df    <- d$txn_type
cx_df     <- d$cx_delta 
ox_df     <- d$ox_delta


#args <- expand.grid(q_grow=0.019, cost_of_debt_nmnl=0.0456, fcast_infltn=0.03, roe=0.041, debt_sens = 0, oxcx_scenario = c("scnr1","scnr4"))
args <- expand.grid(
  q_grow             = 0.019, 
  cost_of_debt_nmnl  = 0.04,  # c(0.9,0.95,1,1.05,1.1) * 0.04, 
  fcast_infltn       = 0.025, # c(0.9,0.95,1,1.05,1.1) * 0.025  || 0.025
  roe                = 0.041, 
  single_price_delta = T, 
  desired_fixed      = 99,
  debt_sens          = 0, 
  oxcx_scenario      = "scnr1",
  capex_ps2          = 500,
  capex_ps3          = 500,
  capex_ps4          = 500
  )

sim <- mapply(
  FUN               = f, 
  dat               = list(dat_df), 
  chart             = list(chart_df), 
  cx_delta          = list(cx_df), 
  ox_delta          = list(ox_df), 
  txn_type          = list(txn_df), 
  ref               = list(ref_df),
  q_grow            = args$q_grow,                # list(0.019), 
  cost_of_debt_nmnl = args$cost_of_debt_nmnl,     # list(0.0456), 
  fcast_infltn      = args$fcast_infltn,          # list(0.03), 
  roe               = args$roe,                   # list(0.041), 
  single_price_delta= args$single_price_delta,
  desired_fixed     = args$desired_fixed,
  debt_sens         = args$debt_sens,             # list(-0.01,0,0.01), 
  oxcx_scenario     = args$oxcx_scenario,         # list("scnr1","scnr4"),
  capex_ps2         = args$capex_ps2,
  capex_ps3         = args$capex_ps3,
  capex_ps4         = args$capex_ps4,
  SIMPLIFY          = FALSE
)

tb         <- tb(sim, chart_df, ref_df)
txns       <- sim[[1]]$txns
prices     <- sim[[1]]$prices
tariff_rev <- sim[[1]]$tariff_rev
rab        <- sim[[1]]$rab
rev_req    <- sim[[1]]$rev_req

plot_tariffs(sim)
plot_kpi(sim, initial_fcast_yr = 2024)
plot_fins(sim, chart=chart_df, ref=ref_df, sel=c("FY2024","FY2025","FY2028","FY2033","FY2038","FY2043"))
plot_opex_capex(sim)
trial_balance <- round( tb(sim, chart_df, ref_df) / 1, 3 )


# --------------------------------------------------------------------------------------------------------------------------


txns[,c("aidb","incm","cshd"), 12 * 1:5]

txns[
  which(rowSums(txns[,c("aidb","incm","cshd"), ]) != 0),  # TB accounts for these txns that are not nil
  c("aidb","incm","cshd"),                                # Transaction types
  1:12                                                    # Months
  ]

txns["3050", "clos", 12 * 1:20]
txns["3050", , 12 * 1:20]
round(txns["3100", , 1:12],1)
