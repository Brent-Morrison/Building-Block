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
args <- expand.grid(q_grow = 0.019, cost_of_debt_nmnl=0.04, fcast_infltn=0.025, roe=0.041, single_price_delta=T, debt_sens = 0, oxcx_scenario = "scnr1")

sim <- mapply(
  FUN               = f, 
  dat               = list(dat_df), 
  chart             = list(chart_df), 
  cx_delta          = list(cx_df), 
  ox_delta          = list(ox_df), 
  txn_type          = list(txn_df), 
  q_grow            = args$q_grow,                # list(0.019), 
  cost_of_debt_nmnl = args$cost_of_debt_nmnl,     # list(0.0456), 
  fcast_infltn      = args$fcast_infltn,          # list(0.03), 
  roe               = args$roe,                   # list(0.041), 
  single_price_delta= args$single_price_delta,
  debt_sens         = args$debt_sens,             # list(-0.01,0,0.01), 
  oxcx_scenario     = args$oxcx_scenario,         # list("scnr1","scnr4"),
  SIMPLIFY          = FALSE
)

plot_kpi(sim, initial_fcast_yr = 2024)
plot_fins(sim, chart=chart_df, ref=ref_df, sel=c("FY2024","FY2025","FY2028","FY2033","FY2038","FY2043"))


