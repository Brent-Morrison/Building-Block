# Shared setup for exercising f() outside Shiny.
#
# Sourced by both test/f_test.R (manual/interactive eyeballing) and
# test/f_invariants_test.R (automated structural checks) so the two stay
# aligned on how a scenario is built. Populates dat_df, chart_df, ref_df,
# txn_df, args and sim in the caller's environment.

library(dplyr)
library(tidyr)
library(lubridate)   # R/f.R:468 uses years() for loan tenor calc

source("./R/funs.R")
source("./R/price_path_engine.R")
source("./R/f.R")   # sources R/trgt_days_cpp.cpp itself on load

# Read data ----------------------------------------------------------------------------------------------------------------

d <- get_data()
dat_df   <- d$dat
chart_df <- d$chart
ref_df   <- d$ref
txn_df   <- d$txn_type

a_sens_param <- "xLabour quantity"


args <- expand.grid(
  q_grow             = 0.019,
  cost_of_debt_nmnl  = 0.08,  # c(0.9,0.95,1,1.05,1.1) * 0.04,
  fcast_infltn       = 0.025, # c(0.9,0.95,1,1.05,1.1) * 0.025  || 0.025
  roe                = 0.041,
  desired_fixed      = 99,
  debt_sens          = 0,
  fte                = if (a_sens_param == "Labour quantity") c(180, 200, 220) else 200,
  capex_ps2          = 100,
  capex_ps3          = 100,
  capex_ps4          = 100,
  opex_ps2           = 10,
  opex_ps3           = 10,
  opex_ps4           = 10
  )

sim <- mapply(
  FUN               = f,
  dat               = list(dat_df),
  chart             = list(chart_df),
  txn_type          = list(txn_df),
  ref               = list(ref_df),
  q_grow            = args$q_grow,                # list(0.019),
  cost_of_debt_nmnl = args$cost_of_debt_nmnl,     # list(0.0456),
  fcast_infltn      = args$fcast_infltn,          # list(0.03),
  roe               = args$roe,                   # list(0.041),
  desired_fixed     = args$desired_fixed,
  debt_sens         = args$debt_sens,             # list(-0.01,0,0.01),
  fte               = args$fte,
  capex_ps2         = args$capex_ps2,
  capex_ps3         = args$capex_ps3,
  capex_ps4         = args$capex_ps4,
  opex_ps2          = args$opex_ps2,
  opex_ps3          = args$opex_ps3,
  opex_ps4          = args$opex_ps4,
  SIMPLIFY          = FALSE
)
