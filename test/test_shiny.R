library(dplyr)
library(tidyr)
library(slider)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(scales)

source("./R/funs.R")
source("./R/f.R")

d <- get_data()

args <- expand.grid(q_grow=0.019, cost_of_debt_nmnl=0.0456, fcast_infltn=0.03, roe=0.041, debt_sens = c(-0.01,0,0.01), oxcx_scenario = c("scnr1","scnr4"))
#args <- expand.grid(q_grow = 0.019, cost_of_debt_nmnl=0.0456, fcast_infltn=0.03, roe=0.041, debt_sens = 0, oxcx_scenario = "scnr1")

sim <- mapply(
  FUN               = f, 
  dat               = list(d$dat), 
  chart             = list(d$chart), 
  cx_delta          = list(d$cx_delta), 
  ox_delta          = list(d$ox_delta), 
  txn_type          = list(d$txn_type), 
  q_grow            = args$q_grow,                # list(0.019), 
  cost_of_debt_nmnl = args$cost_of_debt_nmnl,     # list(0.0456), 
  fcast_infltn      = args$fcast_infltn,          # list(0.03), 
  roe               = args$roe,                   # list(0.041), 
  debt_sens         = args$debt_sens,             # list(-0.01,0,0.01), 
  oxcx_scenario     = args$oxcx_scenario,         # list("scnr1","scnr4"),
  SIMPLIFY          = FALSE
)

plot_kpi(sim)