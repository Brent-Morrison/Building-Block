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
  cost_of_debt_nmnl  = 0.04, 
  fcast_infltn       = 0.025, 
  roe                = 0.041, 
  single_price_delta = T, 
  desired_fixed      = 99,
  debt_sens          = 0, 
  oxcx_scenario      = "scnr1"
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
  SIMPLIFY          = FALSE
)

prices     <- sim[[1]]$prices
txns       <- sim[[1]]$txns
tariff_rev <- sim[[1]]$tariff_rev

# Plot tariff revenue
ggplot() +
  geom_bar(
    data = data.frame(
      income_type = c( rep("Fixed", 20), rep("Variable", 20) ),
      year = rep(2024:2043, 2),
      income = c(colSums(tariff_rev[grepl("Fixed", rownames(tariff_rev)),]), colSums(tariff_rev[grepl("Variable", rownames(tariff_rev)),]) ) /1e3
    ),
    aes(fill=forcats::fct_rev(income_type), y=income, x=year), position="stack", stat="identity", colour="black"
  ) +
  scale_fill_manual(values = c("grey60", "grey30")) +
  geom_point(
    data = data.frame(
      year = 2024:2043,
      perc_fixed = colSums( tariff_rev[grepl("Fixed", rownames(tariff_rev)),] ) / colSums(tariff_rev) * max(colSums(tariff_rev)) / 1e3
    ),
    aes(x = year, y = perc_fixed), colour="black", shape = 15, size = 3
  ) +
  scale_y_continuous("", sec.axis = sec_axis(~ . / (max(colSums(tariff_rev)) / 1e3), name = "")) +
  scale_x_continuous(breaks = c(2028,2033,2038,2043)) +
  ggthemes::theme_base() +
  labs(
    x = "",
    y = "",
    title = "Tariff revenue - fixed vs variable (left axis, $m)",
    subtitle = "Proportion of fixed tariff revenue (right axis, square points)"
  ) + 
  theme(
    plot.subtitle = element_text(size = 11.5, face = 'italic'),
    legend.title = element_blank(),
    #legend.position = c(0.1,0.9),
    legend.background = element_blank(),
    legend.key = element_blank(),
  )

plot_tariffs(sim)
plot_kpi(sim, initial_fcast_yr = 2024)
plot_fins(sim, chart=chart_df, ref=ref_df, sel=c("FY2024","FY2025","FY2028","FY2033","FY2038","FY2043"))
trial_balance <- round( tb(sim, chart_df, ref_df) / 1, 3 )

