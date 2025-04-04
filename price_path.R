source("funs.R")
library(dplyr)
library(tidyr)

# Parameters --------------------------------------------------------------------------------------
dat_src       <- "local"   # "local" / "remote"
ent_parm <- "CW"
initial_fcast_yr <- 2024
price_delta_yr <- 2
cost_of_debt_nmnl <- 0.0456
fcast_infltn <- 0.03
gearing <- 0.6
cost_of_debt_real <- (1 + cost_of_debt_nmnl) / (1 + fcast_infltn) - 1
cost_of_debt_real
roe <- 0.041
rrr <- round((roe * (1 - gearing) + cost_of_debt_real * gearing),4)
rrr



# Data --------------------------------------------------------------------------------------------
if (dat_src == "local") {
  dat_src   <- "./data/price_subm_2023.csv"
} else {
  dat_src   <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/price_subm_2023.csv"
}

dat <- read.csv(dat_src, fileEncoding="UTF-8-BOM")

capex <- dat %>%
  mutate(net_capex = case_when(
    balance_type %in% c("cust_cont","gov_cont") ~ -amount, 
    TRUE ~ amount)
  ) %>%
  filter(entity == ent_parm, balance_type %in% c("cust_cont","gov_cont","gross_capex")) %>%
  select(-c(entity, balance_type, service, asset_category, cost_driver, tax_life, notes, amount)) %>%
  pivot_wider(names_from = year, values_from = net_capex, values_fn = sum, values_fill = 0)

c <- as.matrix(capex[, 3:(ncol(capex))])
colSums(c)

yr_int <- as.integer(colnames(capex)[-c(1:3)])
yr_int

year_operational <- as.integer(sub(".*-", "", capex$year_operational))+2000
year_operational[is.na(year_operational)] <- yr_int[1]
year_operational

yr_op <- match(year_operational, yr_int)
yr_op

#life <- ifelse(capex$regulatory_life == 0, 1, capex$regulatory_life)
life <- capex$regulatory_life
life


# Depreciation on opening RAB ---------------------------------------------------------------------

bv <- dat[dat$entity == ent_parm & dat$year == 2023 & dat$balance_type == "rab_book_value", "amount"]
rl <- dat[dat$entity == ent_parm & dat$year == 2023 & dat$balance_type == "rab_remaining_life", "amount"]
  
# Apply depreciation function over multiple instances
dpn_open_dtl <- mapply(FUN = depn_fun_opn, open_rab_val = bv, open_rab_rem = rl, SIMPLIFY = FALSE)

# Equalise individual lengths to 20 years
dpn_open_dtl <- lapply(dpn_open_dtl, function(x) replace(x[1:20], is.na(x[1:20]), 0))

# To matrix
dpn_open_dtl <- do.call(rbind, dpn_open_dtl)
dpn_open <- colSums(dpn_open_dtl)


# Depreciation on capex ---------------------------------------------------------------------------
dpn_mtrix <- t(mapply(FUN = depn_fun, split(c, row(c)), yr_op = yr_op, life = life))
dpn_mtrix
dpn_cpx <- colSums(dpn_mtrix)
dpn_cpx


# Total depreciation ------------------------------------------------------------------------------
dpn <- dpn_open[1:5] + dpn_cpx[1:5]


# Opex --------------------------------------------------------------------------------------------
opex <- dat %>%
  filter(
    balance_type %in% c("Operations & Maintenance", "External bulk charges (excl. temporary purchases)", 
    "Customer Service and billing", "GSL Payments", "Corporate", "Other operating expenditure",
    "Environment Contribution", "Licence Fees", "Treatment"),
    entity == ent_parm
  ) %>% 
  group_by(year) %>% 
  summarise(amount = sum(amount))


# RAB schedule ------------------------------------------------------------------------------------
# Capex
cx <- colSums(c)[1:5]
unlist(colSums(c)[1:5], use.names = FALSE)

# Customer contributions
cc.t <- rep(0, 5)
y <- seq(initial_fcast_yr, initial_fcast_yr+4, length.out = 5)
if(sum(dat[dat$entity == ent_parm & dat$balance_type == "cust_cont", "amount"]) != 0) {
  ccdf <- aggregate(amount ~ year, data = dat[dat$entity == ent_parm & dat$balance_type == "cust_cont", ], FUN = sum)
  cc.t[which(ccdf$year == y)] <- ccdf$amount[1:5]
  cc <- cc.t
} else {
  cc <- cc.t
}
cc
suppressWarnings(rm(cc.t, ccdf, y))

# Government contributions
gc.t <- rep(0, 5)
y <- seq(initial_fcast_yr, initial_fcast_yr+4, length.out = 5)
if(sum(dat[dat$entity == ent_parm & dat$balance_type == "gov_cont", "amount"]) != 0) {
  gcdf <- aggregate(amount ~ year, data = dat[dat$entity == ent_parm & dat$balance_type == "gov_cont", ], FUN = sum)
  gc.t[which(gcdf$year == y)] <- gcdf$amount[1:5]
  gc <- gc.t
} else {
  gc <- gc.t
}
gc
suppressWarnings(rm(gc.t, gcdf, y))

# Disposals
dp.t <- rep(0, 5)
y <- seq(initial_fcast_yr, initial_fcast_yr+4, length.out = 5)
if(sum(dat[dat$entity == ent_parm & dat$balance_type == "disp_proceeds", "amount"]) != 0) {
  dpdf <- aggregate(amount ~ year, data = dat[dat$entity == ent_parm & dat$balance_type == "disp_proceeds", ], FUN = sum)
  dp.t[which(dpdf$year == y)] <- dpdf$amount[1:5]
  dp <- dp.t
} else {
  dp <- dp.t
}
dp
suppressWarnings(rm(dp.t, dpdf, y))


open_rab_val <- bv
exist_rab_detail <- matrix(rep(0, 8*5), ncol=5)
rownames(exist_rab_detail) <- c("open","capex","cust_cont","gov_cont","reg_depn","disp","close","average")
colnames(exist_rab_detail) <- c(1:5)
exist_rab_detail["open", 1] <- open_rab_val
exist_rab_detail["capex", ] <- cx + cc + gc
exist_rab_detail["cust_cont", ] <- -cc 
exist_rab_detail["gov_cont", ] <- -gc 
exist_rab_detail["reg_depn", ] <- -dpn[1:5]
exist_rab_detail["disp", ] <- -dp 
exist_rab_detail["close", 1] <- sum(exist_rab_detail[1:6, 1])
for(i in 2:5) {
  exist_rab_detail["open", i]  <- exist_rab_detail["close", i-1]
  exist_rab_detail["close", i] <- sum(exist_rab_detail[1:6, i])
}
for(i in 1:5) {
  mment <- exist_rab_detail[2:6, i]
  exist_rab_detail["average", i]  <- exist_rab_detail["open", i] + sum(mment[mment != 0])/2
}

exist_rab_detail


# Return on assets --------------------------------------------------------------------------------
roa <- rrr * exist_rab_detail["average", ]


# Revenue requirement -----------------------------------------------------------------------------
rev_req <- roa + opex$amount[1:5] + dpn[1:5]


# Price & quantity data ---------------------------------------------------------------------------
pq <- dat %>%
  filter(
    balance_type %in% c("Price", "Quantity"),
    entity == ent_parm
  )

# Quantities
q.t1 <- pq %>% filter(entity == ent_parm, year == 2023, balance_type == 'Quantity')  # Pivot wider here
q <- as.matrix(q.t1[, "amount"])
rownames(q) <- paste(q.t1[, "service"], q.t1[, "asset_category"], q.t1[, "cost_driver"], sep = ".")

# Avg. annual consumption (kL per household) 
# TO DO - use this to flex income, ref. line 229.  kL up or down on wet, dry basis
kl_hhold_pa <- q["Water.Residential.Variable", 1][[1]] / q["Water.Residential.Fixed", ][[1]]

# Convert p0 quantities to p1 - p5 with fixed growth rate
# TO DO - 0.05 growth rate to be parameter
q <- q %*% exp( cumsum( log( 1 + rep(0.05, 5) ) ) ) 
#q[grepl("Trade", rownames(q)), ]

# Prices
pq.t1 <- pq %>% filter(entity == ent_parm, year == 2023, balance_type == 'Price')
p0 <- as.matrix(pq.t1[, "amount"])
rownames(p0) <- paste(pq.t1[, "service"], pq.t1[, "asset_category"], pq.t1[, "cost_driver"], sep = ".")


# Perform optimisation ----------------------------------------------------------------------------
optim_result <- optim(
  
  # Initial values for the parameters to be optimized over
  par = c(rrr, 0),
  
  # Function to be minimized, first argument being vector of 
  # parameters over which minimization is applied
  fn  = npv_optim_func,
  
  method = "L-BFGS-B",
  
  # Upper & lower constraints for parameters
  lower = c(rrr - .Machine$double.eps, -0.5),
  upper = c(rrr + .Machine$double.eps,  0.5),
  
  # ... Further arguments to be passed to fn
  pdyr    = price_delta_yr,
  rev_req = rev_req,
  p0      = p0,
  q       = q
  
)
optim_result

optim_result_list <- npv_optim_func(theta=optim_result$par, pdyr=price_delta_yr, rev_req=rev_req, p0=p0, q=q, rtn="data")
price_delta <- optim_result_list$price_delta
prices <- optim_result_list$prices

rev <- prices * q
tot_rev_real <- colSums(prices * q) / 1e6


# Check results
sum(rev_req / (1 + rrr) ^ (1:length(rev_req)))             # NPV of revenue requirement
sum(tot_rev_real / (1 + rrr) ^ (1:length(tot_rev_real)))   # NPV of revenue


cat(
  paste0("Price delta of ", round(price_delta[price_delta != 0] * 100, 2), " percent in year ", price_delta_yr),
  paste0("on revenue requirement of ", round(sum(rev_req), 2)),
  sep = "\n"
)
