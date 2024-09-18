source("funs.R")
library(dplyr)
library(tidyr)

ent_parm <- "CW"

# Depreciation on opening RAB --------------------------------------------
dpn_open <- depn_fun_opn(571.85, 15.92)


# Capex data -------------------------------------------------------------
dat <- read.csv("./data/price_subm_2023.csv")

capex <- dat %>%
  mutate(net_capex = case_when(
    balance_type %in% c("cust_cont","gov_cont") ~ -amount, 
    TRUE ~ amount)
  ) %>%
  filter(entity == "CW", balance_type %in% c("cust_cont","gov_cont","gross_capex")) %>%
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


# Depreciation on capex --------------------------------------------------
dpn_mtrix <- t(mapply(FUN = depn_fun, split(c, row(c)), yr_op = yr_op, life = life))
dpn_mtrix
dpn_cpx <- colSums(dpn_mtrix)
dpn_cpx


# Total depreciation -----------------------------------------------------
dpn <- dpn_open[1:5] + dpn_cpx


# Opex -------------------------------------------------------------------
opex <- dat %>%
  filter(
    balance_type %in% c("Operations & Maintenance", "External bulk charges (excl. temporary purchases)", 
    "Customer Service and billing", "GSL Payments", "Corporate", "Other operating expenditure",
    "Environment Contribution", "Licence Fees", "Treatment"),
    entity == "CW"
  ) %>% 
  group_by(year) %>% 
  summarise(amount = sum(amount))


# RAB schedule -----------------------------------------------------------
open_rab_val <- 591.85
exist_rab_detail <- matrix(rep(0, 8*5), ncol=5)
rownames(exist_rab_detail) <- c("open","capex","cust_cont","gov_cont","reg_depn","disp","close","average")
colnames(exist_rab_detail) <- c(1:5)
exist_rab_detail["open", 1] <- open_rab_val
exist_rab_detail["capex", ] <- colSums(c)[1:5]
exist_rab_detail["reg_depn", ] <- -dpn[1:5]
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


# Return on assets -------------------------------------------------------
rrr = 0.0255
roa <- rrr * exist_rab_detail["average", ]


# Revenue requirement ----------------------------------------------------
rev_req <- roa + opex$amount + dpn
rev_req <- rev_req[1:5]


# Price & quantity data --------------------------------------------------
pq <- dat %>%
  filter(
    balance_type %in% c("Price", "Quantity"),
    entity == ent_parm
  )

# Quantities
q.t1 <- pq %>% filter(year == 2023, balance_type == 'Quantity')  # Pivot wider here
q <- as.matrix(q.t1[, "amount"])
rownames(q) <- paste(q.t1[, "service"], q.t1[, "asset_category"], q.t1[, "cost_driver"], sep = ".")
q <- q %*% exp( cumsum( log( 1 + rep(0.05, 5) ) ) )
#q[grepl("Trade", rownames(q)), ]

# Prices
pq.t1 <- pq %>% filter(year == 2023, balance_type == 'Price')
p0 <- as.matrix(pq.t1[, "amount"])
rownames(p0) <- paste(pq.t1[, "service"], pq.t1[, "asset_category"], pq.t1[, "cost_driver"], sep = ".")


# Perform optimisation ---------------------------------------------------
optim_result <- optim(
  
  # Initial values for the parameters to be optimized over
  par = c(0.0255, 0),
  
  # Function to be minimized, first argument being vector of parameters over which minimization is applied
  fn  = npv_optim_func,
  
  method = "L-BFGS-B",
  
  # Upper & lower constraints for parameters
  lower = c(0.0255 - .Machine$double.eps, -0.5),
  upper = c(0.0255 + .Machine$double.eps,  0.5),
  
  # ... Further arguments to be passed to fn
  pdyr    = 1,
  rev_req = rev_req,
  p0      = p0,
  q       = q
  
)
optim_result

optim_result_list <- npv_optim_func(theta=optim_result$par, pdyr=1, rev_req=rev_req, p0=p0, q=q, rtn="data")
price_delta <- optim_result_list$price_delta
prices <- optim_result_list$prices

rev <- colSums(prices * q) / 1e6

# Check results
sum(rev_req / (1 + 0.0255) ^ (1:length(rev_req))) # NPV of revenue requirement
sum(rev     / (1 + 0.0255) ^ (1:length(rev    ))) # NPV of revenue
