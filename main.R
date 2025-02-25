library(dplyr)
library(tidyr)
library(slider)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(scales)



# Parameters --------------------------------------------------------------------------------------
src                <- "local"   # "local" / "remote"
ent_parm           <- "CW"      # select data for specific entity
initial_fcast_yr   <- 2024
price_delta_yr     <- 2         # for function npv_optim_func, an integer between 0 and 5 representing the year in which
                                # price delta 2 comes into effect, a value of zero returns an even price delta for each year 
single             <- F         # for function npv_optim_func, if true the only price delta (that of pdpar1) occurs
pd_max_per         <- 1         # price delta max period, if single is F, specify which price delta should be higher
cost_of_debt_nmnl  <- 0.0456    # nominal cost of debt
fcast_infltn       <- 0.03
gearing            <- 0.6
cost_of_debt_real  <- (1 + cost_of_debt_nmnl) / (1 + fcast_infltn) - 1
roe                <- 0.041
rrr                <- round((roe * (1 - gearing) + cost_of_debt_real * gearing), 4)


# Data --------------------------------------------------------------------------------------------
if (src == "local") {
  dat_src   <- "./data/price_subm_2023.csv"
  ref_src   <- "./data/reference.csv"
  funs_src  <- "funs.R"
} else {
  dat_src   <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/price_subm_2023.csv"
  ref_src   <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/reference.csv"
  funs_src  <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/funs.R"
}

dat <- read.csv(dat_src, fileEncoding="UTF-8-BOM")
ref <- read.csv(ref_src, fileEncoding="UTF-8-BOM")
source(funs_src)


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
dpn_cpx <- colSums(dpn_mtrix)



# Total depreciation ------------------------------------------------------------------------------
dpn <- dpn_open[1:5] + dpn_cpx[1:5]



# Opex --------------------------------------------------------------------------------------------
# TO DO - how are these costs ("Operations & Maintenance", "Customer Service and billing") split into wages and other
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
  par = c(rrr, if (pd_max_per == 1) c(0.025, 0) else c(0, 0.025)),
  
  # Function to be minimized, first argument being vector of 
  # parameters over which minimization is applied
  fn  = npv_optim_func,
  
  method = "L-BFGS-B",
  
  # Upper & lower constraints for parameters
  lower = c(rrr - .Machine$double.eps, -0.5, -0.5),
  upper = c(rrr + .Machine$double.eps,  0.5,  0.5),
  
  # ... Further arguments to be passed to fn
  pdyr    = price_delta_yr,
  single  = single,
  rev_req = rev_req,
  p0      = p0,
  q       = q
  
)
optim_result

optim_result_list <- npv_optim_func(theta=optim_result$par, pdyr=price_delta_yr, single=single, rev_req=rev_req, p0=p0, q=q, rtn="data")
price_delta <- optim_result_list$price_delta
prices <- optim_result_list$prices

rev <- prices * q
tot_rev_real <- colSums(prices * q) / 1e6


# Check results
sum(rev_req / (1 + rrr) ^ (1:length(rev_req)))             # NPV of revenue requirement
sum(tot_rev_real / (1 + rrr) ^ (1:length(tot_rev_real)))   # NPV of revenue


# Print outcome
cat(
  paste0("Price delta of ", round(price_delta[price_delta != 0] * 100, 2), " percent in year ", price_delta_yr),
  paste0("on revenue requirement of ", round(sum(rev_req), 2)),
  sep = "\n"
)
round(price_delta * 100, 2)



# -------------------------------------------------------------------------------------------------
# Matrix accounting set up
# -------------------------------------------------------------------------------------------------

# Parameters 
mons          <- 60           # months to forecast
open_bals_col <- "cw_23"      # column in the 'chart.csv' file representing the entities data
infltn_factor <- exp(cumsum( log(1 + rep(fcast_infltn, 5)) ))
month_end     <- seq(as.Date("2024-01-31") + 1, by = "month", length.out = mons) - 1
days          <- as.numeric(format(month_end, "%d"))
accrued_days  <- 60
debtors_days  <- 45
crdtr_days_ox <- 90
crdtr_days_cx <- 45

# Data sources
if (src == "local") {
  chart_src   <- "./data/chart.csv"
  txn_src     <- "./data/txn_type.csv"
} else {
  chart_src   <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/chart.csv"
  txn_src     <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/txn_type.csv"
}

chart    <- read.csv(chart_src, fileEncoding="UTF-8-BOM")
txn_type <- read.csv(txn_src, fileEncoding="UTF-8-BOM")
rownames(txn_type) <- txn_type$txn_code



# Matrix ------------------------------------------------------------------------------------------
mon <- 1:mons   # Number of months
txn <- unlist(txn_type[,"txn_code"], use.names = FALSE)  # Transaction types
act <- unlist(chart[,"account_no"], use.names = FALSE)  # GL accounts


# Create matrix and assign names 
mat <- array(rep(0, length(act) * length(txn) * length(mon)), dim=c(length(act), length(txn), length(mon)))
dimnames(mat)[[1]] <- act
dimnames(mat)[[2]] <- txn
dimnames(mat)[[3]] <- mon


# Insert opening balances
opn_bal <- unlist(chart[,open_bals_col], use.names = FALSE)
mat[ , "open", 1]  <- opn_bal


# Rollover and update retained earning
mat["5200","open",1] <- mat["5200","open",1] + sum(mat[as.character(chart[chart$statement_type == 1, ]$account_no), "open" ,1])
mat[as.character(chart[chart$statement_type == 1, ]$account_no), "open" ,1] <- 0


# Check
round(colSums(mat[,,1]), 3)



# -------------------------------------------------------------------------------------------------
# Transaction balances
# https://stackoverflow.com/questions/19340401/convert-a-row-of-a-data-frame-to-a-simple-vector-in-r
# -------------------------------------------------------------------------------------------------

# Income 
tot_rev_nmnl <- tot_rev_real * infltn_factor * 1e3
incm <- round(as.vector(sapply(X = tot_rev_nmnl, FUN = add_trend_season, s=0, a=1, p=1.5)), 3)
gift <- round(rep(cc / 12, each = 12), 3)


# Expenses 
exp1 <- unlist(opex[opex$year %in% initial_fcast_yr:(initial_fcast_yr + 4), "amount"], use.names = FALSE) * 1000 * infltn_factor
exp1 <- round(as.vector(sapply(X = exp1, FUN = add_trend_season, s=0, a=0, p=0)), 3)


# Capex 
cpx1 <- round(rep(cx / 12, each = 12), 3) * 1000


# Depreciation 
# - on opening balance
stat_depn_bld <- depn_bv(
  yrs=5, 
  de=chart[chart$account_no == 2215, "cw_23"], 
  gr=chart[chart$account_no == 3510, "cw_23"], 
  ad=chart[chart$account_no == 3515, "cw_23"]
)

stat_depn_lhi <- depn_bv(
  yrs=5, 
  de=chart[chart$account_no == 2225, "cw_23"], 
  gr=chart[chart$account_no == 3520, "cw_23"], 
  ad=chart[chart$account_no == 3525, "cw_23"]
)

stat_depn_pae <- depn_bv(
  yrs=5, 
  de=chart[chart$account_no == 2235, "cw_23"], 
  gr=chart[chart$account_no == 3530, "cw_23"], 
  ad=chart[chart$account_no == 3535, "cw_23"]
)

stat_depn_inf <- depn_bv(
  yrs=5, 
  de=chart[chart$account_no == 2245, "cw_23"], 
  gr=chart[chart$account_no == 3540, "cw_23"], 
  ad=chart[chart$account_no == 3545, "cw_23"]
)

stat_depn_sca <- depn_bv(
  yrs=5, 
  de=chart[chart$account_no == 2265, "cw_23"], 
  gr=chart[chart$account_no == 3560, "cw_23"], 
  ad=chart[chart$account_no == 3565, "cw_23"]
)

stat_depn_int <- depn_bv(
  yrs=5, 
  de=chart[chart$account_no == 2205, "cw_23"], 
  gr=chart[chart$account_no == 3600, "cw_23"], 
  ad=chart[chart$account_no == 3605, "cw_23"]
)



# - on capex (balances moved from WIP)
dpn1 <- rep(50, mons)                      # TO DO - create depn schedule re opening balances and capex, assume transfer from WIP to asset register
dpn_mtrix <- t(mapply(FUN = depn_fun, split(c, row(c)), yr_op = yr_op, life = life))
dpn_cpx <- colSums(dpn_mtrix)
dpn_cpx <- rep(dpn_cpx / 5, each = 12)


# -------------------------------------------------------------------------------------------------
# Transaction loop
# -------------------------------------------------------------------------------------------------

for (i in 1:length(mon)) {
  
  # Opening balances & debtors ageing post loop / month 1 -----------------------------------------
  if (i > 1) {
    
    # Opening balances
    mat[, "open", i] <- mat[, "clos", i-1]
    
  }
  
  
  # Post income (DR accrued income / CR income) ---------------------------------------------------
  t <- "aidb"
  mat[drcr(t, txn_type), t, i] <- c(incm[i], -incm[i])
  
  
  # Cash receipt to specify desired closing balance for accrued income days parameter -------------
  trail <- 3
  if (i < trail) s <- 1 else s <- i - (trail - 1)
  trail_inc <- -mean(mat["1000", "aidb", s:i]) * trail
  sum_days <- mean(days[s:i]) * trail
  prior_bals <- mean(mat["3050", "open", s:i]) * (trail - 1)
  tfer <- round( abs(accrued_days * trail_inc / sum_days * 3 - prior_bals - mat["3050", "open", i] + mat["1000", "aidb", i]) , 3)
  t <- "incm"
  mat[drcr(t, txn_type), t, i] <- c(tfer, -tfer)
  
  
  # Cash receipt to specify desired closing balance for debtors days parameter --------------------
  trail <- 3
  if (i < trail) s <- 1 else s <- i - (trail - 1)
  trail_inc <- -mean(mat["3050", "incm", s:i]) * trail
  sum_days <- mean(days[s:i]) * trail
  prior_bals <- mean(mat["3100", "open", s:i]) * (trail - 1)
  desired_bal <- debtors_days * trail_inc / sum_days * 3 - prior_bals
  rcpt <- round( abs(desired_bal - mat["3100", "open", i] + mat["3050", "incm", i]) , 3)
  t <- "cshd"
  mat[drcr(t, txn_type), t, i] <- c(rcpt, -rcpt)
  
  
  # Bad debts WO
  # wo <- mat[,,i]["3053", "opn"] + mat[,,i]["3053", "csh"]
  # mat[,,i]["3053", "wof"] <- -wo
  # mat[,,i]["270", "wof"] <- wo
  
  
  # Income re gifted assets -----------------------------------------------------------------------
  t <- "gift"
  mat[drcr(t, txn_type), t, i] <- c(gift[i], -gift[i])
  
  
  # Other income TO DO ----------------------------------------------------------------------------
  
  
  # Interest income TO DO -------------------------------------------------------------------------
  
  
  # Expenses --------------------------------------------------------------------------------------
  # TO DO - ADD EMPLOYEE EXPENSES (AC 2100)
  t <- "exp1"
  mat[drcr(t, txn_type), t, i] <- c(exp1[i], -exp1[i])
  
  
  # Cash payment re trade creditors ---------------------------------------------------------------
  trail <- 3
  rcpt <- trgt_days(i, d=crdtr_days_ox, trail=3, bal_acnt="4000", pl_acnt="2000", txn="exp1")
  t <- "crd1"
  mat[drcr(t, txn_type), t, i] <- c(rcpt, -rcpt)
  
  
  # Capex -----------------------------------------------------------------------------------------
  t <- "cpx1"
  mat[drcr(t, txn_type), t, i] <- c(cpx1[i], -cpx1[i])
  
  
  # Cash payment re capex -------------------------------------------------------------------------
  trail <- 3
  rcpt <- trgt_days(i, d=crdtr_days_cx, trail=3, bal_acnt="4010", pl_acnt="3645", txn="cpx1")
  t <- "wipc"
  mat[drcr(t, txn_type), t, i] <- c(rcpt, -rcpt)
  
  
  # Interest (accrue) -----------------------------------------------------------------------------
  int <- round(sum(mat[c("4100","4500"), "open", i]) * cost_of_debt_nmnl / 12, 3)
  t <- "inta"
  mat[drcr(t, txn_type), t, i] <- c(-int, int)
  
  
  # Interest (pay quarterly) ----------------------------------------------------------------------
  if (i %in% seq(0, mons, by = 3)) {
    t <- "intp"
    intp <- -mat["4020", "open", i]
    mat[drcr(t, txn_type), t, i] <- c(intp, -intp)
  }
  
  
  # Depreciation ----------------------------------------------------------------------------------
  t <- "dpn1"
  p <- c(
    stat_depn_bld[i], -stat_depn_bld[i],
    stat_depn_lhi[i], -stat_depn_lhi[i],
    stat_depn_pae[i], -stat_depn_pae[i],
    stat_depn_inf[i]+dpn_cpx[i], -stat_depn_inf[i]-dpn_cpx[i], # assumes all capex is infrastructure
    stat_depn_pae[i], -stat_depn_pae[i],
    stat_depn_sca[i], -stat_depn_sca[i]
  )
  a <- drcr(t, txn_type)
  if (sum(p) == 0 & length(p) == length(a)) {
    mat[a, t, i] <- p
  } else if (sum(p) != 0) {
    print("Depreciation not posted, accounting entries do not balance to nil")
  } else if (length(p) != length(a)) {
    print(paste0("Depreciation not posted.  Posting data has length ", length(p), " and posting rule has length ", length(a)))
  }
  
  
  # Determine if borrowings required --------------------------------------------------------------
  # TO DO - determine amount to borrow dynamically
  borrow_amt <- 10000
  cash_bal <- sum(mat["3000",-ncol(mat[,,i]), i])
  if (cash_bal < 0) {
    t <- "borr"
    mat[drcr(t, txn_type), t, i] <- c(borrow_amt,-borrow_amt)
  }
  
  # Update closing balance
  mat[, "clos", i] <- rowSums(mat[,-ncol(mat[,,i]), i])
  
}
#mat[,,1]
#mat[,,60]



# Check balances
round(colSums(mat[,,6]), 3)



# -------------------------------------------------------------------------------------------------
# Accounting data (3d matrix to 2d data frame)
# -------------------------------------------------------------------------------------------------

# Stack 3 dim array and unpivot
mat1 <- mat
dim(mat1) <- c(length(act)*length(txn)*length(mon), 1)

df <- expand.grid(act = act, txn = txn, mon = mon)
df$mtd <- mat1[,1]

df1 <- df

# Retain opening balances only for balance sheet, remove all closing balances
df2 <- rbind(df1[df1$txn == "open" & df1$act >= 3000, ], df1[df1$txn %in% txn[-c(1, length(txn))], ])

# Remove opening balance for P&L accounts
#df2[df2$txn == "open" , c("mtd","ytd")] <- 0

# Cumulative sum
df2$yr <- ceiling(df2$mon / 12)
df2$ltd <- ave(df2$mtd, df2$act, df2$txn, FUN=cumsum)
df2$ytd <- ave(df2$mtd, df2$act, df2$txn, df2$yr, FUN=cumsum)

# Order and select columns
df2 <- df2[with(df2, order(mon, act, txn)), c("yr","mon","act","txn","mtd","ytd","ltd")]  

# Insert LTD opening balance into all months opening balance
df3 <- df2[df2$txn == "open" & df2$yr == 1 & df2$mon == 1, ] # Get global P1 opening balance for B/S accounts
for (i in df3$act) {
  insert = df3[df3$act == i, "ltd"]
  df2[df2$txn == "open" & df2$act == i, "ltd"] <- insert
}

# Insert YTD opening balances into all month opening balance
df4 <- df2[df2$txn == "open" & df2$mon %in% (1:5*12 - 11) & df2$act >= 3000, ]  # Get annual P1 opening balance for B/S accounts
for (i in unique(df4$act)) {
  for (j in (1:(mons/12))) {
    insert = df4[df4$act == i & df4$yr == j, "mtd"]
    df2[df2$txn == "open" & df2$act == i & df2$yr == j, "ytd"] <- insert
  }
}

# Remove nil balances and write to csv
df2 <- df2[df2$mtd != 0 & df2$ytd != 0 & df2$ltd != 0, ]
write.csv(df2, file = "slr.csv")

slr <- left_join(df2, chart[1:6], by = join_by(act == account_no))



# -------------------------------------------------------------------------------------------------
# Financial indicators
# -------------------------------------------------------------------------------------------------

# https://www.audit.vic.gov.au/report/auditor-generals-report-annual-financial-report-state-victoria-2017-18?section=33061&show-sections=1#33059--appendix-f-water-sector-financial-sustainability-risk-indicators
# Cash interest cover      - Net operating cash flows before net interest and tax payments / Net interest payments
# Gearing ratio            - Total debt (including finance leases) / Total assets
# Internal financing ratio - Net operating cash flows less dividends / Net capital expenditure
# Current ratio            - Current assets / Current liabilities (excluding long-term employee provisions and revenue in advance)
# Return on assets         - Earnings before net interest and tax / average assets
# Return on equity         - Net profit after tax / average total equity
# EBITDA margin            - Earnings before interest, tax, depreciation and amortisation / Total revenue


monthly_indicators <- slr %>%
  mutate(
    net_income   = if_else(statement_type == 1, mtd, 0),
    ebitda       = if_else(account_grp %in% c(100,110,130,150,200,210,235,240), mtd, 0),
    net_int_pay  = if_else(act == 4020 & txn == "intp", mtd, 0),
    curr_assets  = if_else(account_type == 30, ytd, 0),
    total_assets = if_else(account_type %in% c(30,35), ytd, 0),
    curr_liabs   = if_else(account_type == 40, ytd, 0),
    total_debt   = if_else(act %in% c(4100,4500), ytd, 0),
    cf_rec_cust  = if_else(cf_flag == "rec_cus" & txn != "open", ytd, 0),
    cf_rec_oth   = if_else(cf_flag == "rec_oth" & txn != "open", ytd, 0),
    cf_pay_sup   = if_else(cf_flag == "pay_sup" & txn != "open", ytd, 0),
    cf_pay_int   = if_else(cf_flag == "pay_int" & txn != "open", ytd, 0),
    cf_pay_ppe   = if_else(cf_flag == "pay_ppe" & txn != "open", ytd, 0)
  ) %>%
  group_by(yr, mon) %>%
  summarise(
    net_income   = round(sum(net_income), 0),
    ebitda       = round(sum(ebitda), 0),
    net_int_pay  = round(sum(net_int_pay), 0),
    curr_assets  = round(sum(curr_assets), 0),
    total_assets = round(sum(total_assets), 0),
    curr_liabs   = round(sum(curr_liabs), 0),
    total_debt   = round(sum(total_debt), 0),
    cf_rec_cust  = round(sum(cf_rec_cust), 0),
    cf_rec_oth   = round(sum(cf_rec_oth), 0),
    cf_pay_sup   = round(sum(cf_pay_sup), 0),
    cf_pay_int   = round(sum(cf_pay_int), 0),
    cf_pay_ppe   = round(sum(cf_pay_ppe), 0)
  ) %>%
  ungroup() %>% 
  mutate(
    net_income_ttm   = slide_sum(net_income, before = 12),
    total_assets_ttm = slide_mean(total_assets, before = 12),
    ebitda_ttm       = slide_sum(ebitda, before = 12),
    net_int_pay_ttm  = slide_sum(net_int_pay, before = 12),
    cash_int_cover   = -ebitda_ttm / net_int_pay_ttm,
    gearing          = -total_debt / total_assets,
    int_fin_ratio    = -( cf_rec_cust + cf_rec_oth + cf_pay_sup ) / cf_pay_ppe,
    current_ratio    = curr_assets / -curr_liabs,
    ret_on_asset     = -net_income / total_assets_ttm
    #ret_on_eqt     =
    #ebitda_mgn     =
    #average customer bill
  ) 





# ---------------------------------------------------------------------------------------------------------
# Plots
# ---------------------------------------------------------------------------------------------------------

# Custom theme
cust_theme1 <- theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position.inside = c(0.9,0.9),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption = element_text(size = 8, color = "grey55", face = 'italic'), 
    axis.title.y = element_text(size = 8, color = "darkslategrey"),
    axis.title.x = element_text(size = 8, color = "darkslategrey"),
    axis.text.y = element_text(size = 8, color = "darkslategrey"),
    axis.text.x = element_text(size = 8, color = "darkslategrey"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )

cust_theme2 <- theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position.inside = c(0.9,0.9),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption = element_text(size = 8, color = "grey55", face = 'italic'), 
    axis.title.y = element_text(size = 8, color = "darkslategrey"),
    axis.title.x = element_text(size = 8, color = "darkslategrey"),
    axis.text.y = element_text(size = 8, color = "darkslategrey"),
    axis.text.x = element_text(size = 8, color = "darkslategrey"),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )


# Plot
monthly_indicators %>%
  #filter(mon %in% (1:5*12)) %>% 
  filter(mon %in% seq(3, mons, by = 3)) %>% 
  mutate(date = make_date(
    yr+2022, 
    if_else(mon > 12, mon-((yr-1)*12), mon), 
    1)
  ) %>% 
  select(yr, mon, date, gearing, cash_int_cover, int_fin_ratio, current_ratio, ret_on_asset) %>% 
  pivot_longer(
    cols = c(gearing, cash_int_cover, int_fin_ratio, current_ratio, ret_on_asset),
    names_to = 'indicator', 
    values_to = 'value'
  ) %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(vars(indicator), scales = "free_y", ncol = 3) +
  labs(x = '',
       y = '',
       title = 'Titles',
       subtitle = 'Subtitle',
       caption = 'Caption') +
  #scale_y_continuous(breaks = seq(1,2,0.5)) +
  scale_x_date(date_breaks = '1 year',
               date_labels = '%Y') + 
  cust_theme2



# Financial statement tables
inc1 <- bind_rows(
  slr %>% 
    filter(mon %in% c(12, 24, 36, 48, 60)) %>% 
    left_join(ref, by = join_by(account_grp == lookup1)) %>% 
    group_by(mon, ref1, ref2) %>% 
    summarise(amount = sum(ytd)) %>% 
    ungroup()
  , 
  chart %>% 
    left_join(ref, by = join_by(account_grp == lookup1)) %>% 
    group_by(ref1, ref2) %>% 
    summarise(amount = sum(cw_23)) %>% 
    ungroup() %>% 
    mutate(mon = 0) %>% 
    select(mon, ref1, ref2, amount)
  ) %>% 
  filter(mon %in% c(0, 12, 24, 36, 48, 60)) %>% 
  arrange(mon) %>% 
  mutate(
    mon = (mon / 12 ) + initial_fcast_yr - 1,
    amount = round(amount, 0)
    ) %>% 
  pivot_wider(names_from = mon, values_from = amount) %>% 
  arrange(ref2)  

# Total income
tot1 <- data.table::transpose(data.frame(c(ref1 = "Total revenue from transactions", ref2 = 8, colSums(inc1[inc1$ref2 %in% 2:7, 3:8], na.rm = TRUE))))
names(tot1) <- names(inc1)
tot1[,2:8] <- as.numeric(tot1[,2:8])

# Total expense
tot2 <- data.table::transpose(data.frame(c(ref1 = "Total expenses from transactions", ref2 = 17, colSums(inc1[inc1$ref2 %in% 11:16, 3:8], na.rm = TRUE))))
names(tot2) <- names(inc1)
tot2[,2:8] <- as.numeric(tot2[,2:8])

# Total result txn
tot3 <- data.table::transpose(data.frame(c(ref1 = "Net result from transactions", ref2 = 18, colSums(inc1[inc1$ref2 %in% 1:16, 3:8], na.rm = TRUE))))
names(tot3) <- names(inc1)
tot3[,2:8] <- as.numeric(tot3[,2:8])

inc <- bind_rows(inc1, tot1, tot2, tot3) %>% 
  arrange(ref2) %>% 
  mutate(across(where(is.numeric), abs)) %>%
  mutate(across(where(is.numeric), label_comma())) %>% 
  mutate(across(everything(), \(x) replace_na(x, "- "))) %>% 
  #mutate(across(3:8, \(x) replace(x, "0", "- "))) %>% 
  add_row(ref1 = "Revenue from transactions", ref2 = "",`2023` = "",`2024` = "",`2025` = "",`2026` = "",`2027` = "",`2028` = "", .after = 0) %>% 
  add_row(ref1 = ".", ref2 = "",`2023` = "",`2024` = "",`2025` = "",`2026` = "",`2027` = "",`2028` = "", .after = 8) %>% 
  add_row(ref1 = "Expenses from transactions", ref2 = "",`2023` = "",`2024` = "",`2025` = "",`2026` = "",`2027` = "",`2028` = "", .after = 9) %>% 
  add_row(ref1 = ".", ref2 = "",`2023` = "",`2024` = "",`2025` = "",`2026` = "",`2027` = "",`2028` = "", .after = 17)

# Replace zeros
#inc <- replace(inc[,3:8], "0", "-")

inc %>% 
  select(-ref2) %>% 
  rename(" " = ref1) %>% 
  kbl(
    align = c("l",rep("r",6)),
    caption = "Comprehensive operating statement"
  ) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  column_spec(1,  width = "24em") %>%
  column_spec(2:7,  width = "8em") %>% 
  row_spec(1, bold = TRUE) %>%
  row_spec(7, , extra_css = "border-bottom: 1px solid") %>%
  row_spec(8, bold = TRUE) %>% #, extra_css = "padding: 10px") %>% 
  row_spec(9, color = "white") %>%
  row_spec(10, bold = TRUE) %>%
  row_spec(16, extra_css = "border-bottom: 1px solid") %>% 
  row_spec(17, bold = TRUE) %>% 
  row_spec(18, extra_css = "border-bottom: 1px solid") %>% #color = "white", 
  row_spec(19, bold = TRUE, extra_css = "border-bottom: 1px solid")
