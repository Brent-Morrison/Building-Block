library(dplyr)
library(tidyr)
library(slider)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(scales)

# Sensitivity
# - interest rates
# - customer usage shortfall (kl_hhold_pa in price path vs actual) 
# - inflation wedge (that in rrr and that in actual)
# - flex percentage of income from fixed vs variable tariffs

# Scenario
# - Capex & resultant increase in baseline opex
# - Usage delta impact on pumping costs and treatment costs 

# Output / assessment
# - As below
# - Fund From Operations / Net debt (note FFO includes interest expense)
# - Net debt / RAB
# - Average residential pricing

# Uniform monte carlo example
value <- 3
value + runif(20, min=-value/10, max=value/10)

src <- "local"   # "local" / "remote"

# Data sources
if (src == "local") {
  dat_src   <- "./data/price_subm_2023.csv"
  ref_src   <- "./data/reference.csv"
  funs_src  <- "funs.R"
  chart_src   <- "./data/chart.csv"
  txn_src     <- "./data/txn_type.csv"
} else {
  dat_src   <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/price_subm_2023.csv"
  ref_src   <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/reference.csv"
  funs_src  <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/funs.R"
  chart_src   <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/chart.csv"
  txn_src     <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/txn_type.csv"
}

dat      <- read.csv(dat_src, fileEncoding="UTF-8-BOM")
ref      <- read.csv(ref_src, fileEncoding="UTF-8-BOM")
source(funs_src)
chart    <- read.csv(chart_src, fileEncoding="UTF-8-BOM")
txn_type <- read.csv(txn_src, fileEncoding="UTF-8-BOM")
rownames(txn_type) <- txn_type$txn_code

# Capex and opex scenarios
ps <- c("ps23","ps28","ps33","ps38")

cx_delta <- data.frame(
  scnr1 = c(0,1061,1205,398), 
  scnr2 = c(0,688,690,831),
  scnr3 = c(0,980,533,1031),
  scnr4 = c(0,1159,1110,551),
  row.names = ps
  )

ox_delta <- data.frame(
  scnr1 = c(0,0,34,140), 
  scnr2 = c(0,0,28,55),
  scnr3 = c(0,0,19,100),
  scnr4 = c(0,0,34,148),
  row.names = ps
)


# Function
f <- function(dat, chart, cx_delta, ox_delta, txn_type, q_grow, debt_sens, oxcx_scenario) { 
  
  # Args:
  #   dat            - a data frame containing the required regulatory data (TO DO - document this) 
  #   chart          - a data frame representing the accounting chart of accounts
  #   cx_delta       - a data frame containing multiple scenarios of 5 year capex spend for price periods 2 to n
  #   ox_delta       - a data frame containing multiple scenarios of incremental 5 year opex spend for price periods 2 to n
  #   txn_type       - a data frame specifying account posting for each transaction type
  #   q_grow         - the annual growth rate for all quantities
  #   debt_sens      - real cost of debt sensitivity, these values adjust the real c.o.d. in order to perform sensitivity analysis
  #   oxcx_scenario  - the opex & capex scenario to be selected from ox_delta and cx_delta
  #
  # Returns:
  #   list containing the following element, 
  #   txns           - a matrix containing all accounting transactions
  #   rab            - a data frame being the opening to closing reconciliation of the movement in the regulatory asset base
  #   price_delta    - a list of each yearly real price delta
  #   prices         - a matrix of all prices for each year
  #   rev_req        - a data frame of the components to the revenue requirement for each year
  
  # Parameters --------------------------------------------------------------------------------------
  ent_parm           <- "CW"      # select data for specific entity from the "dat" data frame
  initial_fcast_yr   <- 2024      # the first forecast year
  price_delta_yr     <- 0         # for function npv_optim_func, an integer between 0 and 5 representing the year in which
                                  # price delta 2 comes into effect, a value of zero returns an equal price delta for each year 
  single             <- T         # for function npv_optim_func, logical, if true the only price delta (that of pdpar1) occurs
  pd_max_per         <- 1         # price delta max period, if parameter single is F, specify which price delta should be higher
  cost_of_debt_nmnl  <- 0.0456    # nominal cost of debt
  fcast_infltn       <- 0.03
  gearing            <- 0.6
  cost_of_debt_real  <- (1 + cost_of_debt_nmnl) / (1 + fcast_infltn) - 1
  roe                <- 0.041
  rrr                <- round((roe * (1 - gearing) + cost_of_debt_real * gearing), 4)
  cash_buffer        <- 10000
  
  
  # Data --------------------------------------------------------------------------------------------
  capex <- dat %>%
    mutate(net_capex = case_when(
      balance_type %in% c("cust_cont","gov_cont") ~ -amount, 
      TRUE ~ amount)
    ) %>%
    filter(entity == ent_parm, balance_type %in% c("cust_cont","gov_cont","gross_capex")) %>%
    select(-c(entity, balance_type, service, asset_category, cost_driver, tax_life, notes, amount)) %>%
    pivot_wider(names_from = year, values_from = net_capex, values_fn = sum, values_fill = 0)

  # Append out year capex per "cx_delta" data frame
  capex[ , as.character(2029:2033)] <- capex[ , as.character(2029:2033)] / sum(capex[ , as.character(2029:2033)]) * cx_delta["ps28", oxcx_scenario]
  capex[ , as.character(2034:2038)] <- capex[ , as.character(2034:2038)] / sum(capex[ , as.character(2034:2038)]) * cx_delta["ps33", oxcx_scenario]
  capex[ , as.character(2039:2043)] <- capex[ , as.character(2039:2043)] / sum(capex[ , as.character(2039:2043)]) * cx_delta["ps38", oxcx_scenario]
  
  c <- as.matrix(capex[, 3:(ncol(capex))])
  rab_n_yrs <- ncol(c)
  
  yr_int <- as.integer(colnames(capex)[-c(1:3)])
  yr_int
  
  year_operational <- suppressWarnings(as.integer(sub(".*-", "", capex$year_operational))+2000)
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
  dpn <- dpn_open[1:rab_n_yrs] + dpn_cpx[1:rab_n_yrs]
  
  
  
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
  
  opex[opex$year == 2029:2033, "amount"] <- opex[opex$year == 2029:2033, "amount"] + opex[opex$year == 2029:2033, "amount"] / sum(opex[opex$year == 2029:2033, "amount"]) * ox_delta["ps28", oxcx_scenario]
  opex[opex$year == 2034:2038, "amount"] <- opex[opex$year == 2034:2038, "amount"] + opex[opex$year == 2034:2038, "amount"] / sum(opex[opex$year == 2034:2038, "amount"]) * ox_delta["ps33", oxcx_scenario]
  opex[opex$year == 2039:2043, "amount"] <- opex[opex$year == 2039:2043, "amount"] + opex[opex$year == 2039:2043, "amount"] / sum(opex[opex$year == 2039:2043, "amount"]) * ox_delta["ps38", oxcx_scenario]

  
  # RAB schedule ------------------------------------------------------------------------------------
  # Capex
  cx <- colSums(c)[1:rab_n_yrs]
  unlist(colSums(c)[1:rab_n_yrs], use.names = FALSE)
  
  # Customer contributions
  cc.t <- rep(0, rab_n_yrs)
  y <- seq(initial_fcast_yr, initial_fcast_yr+(rab_n_yrs-1), length.out = rab_n_yrs)
  if(sum(dat[dat$entity == ent_parm & dat$balance_type == "cust_cont", "amount"]) != 0) {
    ccdf <- aggregate(amount ~ year, data = dat[dat$entity == ent_parm & dat$balance_type == "cust_cont", ], FUN = sum)
    cc.t[which(ccdf$year == y)] <- ccdf$amount[1:rab_n_yrs]
    cc <- cc.t
  } else {
    cc <- cc.t
  }
  cc
  suppressWarnings(rm(cc.t, ccdf, y))
  
  # Government contributions
  gc.t <- rep(0, rab_n_yrs)
  y <- seq(initial_fcast_yr, initial_fcast_yr+(rab_n_yrs-1), length.out = rab_n_yrs)
  if(sum(dat[dat$entity == ent_parm & dat$balance_type == "gov_cont", "amount"]) != 0) {
    gcdf <- aggregate(amount ~ year, data = dat[dat$entity == ent_parm & dat$balance_type == "gov_cont", ], FUN = sum)
    gc.t[which(gcdf$year == y)] <- gcdf$amount[1:rab_n_yrs]
    gc <- gc.t
  } else {
    gc <- gc.t
  }
  suppressWarnings(rm(gc.t, gcdf, y))
  
  # Disposals
  dp.t <- rep(0, rab_n_yrs)
  y <- seq(initial_fcast_yr, initial_fcast_yr+(rab_n_yrs-1), length.out = rab_n_yrs)
  if(sum(dat[dat$entity == ent_parm & dat$balance_type == "disp_proceeds", "amount"]) != 0) {
    dpdf <- aggregate(amount ~ year, data = dat[dat$entity == ent_parm & dat$balance_type == "disp_proceeds", ], FUN = sum)
    dp.t[which(dpdf$year == y)] <- dpdf$amount[1:rab_n_yrs]
    dp <- dp.t
  } else {
    dp <- dp.t
  }
  suppressWarnings(rm(dp.t, dpdf, y))
  
  
  open_rab_val <- bv
  exist_rab_detail <- matrix(rep(0, 8*rab_n_yrs), ncol=rab_n_yrs)
  rownames(exist_rab_detail) <- c("open","capex","cust_cont","gov_cont","reg_depn","disp","close","average")
  colnames(exist_rab_detail) <- c(1:rab_n_yrs)
  exist_rab_detail["open", 1] <- open_rab_val
  exist_rab_detail["capex", ] <- cx + cc + gc
  exist_rab_detail["cust_cont", ] <- -cc 
  exist_rab_detail["gov_cont", ] <- -gc 
  exist_rab_detail["reg_depn", ] <- -dpn[1:rab_n_yrs]
  exist_rab_detail["disp", ] <- -dp 
  exist_rab_detail["close", 1] <- sum(exist_rab_detail[1:6, 1])
  for(i in 2:rab_n_yrs) {
    exist_rab_detail["open", i]  <- exist_rab_detail["close", i-1]
    exist_rab_detail["close", i] <- sum(exist_rab_detail[1:6, i])
  }
  for(i in 1:rab_n_yrs) {
    mment <- exist_rab_detail[2:6, i]
    exist_rab_detail["average", i]  <- exist_rab_detail["open", i] + sum(mment[mment != 0])/2
  }
  colnames(exist_rab_detail) <- colnames(capex[,3:ncol(capex)])
  
  
  # Return on assets --------------------------------------------------------------------------------
  roa <- rrr * exist_rab_detail["average", ]
  
  
  
  # Revenue requirement -----------------------------------------------------------------------------
  rev_req <- roa[1:rab_n_yrs] + opex$amount[1:rab_n_yrs] + dpn[1:rab_n_yrs]
  rev_req_df <- data.frame(
    roa = roa[1:rab_n_yrs], 
    opx = opex$amount[1:rab_n_yrs],
    dpn = dpn[1:rab_n_yrs],
    row.names = seq(initial_fcast_yr, initial_fcast_yr+(rab_n_yrs-1), length.out = rab_n_yrs)
  )
  
  
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
  # TO DO - use this to flex income, ref. line 288.  kL up or down on wet, dry basis
  kl_hhold_pa <- q["Water.Residential.Variable", 1][[1]] / q["Water.Residential.Fixed", ][[1]]
  
  # Convert p0 quantities to p1 - p5 with fixed growth rate
  q <- q %*% exp( cumsum( log( 1 + rep(q_grow, rab_n_yrs) ) ) ) 
  
  # Prices
  pq.t1 <- pq %>% filter(entity == ent_parm, year == 2023, balance_type == 'Price')
  p0 <- as.matrix(pq.t1[, "amount"])
  rownames(p0) <- paste(pq.t1[, "service"], pq.t1[, "asset_category"], pq.t1[, "cost_driver"], sep = ".")
  
  
  
  # Perform optimisation ----------------------------------------------------------------------------
  optim_result_loop <- vector(mode = "list", length = 4)
  counter <- 0
  for (i in c(1,6,11,16)) {  
    counter <- counter + 1
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
      rev_req = rev_req[i:(i+4)],
      p0      = p0,
      q       = q[,i:(i+4)]
      
    )
    optim_result_loop[[counter]] <- optim_result
  }
  #optim_result
  
  optim_result <- vector(mode = "list", length = 2)
  counter <- 0
  for (i in c(1,6,11,16)) {
    counter <- counter + 1
    res <- npv_optim_func(theta=optim_result_loop[[counter]]$par, pdyr=price_delta_yr, single=single, rev_req=rev_req[i:(i+4)], p0=p0, q=q[,i:(i+4)], rtn="data")
    optim_result[[counter]] <- res
  }
  
  price_delta <- unlist(lapply(optim_result, function(x) x$price_delta))
  prices <- do.call(cbind, lapply(optim_result, function(x) x$prices))
  
  rev <- prices * q
  tot_rev_real <- colSums(prices * q) / 1e6  # TO DO - use this to flex income, ref. line 288 (object q).  kL up or down on wet, dry basis.  Variability based on statistical model.
  
  
  # Check results
  c1 <- sum(rev_req / (1 + rrr) ^ (1:length(rev_req)))             # NPV of revenue requirement
  c2 <- sum(tot_rev_real / (1 + rrr) ^ (1:length(tot_rev_real)))   # NPV of revenue
  
  
  # Print outcome
  cat(
    #paste0("Price delta of ", round(price_delta[price_delta != 0] * 100, 2), " percent in year ", price_delta_yr),
    paste0(c("ps23","ps28","ps33","ps38"), " revenue requirement of ", round(tapply(rev_req, rep(1:(length(rev_req)/5), each=5), sum), 2)),
    paste0("NPV of revenue requirement less NPV of revenue (s/be nil) ", round(c1 - c2, 3)),
    sep = "\n"
  )
  
  
  
  # -------------------------------------------------------------------------------------------------
  # Matrix accounting set up
  # -------------------------------------------------------------------------------------------------
  
  # Parameters 
  mons          <- rab_n_yrs * 12  # months to forecast
  open_bals_col <- "cw_23"         # column in the 'chart.csv' file representing the entities data
  infltn_factor <- exp(cumsum( log(1 + rep(fcast_infltn, 5)) ))
  month_end     <- seq(as.Date("2024-01-31") + 1, by = "month", length.out = mons) - 1
  days          <- as.numeric(format(month_end, "%d"))
  accrued_days  <- 60
  debtors_days  <- 45
  crdtr_days_ox <- 30
  crdtr_days_cx <- 45
  
  
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
  gift <- round(rep(cc / 12, each = 12), 3) * 1000
  
  
  # Expenses 
  # Percentage of opex relating to employee expenses (from published accounts)
  # This required to disaggregate baseline opex from pricing submission
  perc_opex_emp <- chart[chart$account_no == 2100, open_bals_col] / sum(chart[chart$account_no %in% c(2000,2100), open_bals_col])
  
  exp1 <- unlist(opex[opex$year %in% initial_fcast_yr:(initial_fcast_yr+rab_n_yrs-1), "amount"], use.names = FALSE) * 1000 * infltn_factor * (1-perc_opex_emp)
  exp1 <- round(as.vector(sapply(X = exp1, FUN = add_trend_season, s=0, a=0, p=0)), 3)
  exp2 <- unlist(opex[opex$year %in% initial_fcast_yr:(initial_fcast_yr+rab_n_yrs-1), "amount"], use.names = FALSE) * 1000 * infltn_factor * perc_opex_emp
  exp2 <- round(as.vector(sapply(X = exp2, FUN = add_trend_season, s=0, a=0, p=0)), 3)
  
  
  # Capex 
  cpx1 <- round(rep(cx / 12, each = 12), 3) * 1000
  
  
  # Depreciation 
  # - on opening balance
  stat_depn_bld <- depn_bv(
    yrs=rab_n_yrs, 
    de=chart[chart$account_no == 2215, open_bals_col], 
    gr=chart[chart$account_no == 3510, open_bals_col], 
    ad=chart[chart$account_no == 3515, open_bals_col]
  )
  
  stat_depn_lhi <- depn_bv(
    yrs=rab_n_yrs, 
    de=chart[chart$account_no == 2225, open_bals_col], 
    gr=chart[chart$account_no == 3520, open_bals_col], 
    ad=chart[chart$account_no == 3525, open_bals_col]
  )
  
  stat_depn_pae <- depn_bv(
    yrs=rab_n_yrs, 
    de=chart[chart$account_no == 2235, open_bals_col], 
    gr=chart[chart$account_no == 3530, open_bals_col], 
    ad=chart[chart$account_no == 3535, open_bals_col]
  )
  
  stat_depn_inf <- depn_bv(
    yrs=rab_n_yrs, 
    de=chart[chart$account_no == 2245, open_bals_col], 
    gr=chart[chart$account_no == 3540, open_bals_col], 
    ad=chart[chart$account_no == 3545, open_bals_col]
  )
  
  stat_depn_sca <- depn_bv(
    yrs=rab_n_yrs, 
    de=chart[chart$account_no == 2265, open_bals_col], 
    gr=chart[chart$account_no == 3560, open_bals_col], 
    ad=chart[chart$account_no == 3565, open_bals_col]
  )
  
  stat_depn_int <- depn_bv(
    yrs=rab_n_yrs, 
    de=chart[chart$account_no == 2205, open_bals_col], 
    gr=chart[chart$account_no == 3600, open_bals_col], 
    ad=chart[chart$account_no == 3605, open_bals_col]
  )
  
  
  
  # - on capex (balances moved from WIP)
  dpn1 <- rep(50, mons)                      # TO DO - create depn schedule re opening balances and capex, assume transfer from WIP to asset register
  dpn_mtrix <- t(mapply(FUN = depn_fun, split(c, row(c)), yr_op = yr_op, life = life))
  dpn_cpx <- colSums(dpn_mtrix)
  dpn_cpx <- rep(dpn_cpx / 12, each = 12)
  
  
  # -------------------------------------------------------------------------------------------------
  # Transaction loop
  # -------------------------------------------------------------------------------------------------
  
  # Cost debt variance
  cost_of_debt <- cost_of_debt_nmnl + if (is.null(debt_sens)) 0 else debt_sens  #+ if (is.null(x)) 0 else rnorm(1, 0, x)    
  
  for (i in 1:length(mon)) {
    
    # Opening balances & debtors ageing post loop / month 1 -----------------------------------------
    if (i > 1) {
      
      # Opening balances
      mat[, "open", i] <- mat[, "clos", i-1]
      
    }
    
    
    # Post income (DR accrued income / CR income) ---------------------------------------------------
    t <- "aidb"
    mat[drcr(t, txn_type), t, i] <- c(incm[i], -incm[i])
    
    
    # Transfer to debtors to specify desired closing balance for accrued income days parameter -------------
    trail <- 3
    t <- "incm"
    tfer <- trgt_days(mat, days, i, accrued_days, trail, bal_acnt="3050", pl_acnt="1000", txn="aidb")
    mat[drcr(t, txn_type), t, i] <- c(-tfer, tfer)
    
    
    # Cash receipt to specify desired closing balance for debtors days parameter --------------------
    trail <- 3
    t <- "cshd"
    rcpt <- trgt_days(mat, days, i, accrued_days, trail, bal_acnt="3100", pl_acnt="3050", txn="incm")
    mat[drcr(t, txn_type), t, i] <- c(-rcpt, rcpt)
    
    
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
    t <- "exp1"
    mat[drcr(t, txn_type), t, i] <- c(exp1[i], -exp1[i])
      
    t <- "exp2"
    mat[drcr(t, txn_type), t, i] <- c(exp2[i], -exp2[i])
    
    
    # Cash payment re trade creditors ---------------------------------------------------------------
    trail <- 3
    rcpt <- trgt_days(mat, days, i, d=crdtr_days_ox, trail=3, bal_acnt="4000", pl_acnt="2000", txn="exp1")
    t <- "crd1"
    mat[drcr(t, txn_type), t, i] <- c(rcpt, -rcpt)
    
    
    # Capex -----------------------------------------------------------------------------------------
    t <- "cpx1"
    mat[drcr(t, txn_type), t, i] <- c(cpx1[i], -cpx1[i])
    
    
    # Cash payment re capex -------------------------------------------------------------------------
    trail <- 3
    rcpt <- trgt_days(mat, days, i, d=crdtr_days_cx, trail=3, bal_acnt="4010", pl_acnt="3645", txn="cpx1")
    t <- "wipc"
    mat[drcr(t, txn_type), t, i] <- c(rcpt, -rcpt)
    
    
    # Interest (accrue) -----------------------------------------------------------------------------
    int <- round(sum(mat[c("4100","4500"), "open", i]) * cost_of_debt / 12, 3)
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
    cash_bal <- sum(mat["3000",-ncol(mat[,,i]), i])  # cash balance after all transactions
    borrow_amt <- -cash_bal + cash_buffer
    if (cash_bal < 0) {
      t <- "borr"
      mat[drcr(t, txn_type), t, i] <- c(borrow_amt,-borrow_amt)
    }
    #cat(c(i, ": Cash balance - ", cash_bal, "\n"))
    #cat(c(i, ": Borrowing requirement - ", borrow_amt, "\n"))
    
    # Update closing balance
    mat[, "clos", i] <- rowSums(mat[,-ncol(mat[,,i]), i])
    
  }
  return(list(txns = mat, rab = exist_rab_detail, price_delta = price_delta, prices = prices, rev_req = rev_req_df))
}

res_single <- list(f(dat=dat, chart=chart, cx_delta=cx_delta, ox_delta=ox_delta, txn_type=txn_type, q_grow=0.019, debt_sens=NULL, oxcx_scenario="scnr4"))
res_mcs <- replicate(3, f(dat=dat, chart=chart, cx_delta=cx_delta, ox_delta=ox_delta, txn_type=txn_type, q_grow=0.019, debt_sens=NULL, oxcx_scenario="scnr4"), simplify = FALSE)
args <- expand.grid(q_grow = 0.019, debt_sens = c(-0.01,0,0.01), oxcx_scenario = c("scnr1","scnr4"))
res_scenario <- mapply(
  FUN=f, 
  dat=list(dat), 
  chart=list(chart), 
  cx_delta=list(cx_delta), 
  ox_delta=list(ox_delta), 
  txn_type=list(txn_type), 
  q_grow=args$q_grow, #list(0.019), 
  debt_sens=args$debt_sens, #list(-0.01,0,0.01), 
  oxcx_scenario=args$oxcx_scenario, #list("scnr1","scnr4"),
  SIMPLIFY = FALSE
  )


# Diagnostics
#z <- data.frame(mat[ , , 5])
#m1 <- mat[c("1000","2000","3000","3050","3100","3645","4000","4010","4020","4100"),c("open","aidb","incm","cshd","borr","exp1","crd1","cpx1","wipc","intp","borr","clos"), 24:26]
#m1


# Check balances
#round(colSums(mat[,,6]), 3)




# -------------------------------------------------------------------------------------------------
# Accounting data (3d matrix to 2d data frame)
# -------------------------------------------------------------------------------------------------

# Check transactions sum to nil / extract profit
#sum(mat[, !colnames(mat) %in% c("open", "clos"), 1:60])
#sum(mat[as.integer(row.names(mat)) < 3000, !colnames(mat) %in% c("open", "clos"), 49:60])

res <- res_scenario[[1]]
slr <- slr_fun(res, chart)

#slr2 <- lapply(res_scenario, FUN = slr_fun, chart)


# Check SLR
slr_check1 <- slr %>%
  filter(txn != "open") %>%
  group_by(mon) %>%
  summarise(total = round(sum(mtd), 3))

#write.csv(slr, file = "slr.csv")




# -------------------------------------------------------------------------------------------------
# Create trial balance 
# -------------------------------------------------------------------------------------------------

tb1 <- slr %>%
  mutate(vals = if_else(act < 3000, ytd, ltd)) %>%
  select(mon, act, account_desc, vals) %>%
  pivot_wider(names_from = mon, values_from = vals, values_fn = sum, values_fill = 0)

re <- slr %>%
  filter(mon %in% (1:(max(as.numeric(mon))/12)*12), statement_type == 1) %>%
  #filter(mon %in% c(12,24,36,48,60), statement_type == 1) %>%
  group_by(mon) %>%
  summarise(profit = sum(ytd))
re

# Add retained earnings
tbmat <- as.matrix(tb1[, 3:ncol(tb1)])
tbmat <- rbind(tbmat, rep(c(0, cumsum(re$profit)), each = 12)[1:(max(as.numeric(slr$mon)))])
round(colSums(tbmat), 3)

tb <- data.frame(tbmat)
names(tb) <- 1:(max(as.numeric(slr$mon)))
tb <- tb %>%
  mutate(
    act = c(tb1$act, 5201),
    account_desc = c(tb1$account_desc, "Retained earnings - profit")
  ) %>%
  relocate(c("act", "account_desc"), .before = '1')

rm(list=c("tb1","tbmat","re"))
round(colSums(tb[, 3:ncol(tb)], 3))

# Pretty number
tb[ ,3:ncol(tb)] <- apply(tb[ ,3:ncol(tb)], 2, acc_num)




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
    ret_on_asset     = -net_income_ttm / total_assets_ttm
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
  filter(mon %in% seq(3, max(slr$mon), by = 3)) %>% 
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



# Financial statement tables (INCOME STATEMENT)
inc1 <- bind_rows(
    slr %>% # modelled / forward looking data
      filter(
        statement_type == 1,
        mon %in% c(12, 24, 36, 48, 60)
        ) %>% 
      left_join(ref[ref$ref_type == "account_grp",], by = join_by(account_grp == lookup1)) %>% 
      group_by(mon, ref1, ref2) %>% 
      summarise(amount = sum(ytd)) %>% 
      ungroup()
    , 
    chart %>% # opening balances
      filter(statement_type == 1) %>% 
      left_join(ref[ref$ref_type == "account_grp",], by = join_by(account_grp == lookup1)) %>% 
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
  arrange(ref2) %>% 
  filter(!is.na(ref1))
  
# Total income
tot1 <- data.table::transpose(data.frame(c(ref1 = "Total revenue from transactions", ref2 = 8, colSums(inc1[inc1$ref2 %in% 2:7, 3:8], na.rm = TRUE))))
names(tot1) <- names(inc1)
tot1[,2:8] <- as.numeric(tot1[,2:8])

# Total expense
tot2 <- data.table::transpose(data.frame(c(ref1 = "Total expenses from transactions", ref2 = 17, colSums(inc1[inc1$ref2 %in% 11:16, 3:8], na.rm = TRUE))))
names(tot2) <- names(inc1)
tot2[,2:8] <- as.numeric(tot2[,2:8])

# Net result from transactions
tot3 <- data.table::transpose(data.frame(c(ref1 = "Net result from transactions", ref2 = 18, colSums(inc1[inc1$ref2 %in% 2:17, 3:8], na.rm = TRUE))))
names(tot3) <- names(inc1)
tot3[,2:8] <- as.numeric(tot3[,2:8])

# Net result
tot4 <- data.table::transpose(data.frame(c(ref1 = "Net result", ref2 = 20, colSums(inc1[inc1$ref2 %in% 2:19, 3:8], na.rm = TRUE))))
names(tot4) <- names(inc1)
tot4[,2:8] <- as.numeric(tot4[,2:8])

inc2 <- bind_rows(inc1, tot1, tot2, tot3, tot4) %>% 
  arrange(ref2) 

# Assign correct sign
inc2[, 3:8] <- as.matrix(inc2[, 3:8]) * matrix(rep(c(rep(-1,7), rep(1,7), -1, -1, -1), 6), ncol = 6)   # , -1, rep(1,3)

# Format as character
nums <- as.matrix(inc2[, 3:8])
nums[is.na(nums)] <- 0
nums_new <- t(prettyNum(abs(nums), big.mark=','))
#ifelse(nums >= 0, nums_new, paste0('(', nums_new, ')'))
nums_new <- ifelse(nums >= 0, paste0(nums_new, ' '), paste0('(', nums_new, ')'))
nums_new <- ifelse(nums == 0, "- ", nums_new) 
nums_new
inc2[, 3:8] <- nums_new
inc2
  
# Add title rows
inc3 <- inc2 %>% 
  mutate(across(where(is.numeric), abs)) %>%
  mutate(across(where(is.numeric), label_comma())) %>% 
  mutate(across(everything(), \(x) replace_na(x, "- "))) %>% 
  #mutate(across(3:8, \(x) replace(x, "0", "- "))) %>% 
  add_row(ref1 = "Revenue from transactions", ref2 = "",`2023` = "",`2024` = "",`2025` = "",`2026` = "",`2027` = "",`2028` = "", .after = 0) %>% 
  add_row(ref1 = "x", ref2 = "",`2023` = "",`2024` = "",`2025` = "",`2026` = "",`2027` = "",`2028` = "", .after = 8) %>% 
  add_row(ref1 = "Expenses from transactions", ref2 = "",`2023` = "",`2024` = "",`2025` = "",`2026` = "",`2027` = "",`2028` = "", .after = 9) %>% 
  add_row(ref1 = "x", ref2 = "",`2023` = "",`2024` = "",`2025` = "",`2026` = "",`2027` = "",`2028` = "", .after = 17)
  
inc3 %>% 
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
  row_spec(7, extra_css = "border-bottom: 1px solid") %>%
  row_spec(8, bold = TRUE) %>% #, extra_css = "padding: 10px") %>% 
  row_spec(9, color = "white") %>%
  row_spec(10, bold = TRUE) %>%
  row_spec(16, extra_css = "border-bottom: 1px solid") %>% 
  row_spec(17, bold = TRUE) %>% 
  row_spec(18, color = "white", extra_css = "border-bottom: 1px solid") %>% 
  row_spec(c(19,21), bold = TRUE)



# Financial statement tables (BALANCE SHEET)
bal1 <- bind_rows(
  slr %>% # modelled / forward looking data
    filter(
      statement_type == 2,
      mon %in% c(12, 24, 36, 48, 60)
    ) %>% 
    left_join(ref[ref$ref_type == "account_type",], by = join_by(account_type == lookup1)) %>% 
    group_by(mon, ref1, ref2) %>% 
    summarise(amount = sum(ytd)) %>% 
    ungroup()
  , 
  chart %>% # opening balances
    filter(statement_type == 2) %>% 
    left_join(ref[ref$ref_type == "account_type",], by = join_by(account_type == lookup1)) %>% 
    group_by(ref1, ref2) %>% 
    summarise(amount = sum(cw_23)) %>% 
    ungroup() %>% 
    mutate(mon = 0) %>% 
    select(mon, ref1, ref2, amount)
  ,
  slr %>% # modelled / forward looking data
    filter(
      statement_type == 2,
      mon %in% c(12, 24, 36, 48, 60)
    ) %>% 
    left_join(ref[ref$ref_type == "account_grp",], by = join_by(account_grp == lookup1)) %>% 
    group_by(mon, ref1, ref2) %>% 
    summarise(amount = sum(ytd)) %>% 
    ungroup()
  , 
  chart %>% # opening balances
    filter(statement_type == 2) %>% 
    left_join(ref[ref$ref_type == "account_grp",], by = join_by(account_grp == lookup1)) %>% 
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
  filter(!is.na(ref1)) %>% 
  pivot_wider(names_from = mon, values_from = amount) %>% 
  arrange(ref2)
  
bal1[ ,3:8] <- apply(bal1[ ,3:8], 2, acc_num)
bal1
