# To DO - check if the real to nominal uplift via inflation factors is being reset each price period
# Function
f <- function(
    dat=dat_df, chart=chart_df, txn_type=txn_df, ref=ref_df, cx_delta=cx_df, ox_delta=ox_df,  
    q_grow             = 0.019, 
    cost_of_debt_nmnl  = 0.0456, 
    fcast_infltn       = 0.03, 
    roe                = 0.041, 
    single_price_delta = T,
    desired_fixed      = 0.40,
    debt_sens          = 0, 
    fte                = 200,  # fte
    cost_fte           = 100,
    q_grow_fte         = 0.02,
    ni_cost_fte        = 0.02,
    kwh                = 25,   # kwh
    cost_kwh           = 22,
    q_grow_kwh         = 0.02,
    ni_cost_kwh        = 0.02,
    ml                 = 22,   # ml
    cost_ml            = 120,
    q_grow_ml          = 0.02,
    ni_cost_ml         = 0.02,
    dl                 = 60,   # dl
    cost_dl            = 1,
    q_grow_dl          = 0.02,
    ni_cost_dl         = 0.02,
    capex_ps2          = 100,
    capex_ps3          = 100,
    capex_ps4          = 100,
    opex_ps2           = 10,
    opex_ps3           = 10,
    opex_ps4           = 10,
    verbose            = F
    ) { 
  
  # Args:
  #   dat               - a data frame containing the required regulatory data (TO DO - document this) 
  #   chart             - a data frame representing the accounting chart of accounts
  #   txn_type          - a data frame specifying account posting for each transaction type
  #   q_grow            - the annual growth rate for all quantities
  #   cost_of_debt_nmnl - nominal cost of debt (0.0456)
  #   cast_infltn       - inflation (0.03)
  #   roe               - allowed return on equity (0.041)
  #   single_price_delta- for function npv_optim_func, logical, if true the only price delta (that of the first price delta `theta[2]`) is used
  #   debt_sens         - real cost of debt sensitivity, these values adjust the real c.o.d. in order to perform sensitivity analysis
  #   oxcx_scenario     - the opex & capex scenario to be selected from ox_delta and cx_delta
  #
  # Returns:
  #   list containing the following element, 
  #   txns           - a matrix containing all accounting transactions
  #   rab            - a data frame being the opening to closing reconciliation of the movement in the regulatory asset base
  #   price_delta    - a list of each yearly real price delta
  #   prices         - a matrix of all prices for each year
  #   rev_req        - a data frame of the components to the revenue requirement for each year
  #   tariff_rev     - a matrix tariff revenue for each year
  #
  # Documentation TO DO
  #   use GoldSim website as a guide to explain model functionality
  #   - https://www.goldsim.com/Web/Applications/ExampleApplications/BusinessExamples/CityofMelbournesPopulationForecasts/PopulationModelDescription/
  #   - https://www.goldsim.com/Web/Products/Modules/Financial/Overview/
  #   - https://www.goldsim.com/Web/Products/GoldSim/Comparison/Spreadsheets/
  
  # Parameters --------------------------------------------------------------------------------------
  ent_parm           <- "CW"      # select data for specific entity from the "dat" data frame
  initial_fcast_yr   <- 2024      # the first forecast year (first financial year of the price submission)
  price_delta_yr     <- 1         # for function npv_optim_func, parameter 'pdyr', an integer between 0 and 5 representing the year in which
  # price delta 2 comes into effect, a value of zero returns an equal price delta for each year (refer to documentation for function 'npv_optim_func')
  pd_max_per         <- 1         # price delta max period, if parameter single is F, specify which price delta should be higher
  gearing            <- 0.6       # gearing assumption for WACC
  cost_of_debt_real  <- (1 + cost_of_debt_nmnl) / (1 + fcast_infltn) - 1
  rrr                <- round((roe * (1 - gearing) + cost_of_debt_real * gearing), 4)
  cash_buffer        <- 10000     # cash buffer for loan drawdown
  
  
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
  capex[ , as.character(2029:2033)] <- capex[ , as.character(2029:2033)] / sum(capex[ , as.character(2029:2033)]) * capex_ps2 * 5 #cx_delta["ps28", oxcx_scenario]
  capex[ , as.character(2034:2038)] <- capex[ , as.character(2034:2038)] / sum(capex[ , as.character(2034:2038)]) * capex_ps3 * 5 #cx_delta["ps33", oxcx_scenario]
  capex[ , as.character(2039:2043)] <- capex[ , as.character(2039:2043)] / sum(capex[ , as.character(2039:2043)]) * capex_ps4 * 5 #cx_delta["ps38", oxcx_scenario]
  
  c <- as.matrix(capex[, 3:(ncol(capex))])
  rab_n_yrs <- ncol(c)
  
  yr_int <- as.integer(colnames(capex)[-c(1:3)])
  
  year_operational <- suppressWarnings(as.integer(sub(".*-", "", capex$year_operational))+2000)
  year_operational[is.na(year_operational)] <- yr_int[1]
  
  yr_op <- match(year_operational, yr_int)
  
  #life <- ifelse(capex$regulatory_life == 0, 1, capex$regulatory_life)
  life <- capex$regulatory_life
  
  
  
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
  # TO DO - how are these costs ("Operations & Maintenance", "Customer Service and billing") split into wages, electricity, chemicals and other
  # opex <- dat %>%
  #   filter(
  #     balance_type %in% c("Operations & Maintenance", "External bulk charges (excl. temporary purchases)", 
  #                         "Customer Service and billing", "GSL Payments", "Corporate", "Other operating expenditure",
  #                         "Environment Contribution", "Licence Fees", "Treatment"),
  #     entity == ent_parm
  #   ) %>% 
  #   group_by(year) %>% 
  #   summarise(amount = sum(amount))
  # 
  # opex[opex$year == 2029:2033, "amount"] <- opex[opex$year == 2029:2033, "amount"] + opex[opex$year == 2029:2033, "amount"] / sum(opex[opex$year == 2029:2033, "amount"]) * opex_ps2 #ox_delta["ps28", oxcx_scenario]
  # opex[opex$year == 2034:2038, "amount"] <- opex[opex$year == 2034:2038, "amount"] + opex[opex$year == 2034:2038, "amount"] / sum(opex[opex$year == 2034:2038, "amount"]) * opex_ps3 # ox_delta["ps33", oxcx_scenario]
  # opex[opex$year == 2039:2043, "amount"] <- opex[opex$year == 2039:2043, "amount"] + opex[opex$year == 2039:2043, "amount"] / sum(opex[opex$year == 2039:2043, "amount"]) * opex_ps4 # ox_delta["ps38", oxcx_scenario]
  
  
  # NEW OPEX FROM UI (retuned in thousands)
  opx_labr <- real_series(q = fte    , p = cost_fte    , q_grow = q_grow_fte)
  opx_elec <- real_series(q = kwh*1e6, p = cost_kwh/1e3, q_grow = q_grow_kwh) / 1e3
  opx_chem <- real_series(q = ml *1e3, p = cost_ml     , q_grow = q_grow_ml) / 1e3
  opx_othr <- real_series(q = dl *1e6, p = cost_dl     , q_grow = q_grow_dl) / 1e3
  
  # Extend opex series with subsequent PS periods from UI input
  opx_outy <- c(rep(0, 5), rep(opex_ps2*1e3, 5), rep(opex_ps3*1e3, 5), rep(opex_ps4*1e3, 5))
  opx_tot5 <- opx_labr[5] + opx_elec[5] + opx_chem[5] + opx_othr[5]
  
  # Apportion subsequent PS periods across opex types
  opx_labr <- opx_labr + opx_outy * opx_labr[5] / opx_tot5
  opx_elec <- opx_elec + opx_outy * opx_elec[5] / opx_tot5
  opx_chem <- opx_chem + opx_outy * opx_chem[5] / opx_tot5
  opx_othr <- opx_othr + opx_outy * opx_othr[5] / opx_tot5
  
  # Total opex (convert to millions)
  opex <- (opx_labr + opx_elec + opx_chem + opx_othr) / 1000

  
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
  
  
  
  # Revenue requirement (millions) -------------------------------------------------------------------
  rev_req <- roa[1:rab_n_yrs] + opex[1:rab_n_yrs] + dpn[1:rab_n_yrs]
  rev_req_df <- data.frame(
    roa = roa[1:rab_n_yrs], 
    opx = opex[1:rab_n_yrs],
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
  q.t1 <- pq %>% filter(entity == ent_parm, year == initial_fcast_yr-1, balance_type == 'Quantity')  # Pivot wider here
  q <- as.matrix(q.t1[, "amount"])
  rownames(q) <- paste(q.t1[, "service"], q.t1[, "asset_category"], q.t1[, "cost_driver"], sep = ".")
  
  # Avg. annual consumption (kL per household) 
  # TO DO - use this to flex income, ref. line 288.  kL up or down on wet, dry basis
  kl_pa_hhold <- q["Water.Residential.Variable", 1][[1]] / q["Water.Residential.Fixed", ][[1]]
  kl_pa_bus <- q["Water.Non-residential.Variable", 1][[1]] / q["Water.Non-residential.Fixed", ][[1]]
  
  # Convert p0 quantities to p1 - p5 with fixed growth rate
  q <- q %*% exp( cumsum( log( 1 + rep(q_grow, rab_n_yrs) ) ) ) 
  
  # Prices
  pq.t1 <- pq %>% filter(entity == ent_parm, year == initial_fcast_yr-1, balance_type == 'Price')
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
      single  = single_price_delta,
      des_f   = desired_fixed,
      rev_req = rev_req[i:(i+4)],
      p0      = p0,
      q       = q[,i:(i+4)]
      
    )
    optim_result_loop[[counter]] <- optim_result
  }
  
  
  optim_result <- vector(mode = "list", length = 2)
  counter <- 0
  for (i in c(1,6,11,16)) {
    counter <- counter + 1
    res <- npv_optim_func(
      theta      = optim_result_loop[[counter]]$par, 
      pdyr       = price_delta_yr, 
      single     = single_price_delta, 
      des_f      = desired_fixed ,
      rev_req    = rev_req[i:(i+4)], 
      p0         = p0, 
      q          = q[,i:(i+4)], 
      rtn        = "data"
      )
    optim_result[[counter]] <- res
  }
  
  price_delta <- unlist(lapply(optim_result, function(x) x$price_delta))
  prices <- do.call(cbind, lapply(optim_result, function(x) x$prices))
  tot_rev_real <- prices * q / 1e6  # TO DO - use this to flex income, ref. line 288 (object q).  kL up or down on wet, dry basis.  Variability based on statistical model.
  # TO DO - what is "Water.Service fees.Fixed"
  
  # Check results
  c1 <- sum(rev_req / (1 + rrr) ^ (1:length(rev_req)))             # NPV of revenue requirement
  c2 <- sum(tot_rev_real / (1 + rrr) ^ (1:length(tot_rev_real)))   # NPV of revenue
  
  
  # Print outcome
  if (verbose) {
    cat(
      #paste0("Price delta of ", round(price_delta[price_delta != 0] * 100, 2), " percent in year ", price_delta_yr),
      paste0(c("ps23","ps28","ps33","ps38"), " revenue requirement of ", round(tapply(rev_req, rep(1:(length(rev_req)/5), each=5), sum), 2)),
      paste0("NPV of revenue requirement less NPV of revenue (s/be nil) ", round(c1 - c2, 3)),
      sep = "\n"
    )
  }
    
  
  
  # -------------------------------------------------------------------------------------------------
  # Matrix accounting set up (balances are in thousands)
  # -------------------------------------------------------------------------------------------------
  
  # Parameters 
  mons          <- rab_n_yrs * 12  # months to forecast
  open_bals_col <- "cw_23"         # column in the 'chart.csv' file representing the entities data
  infltn_factor <- growth_fctr(esc_rate = 0.025, len = rab_n_yrs)   
  #infltn_factor <- exp(cumsum( log(1 + rep(fcast_infltn, 5)) ))
  month_end     <- seq(as.Date(paste(initial_fcast_yr-1,"07","31", sep = "-")) + 1, by = "month", length.out = mons) - 1
  days          <- as.numeric(format(month_end, "%d"))
  accrued_days  <- 60
  debtors_days  <- 45
  crdtr_days_ox <- 30
  crdtr_days_cx <- 45
  
  
  # Matrix ------------------------------------------------------------------------------------------
  mon <- 1:mons   # Number of months
  txn <- unlist(txn_type[,"txn_code"], use.names = FALSE)  # Transaction types
  act <- unlist(chart[,"account_no"], use.names = FALSE)   # GL accounts
  
  
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
  incm2 <- t( apply( tot_rev_nmnl, 1, function(x) round( as.vector( sapply(X = x, FUN = add_trend_season, s=0, a=1, p=1.5) ), 3 ) ) )
  income_map <- ref[ref$ref_type == "income_map", c("lookup2","ref2")]  
  # Map income types to GL accounts
  incm1 <- left_join(
    mutate(data.frame(incm2), income_type = rownames(incm2)), 
    income_map, 
    by = c("income_type" = "lookup2")
    ) %>% 
    group_by(ref2) %>% 
    summarise(across(where(is.numeric), sum)) %>% 
    mutate(ref2 = as.character(ref2))
  incm <- as.matrix(incm1[-1])
  rownames(incm) <- incm1$ref2
  colnames(incm) <- as.character(substr(month_end, 1, 7))
  
  gift <- round(rep(cc / 12, each = 12), 3) * 1000
  
  
  # Expenses 
  
  # Percentage of opex relating to employee expenses (from published accounts)
  # This required to disaggregate baseline opex from pricing submission
  # perc_opex_emp <- chart[chart$account_no == 2100, open_bals_col] / sum(chart[chart$account_no %in% c(2000,2100), open_bals_col])
  # 
  # exp1 <- unlist(opex[opex$year %in% initial_fcast_yr:(initial_fcast_yr+rab_n_yrs-1), "amount"], use.names = FALSE) * 1000 * infltn_factor * (1-perc_opex_emp)
  # exp1 <- round(as.vector(sapply(X = exp1, FUN = add_trend_season, s=0, a=0, p=0)), 3)
  # exp2 <- unlist(opex[opex$year %in% initial_fcast_yr:(initial_fcast_yr+rab_n_yrs-1), "amount"], use.names = FALSE) * 1000 * infltn_factor * perc_opex_emp
  # exp2 <- round(as.vector(sapply(X = exp2, FUN = add_trend_season, s=0, a=0, p=0)), 3)
  # exp3 <- unlist(opex1[opex1$year %in% initial_fcast_yr:(initial_fcast_yr+rab_n_yrs-1), "amount"], use.names = FALSE) * 1000 * ni_cost_ml
  # exp3 <- round(as.vector(sapply(X = exp3, FUN = add_trend_season, s=0, a=0, p=0)), 3)
  
  opx_labr_m <- round(as.vector(sapply(X = opx_labr * growth_fctr(ni_cost_fte, rab_n_yrs), FUN = add_trend_season, s=0, a=0, p=0)), 3)
  opx_elec_m <- round(as.vector(sapply(X = opx_elec * growth_fctr(ni_cost_kwh, rab_n_yrs), FUN = add_trend_season, s=0, a=0, p=0)), 3)
  opx_chem_m <- round(as.vector(sapply(X = opx_chem * growth_fctr(ni_cost_ml , rab_n_yrs), FUN = add_trend_season, s=0, a=0, p=0)), 3)
  opx_othr_m <- round(as.vector(sapply(X = opx_othr * growth_fctr(ni_cost_dl , rab_n_yrs), FUN = add_trend_season, s=0, a=0, p=0)), 3)
  
  
  
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
    p <- c(
      sum(incm[c("1000","1001","1002","1003","1101","1102","1103","1104","1105"),i]), 
      -incm["1000",i], -incm["1001",i], -incm["1002",i], -incm["1003",i], -incm["1101",i],
      -incm["1102",i], -incm["1103",i], -incm["1104",i], -incm["1105",i]
      )
    a <- unique(drcr(t, txn_type))
    
    if ( round(sum(p),3) == 0 & length(p) == length(a) ) {
     mat[a, t, i] <- p
    } else if (sum(p) != 0) {
     print("Income not posted, accounting entries do not balance")
    } else if (length(p) != length(a)) {
     print(paste0("Income not posted.  Posting data has length ", length(p), " and posting rule has length ", length(a)))
    }
    #if (i == 12) browser()
    
    # Transfer to debtors to specify desired closing balance for accrued income days parameter -------------
    trail <- 3
    t <- "incm"
    pl_acs <- c("1000","1001","1002","1003","1101","1102","1103","1104","1105")
    
    tfer <- trgt_days(mat, days, i, accrued_days, trail, bal_acnt="3050", pl_acnt=pl_acs, txn="aidb") 
    #if (i == 200) browser()
    #print(paste("Txn 'incm', tfer accrued income to debtors. Loop no.", i, round(-tfer,1)))
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
    t <- "exp1" # other expenses
    mat[drcr(t, txn_type), t, i] <- c(opx_othr_m[i], -opx_othr_m[i])
    
    t <- "exp2" # employee expenses
    mat[drcr(t, txn_type), t, i] <- c(opx_labr_m[i], -opx_labr_m[i])
    
    t <- "exp3" # chemicals
    mat[drcr(t, txn_type), t, i] <- c(opx_chem_m[i], -opx_chem_m[i])
    
    t <- "exp4" # electricty
    mat[drcr(t, txn_type), t, i] <- c(opx_elec_m[i], -opx_elec_m[i])   
    
    # Cash payment re trade creditors ---------------------------------------------------------------
    trail <- 3
    rcpt <- trgt_days(mat, days, i, d=crdtr_days_ox, trail=3, bal_acnt="4000", pl_acnt="2000", txn=c("exp1","exp3","exp4"))
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
      stat_depn_inf[i]+dpn_cpx[i], -stat_depn_inf[i]-dpn_cpx[i], # TO DO - assumes all capex is infrastructure
      stat_depn_pae[i], -stat_depn_pae[i],
      stat_depn_sca[i], -stat_depn_sca[i]
    )
    a <- drcr(t, txn_type)
    if (sum(p) == 0 & length(p) == length(a)) {
      mat[a, t, i] <- p
    } else if (sum(p) != 0) {
      print("Depreciation not posted, accounting entries do not balance")
    } else if (length(p) != length(a)) {
      print(paste0("Depreciation not posted.  Posting data has length ", length(p), " and posting rule has length ", length(a)))
    }
    
    
    # Determine if borrowings required --------------------------------------------------------------
    # TO DO - determine amount to borrow dynamically
    cash_bal <- sum(mat["3000",-ncol(mat[,,i]), i])  # cash balance after all transactions
    borrow_amt <- round(-cash_bal + cash_buffer, -3)
    if (cash_bal < 0) {
      t <- "borr"
      mat[drcr(t, txn_type), t, i] <- c(borrow_amt,-borrow_amt)
    }
    #cat(c(i, ": Cash balance - ", cash_bal, "\n"))
    #cat(c(i, ": Borrowing requirement - ", borrow_amt, "\n"))
    
    
    # Update closing balance
    mat[, "clos", i] <- rowSums(mat[,-ncol(mat[,,i]), i])
    
  }
  
  return(list(
    txns         = mat, 
    rab          = exist_rab_detail, 
    price_delta  = price_delta, 
    prices       = prices, 
    rev_req      = rev_req_df, 
    tariff_rev   = tot_rev_nmnl,
    call         = list(cx_delta=cx_delta, ox_delta=ox_delta, q_grow=q_grow, 
                        cost_of_debt_nmnl=cost_of_debt_nmnl, fcast_infltn=fcast_infltn, 
                        roe=roe, debt_sens=debt_sens) # , oxcx_scenario=oxcx_scenario
  ))
  
}
