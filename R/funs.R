# ---------------------------------------------------------------------------------------------------------
# Plot theme
# ---------------------------------------------------------------------------------------------------------

cstm_theme1 <- function() {
  
  theme_minimal() +
    theme(
      legend.title      = element_blank(),
      legend.position   = c(0.9,0.9),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      strip.text        = element_text(size = 12, color = "darkslategrey"),   # , family = "mono"
      plot.caption      = element_text(size = 12, color = "grey55", face = 'italic'), 
      axis.title.y      = element_text(size = 12, color = "darkslategrey"),
      axis.title.x      = element_text(size = 12, color = "darkslategrey"),
      axis.text.y       = element_text(size = 10, color = "darkslategrey"),
      axis.text.x       = element_text(size = 10, color = "darkslategrey")
    )
  
}





# ---------------------------------------------------------------------------------------------------------
# Function reading table
# ---------------------------------------------------------------------------------------------------------

xl_to_df <- function(file, sheet=NULL, table=NULL, date_range, cols_range, cols_name=NULL, data_range) {
  
  dates <- read_xlsx(path, range = paste0(sheet,"!",date_range), col_names = FALSE)
  cols <- read_xlsx(path, range = paste0(sheet,"!",cols_range), col_names = TRUE)
  data <- read_xlsx(path, range = paste0(sheet,"!",data_range), col_names = FALSE)
  
  df <- data.frame(data)
  names(df) <- t(gsub("-", "_", dates))
  names(df) <- paste0("FY", sub('.*_', '', names(df)))
  
  if (is.null(cols_name)) {
    cols_name <- "columns"
  }
  
  cols <- cols[,colSums(is.na(cols)) < nrow(cols)]
  #names(cols) <- cols_name
  names(cols) <- gsub("-", "_", names(cols))
  names(cols) <- gsub(" ", "_", names(cols))
  names(cols) <- gsub("\\(", "", names(cols))
  names(cols) <- gsub("\\)", "", names(cols))
  names(cols) <- tolower(gsub("/", "_", names(cols)))
  
  if (!is.null(sheet)) {
    df$sheet <- tolower(sheet)
  }
  
  if (!is.null(table)) {
    df$table <- tolower(table)
  }
  
  df <- bind_cols(cols, df) %>% relocate(any_of(c("sheet", "table")), .before = cols_name)
  #df <- df[rowSums(is.na(select(df, FY24:FY33))) < ncol(select(df, FY24:FY33)), ]
  #df <- df %>% mutate(across(starts_with("FY"), ~replace_na(., 0)))
  
  df
  
}






# --------------------------------------------------------------------------------------------------------------------------
# Depreciation on capex
# --------------------------------------------------------------------------------------------------------------------------

depn_fun <- function(capex, yr_op, life) {
  
  # capex   - a numeric vector representing the time series of capital expenditure to be depreciated
  # yr_op   - year operational, an integer representing the first period in which the capex is depreciated
  # life    - an integer representing the asset expected life
  # returns - vector of yearly depreciation (straight line basis)
  
  # Initialise an object (ac) to hold capex up to and post the year in which the asset is operational, and indices
  ac <- rep(0,length(capex))
  ind <- 1:length(capex)
  
  # Create vector of capex up to and post the year in which the asset is operational
  if (yr_op == 1) {
    ac <- capex
  } else {
    ac[ind <  yr_op] <- 0
    ac[ind == yr_op] <- sum(capex[ind <= yr_op])
    ac[ind >  yr_op] <- capex[ind >  yr_op]
  }
  
  # Matrix containing the amended capex times series (ac) on the diagonal
  # Near zero added to avoid divide by nil error
  cpx.m <- diag(as.vector(ac)) + diag(rep(1e-9, length(ac)))
  
  # Initial year depn
  yr1.dpn <- cpx.m / life * 0.5
  
  # Subsequent years depn - inserted both sides of diagonal
  yr2p.dpn <- cpx.m
  for (i in 1:ncol(yr2p.dpn)) {
    yr2p.dpn[i,][yr2p.dpn[i,] == 0] <- rep(diag(yr2p.dpn)[i] / life, ncol(cpx.m) - 1)
  }
  
  # Columns represents time period so remove diagonal (capex) and left thereof (prior period depn)
  yr2p.dpn[lower.tri(yr2p.dpn, diag = TRUE)] <- 0
  
  # Add year 1 depn
  dpn <- yr1.dpn + yr2p.dpn
  
  # Remove depn that exceeds expected life, add final year depn (same as initial year - 50%)
  for (i in 1:ncol(dpn)) {
    if (i + life <= ncol(dpn)) {
      dpn[i,][i + life] <- dpn[i,][i + life] * 0.5
      dpn[i,][ind > (i + life)] <- 0
    }
  }
  
  dpn <- colSums(dpn)
  
  # If values are NaN or Inf due to a zero depreciation rate, replace with nil
  dpn[!is.finite(dpn)] <- 0
  
  return(round(dpn, 4))
  
}






# --------------------------------------------------------------------------------------------------------------------------
# Depreciation of existing assets, estimation from depreciation charge
# --------------------------------------------------------------------------------------------------------------------------

depn_bv <- function(yrs=20, de, gr, ad, monthly=TRUE) {
  
  # yrs - integer, length of series required in years
  # de  - depreciation expense per annum
  # gr  - gross book value
  # ad  - accumulated depreciation

  bv <- gr + ad                 # Book value
  yrs_rem <- floor(bv / de)     # Years depreciation remaining
  depn <- c(rep(de, yrs_rem), bv - (de * yrs_rem)) 
  
  # Pad or truncate years as required
  if (length(depn) < yrs) {
    depn <- c(depn, rep(0, yrs - length(depn)))
  } else {
    depn <- depn[1:yrs]
  }
  
  if (monthly) {
    depn <- round(rep(depn / 12, each = 12), 3)
  }
  
  return(depn)

}

#depn_bv(yrs=2, de=385, gr=chart[chart$account_no == 3530, "cw_23"], ad=chart[chart$account_no == 3535, "cw_23"], monthly=F)






# --------------------------------------------------------------------------------------------------------------------------
# Optimisation / price goal seek
# --------------------------------------------------------------------------------------------------------------------------

npv_optim_func <- function(theta, pdyr, single, des_f=99, rev_req, p0, q, rtn_mode="obj") {
  
  # Net Present Value Optimisation Objective Function
  #
  # Computes the squared difference between the net present value (NPV) of expected revenue 
  # (based on price changes and quantities) and the regulatory revenue requirement. 
  # This function is typically used as an objective function in optimisation routines 
  # (e.g., to determine optimal price paths over a 5-year regulatory period).
  #
  # Args:
  #   theta    - a numeric vector of length 3 being regulatory rate of return, initial price delta (used in early years), and 
  #              second price delta (used in later years if applicable)
  #   pdyr     - an integer between 0 and 5 representing the year in which the second price delta (`theta[3]`) takes effect,
  #              a value of 0 applies the same delta evenly across all years (that of theta[2] when single = TRUE, else theta[3]). 
  #              A value of 1 sets both deltas equal.
  #   single   - logical, if true the only price delta (that of the first price delta `theta[2]`) is used
  #   des_f    - desired proportion of fixed income.  Default 99 does not change existing proportion
  #   rev_req  - numeric vector of length 5: revenue requirement over the 5-year regulatory period (in millions)
  #   p0       - numeric matrix (n * 1), initial prices 
  #   q        - numeric matrix (n * 5), quantities corresponding to each price and year
  #   rtn_mode - character, if "obj" (default) returns the squared difference between NPV of revenue and required revenue (for 
  #              use in optimisation) ,any other value returns a list with intermediate outputs including `price_delta` 
  #              (length-5 vector of price changes) and `prices` (matrix of adjusted prices)
  #
  # Returns:
  #   see parameter "rtn_mode"
  #
  # Example results
  #
  # theta  <- c(0.025, 0.2, 0.3)
  # pdyr <- 0 | single <- F | 0.3 0.3 0.3 0.3 0.3 0.3  ONE EXTRA!
  # pdyr <- 0 | single <- T | 0.2 0.2 0.2 0.2 0.2
  # pdyr <- 1 | single <- T | 0.2 0.0 0.0 0.0 0.0
  # pdyr <- 1 | single <- F | 0.2 0.2 0.2 0.2 0.2
  # pdyr <- 2 | single <- F | 0.2 0.3 0.3 0.3 0.3
  # pdyr <- 2 | single <- T | 0.0 0.2 0.0 0.0 0.0
  
  
  pdpar1 <- theta[2]  # First price delta   
  pdpar2 <- theta[3]  # Second price delta
  rrr    <- theta[1]  # Regulatory rate of return
  
  pdpar2 <- if (pdyr == 1) pdpar1 else pdpar2
  pdvec  <- c(rep(pdpar1, max(0, pdyr-1)), rep(pdpar2, 5-pdyr+1))
  pdvecT <- rep(pdpar1, 5)
  pdvecT[(1:5)[-pdyr]] <- 0
  pdvec  <- if (single == T) pdvecT else pdvec
  pdvec  <- pdvec[1:5]
  
  # TO DO - insert a parameter so that the proportion of income from usage vs service charges can be flexed
  # see below 
  pdcum        <- cumprod(1 + pdvec) - 1              # vector cumulative of yearly price delta  
  pnew         <- p0 %*% (1 + pdcum)                  # new prices, apply cumulative price deltas to p0 tariffs 
  r            <- pnew * q                            # revenue: prices * quantity by tariff
  tot_r        <- colSums(r) / 1e6                    # total revenue by year
  
  # Return desired proportion of fixed and variable tariffs
  f            <- c("Water.Residential.Fixed"   ,"Water.Non-residential.Fixed"   ,"Sewerage.Non-residential.Fixed"   ,"Sewerage.Residential.Fixed")
  v            <- c("Water.Residential.Variable","Water.Non-residential.Variable","Sewerage.Non-residential.Variable")
  fix_r        <- colSums(r[f, ])                                    # current fixed revenue
  var_r        <- colSums(r[v, ])                                    # current variable revenue
  prop_f       <- sum(fix_r) / (sum(fix_r) + sum(var_r))             # current proportion fixed 
  nfix_r       <- r[f, ] / prop_f * (des_f /100)                     # desired fixed revenue  
  nvar_r       <- r[v, ] / (1 - prop_f) * (1 - (des_f /100))         # desired variable revenue
  
  npnew        <- pnew                                               # copy prices matrix
  npnew[f,]    <- nfix_r / q[f,]                                     # assign new fixed prices
  npnew[v,]    <- nvar_r / q[v,]                                     # assign new variable prices
  nr           <- npnew * q                                          # new revenue: prices * quantity by tariff
  tot_nr       <- colSums(nr) / 1e6                                  # total new revenue by year
  
  #stopifnot("Total revenue unmatched" = max(abs(round(tot_r, 3) - round(tot_nr, 3))) == 0) # check
  
  r_rtn <- if (des_f == 99) tot_r else tot_nr
  p_rtn <- if (des_f == 99) pnew else npnew
  
  npv_tot_r    <- sum(r_rtn   / (1 + rrr) ^ (1:length(r_rtn))  ) * (1 + rrr) ^ 0.5  
  npv_rev_req  <- sum(rev_req / (1 + rrr) ^ (1:length(rev_req))) * (1 + rrr) ^ 0.5
  
  obj          <- (npv_rev_req - npv_tot_r) ^ 2
  
  rtn_list     <- list(price_delta = pdvec, prices = p_rtn)
  
  if (rtn_mode == "obj") return(obj) else return(rtn_list)
  
}






# --------------------------------------------------------------------------------------------------------------------------
# ROU / lease schedule
# --------------------------------------------------------------------------------------------------------------------------

rou_func <- function(lease_pay, periods, rate) {
  
  # Create a lease schedule detailing repayment, interest component & depreciation
  #
  # Args:
  #   lease_pay - monthly lease payment
  #   periods   - number of periods in the lease contract
  #   rate      - discount rate to be used in calculating the right of use asset / lease liability
  #
  # Returns:
  #   matrix being schedule of payments, interest component thereof and depreciation expense
  
  mat <- matrix(rep(0, periods * 8), nrow = periods)
  dimnames(mat)[[1]] <- 1:periods
  dimnames(mat)[[2]] <- c("open_liab","payment","interest","clos_liab","open_depn","depn_exp","clos_depn","inferred_rate")
  pays <- rep(lease_pay, periods)
  
  for (i in 1:length(pays)) {
    pays_ <- pays[i:length(pays)]
    oll <- sum(pays_ / (1 + rate / 12) ^ (1:length(pays_)))
    mat[i,"open_liab"] <- -round(oll,2)
    mat[i,"payment"]   <- round(lease_pay,2)
    mat[i,"interest"]  <- round(mat[i,"open_liab"] * rate / 12 , 2)
    mat[i,"clos_liab"] <- round(sum(mat[i,1:3]), 2)
    mat[i,"open_depn"] <- ifelse(i == 1, 0, mat[i - 1, "clos_depn"])
    mat[i,"depn_exp"]  <- round(mat[1, "open_liab"] / periods, 2)
    mat[i,"clos_depn"] <- sum(mat[i,5:6])
    mat[i,"inferred_rate"] <- round(mat[i,"interest"] * 12 / mat[i,"open_liab"], 3)
  }
  
  return(mat)
}


sca_func <- function(lease_pay, periods, asset_fv) {
  
  # Create a lease schedule detailing repayment, interest component & depreciation
  #
  # Args:
  #   lease_pay - the lease payment
  #   periods   - number of periods in the lease contract
  #   asset_fv  - the fair value of the asset
  #
  # Returns:
  #   matrix being
  
  mat <- matrix(rep(0, periods * 8), nrow = periods)
  dimnames(mat)[[1]] <- 1:periods
  dimnames(mat)[[2]] <- c("open_liab","payment","interest","clos_liab","open_depn","depn_exp","clos_depn","inferred_rate")
  pays <- rep(lease_pay, periods)
  times <- 1:periods
  
  # Present value function
  pv_function <- function(r, cashflows, times, asset_fv) {
    sum(cashflows / (1 + r / 12)^times) - asset_fv
  }
  
  # Find discount rate
  rate <- 
    uniroot(
      pv_function,
      lower = -0.9,
      upper = 0.9,
      cashflows = pays,
      times = times,
      asset_fv = asset_fv
    )$root
  
  for (i in 1:length(pays)) {
    pays_ <- pays[i:length(pays)]
    oll <- sum(pays_ / (1 + rate / 12) ^ (1:length(pays_)))
    mat[i,"open_liab"] <- -round(oll,2)
    mat[i,"payment"]   <- round(lease_pay,2)
    mat[i,"interest"]  <- round(mat[i,"open_liab"] * rate / 12 , 2)
    mat[i,"clos_liab"] <- round(sum(mat[i,1:3]), 2)
    mat[i,"open_depn"] <- ifelse(i == 1, 0, mat[i - 1, "clos_depn"])
    mat[i,"depn_exp"]  <- round(mat[1, "open_liab"] / periods, 2)
    mat[i,"clos_depn"] <- sum(mat[i,5:6])
    mat[i,"inferred_rate"] <- round(mat[i,"interest"] * 12 / mat[i,"open_liab"], 3)
  }
  
  return(mat)
}








# --------------------------------------------------------------------------------------------------------------------------
# Depreciation on opening RAB
# --------------------------------------------------------------------------------------------------------------------------

depn_fun_opn <- function(open_rab_val, open_rab_rem) {
  
  # Create a series of depreciation charges for an opening asset base
  #
  # Args:
  #   open_rab_val - value of the opening Regulatory Asset Base
  #   open_rab_rem -  the remaining life of the opening Regulatory Asset Base
  # 
  # Returns
  #   a series of depreciation charges
  
  open_rab_depn <- open_rab_val / open_rab_rem
  depn1 <- rep(round(open_rab_depn, 5), floor(open_rab_rem))
  depn2 <- open_rab_val - sum(depn1)
  depn <- c(depn1, depn2)
  
  return(depn)
}






# --------------------------------------------------------------------------------------------------------------------------
# Convert yearly data to monthly and add trend and seasonality
# --------------------------------------------------------------------------------------------------------------------------

add_trend_season <- function(y, s, a, p) {
  
  # Convert yearly data to monthly and add trend and seasonality
  #
  # Args:
  #   y - yearly income / expense
  #   s - slope
  #   a - amplitude
  #   p - phase shift
  #
  # Returns
  #   a monthly series
  
  m <- rep(y/12, 12)                                                            # monthly
  m <- m + scale((1:12 * s), center = TRUE, scale = FALSE)                      # monthly with trend
  m <- m[,1]
  
  x <- seq(from = 0, to = 2 * pi, length.out = 12)                              # sequence for seasonality
  y <- scale(a * sin(x + p), center = TRUE, scale = FALSE)
  y <- y[,1]
  
  return(y + m)
}






# --------------------------------------------------------------------------------------------------------------------------
# Select DR's & Cr's
# --------------------------------------------------------------------------------------------------------------------------
drcr <- function(txn, txn_df) {
  trns <- c("dr","cr","dr1","cr1","dr2","cr2","dr3","cr3","dr4","cr4","dr5","cr5","dr6","cr6","dr7","cr7","dr8","cr8","dr9","cr9")
  txn_type_parm = txn_df
  dc <- as.character(na.omit(unname(unlist(txn_type_parm[txn, trns]))))
  return(dc)
}

#drcr(txn = "incm", txn_df = txn_type)






# --------------------------------------------------------------------------------------------------------------------------
# Target cash payment re creditors / debtors based on debtor (creditors) days
# --------------------------------------------------------------------------------------------------------------------------

trgt_days <- function(mat, days, i, d, trail, bal_acnt, pl_acnt, txn, open, clos) {
  
  # Return the cash receipt / payment to arrive at balance required for designated debtors / creditors days
  # - assumes presence of matrix "mat" in environment
  #
  # Args:
  #   days     - days in each month being looped over (vector)
  #   i        - the iteration of the loop passing over "mat"
  #   d        - the target debtors (creditors) days, integer
  #   trail    - number of period to calculate over, integer
  #   bal_acnt - debtors / creditors account
  #   pl_acnt  - P&L account accruing to debtors / creditors account
  #   txn      - the transaction type (vector)
  #   open     - integer index for opening balance
  #   clos     - integer index for closing balance
  #
  # Returns
  #   the cash payment / receipt to arrive at balance required for designated debtors / creditors days 
  
  s <- if (i < trail) 1L else i - (trail - 1L)
  
  if (length(pl_acnt) == 1L || i == 1L) {
    trail_exp <- -mean(mat[pl_acnt, txn, s:i]) * trail
  } else {
    trail_exp <- -mean(colSums(mat[pl_acnt, txn, s:i])) * trail
  }
  
  sum_days    <- mean(days[s:i]) * trail                                  # Sum of calendar days for calc.
  prior_bals  <- mean(mat[bal_acnt, clos, s:(i-1L)]) * (trail-1L)         # Prior opening balances for calc.
  desired_bal <- d * trail_exp / sum_days * trail - prior_bals            # Desired closing balance
  bal_pre     <- mat[bal_acnt, open, i] + abs(sum(mat[pl_acnt, txn, i]))  # Account balance pre cash transaction (opening + accrued)
  
  delta <- round(desired_bal - bal_pre, 3)
  #browser()
  if (prior_bals > 0) {
    rcpt0 <- min(0, max(-bal_pre, delta))
  } else {
    rcpt0 <- max(0, min(-bal_pre, delta))
  }
  
  if (i < trail) {
    rcpt <- -sum(mat[bal_acnt, txn, i])
  } else {
    rcpt <- rcpt0
  }
  
  return(rcpt)
}

#xxx(i=3, d=creditor_days, trail=3, bal_acnt="4000", pl_acnt="2000", txn="exp1")
#mat[c("2000","4000"),c("open","exp1","crd1","clos"),5:7]
#mat[c("3645","4010"),c("open","cpx1","wipc","clos"),1:3]






# --------------------------------------------------------------------------------------------------------------------------
# Accounting numbers
# --------------------------------------------------------------------------------------------------------------------------

acc_num <- function(x){
  xc <- prettyNum(abs(round(x, 0)), big.mark = ',')
  xo1 <- ifelse(
    round(x, 0) > 0, 
    xc, 
    ifelse(
      round(x, 0) == 0, 
      "- ", 
      paste0('(', xc, ')')
    )
  )
  xo <- ifelse(
    round(x, 0) > 0,
    paste0(xo1, ' '),
    xo1
  )
  return(xo)
}

#x <- c(-50000, 50000, -500, -49979, 48778, 1000, -41321, 0, .01)
#acc_num(x)






# --------------------------------------------------------------------------------------------------------------------------
# Transaction matrix to data.frame
# --------------------------------------------------------------------------------------------------------------------------

slr_fun <- function(res, chart) {
  
  res_t <- res$txns
  act <- dimnames(res_t)[[1]]
  txn <- dimnames(res_t)[[2]]
  mon <- dimnames(res_t)[[3]]
  
  dim(res_t) <- c(prod(dim(res_t)), 1)
  df <- expand.grid(act = act, txn = txn, mon = mon, stringsAsFactors = FALSE)
  df$mon <- as.numeric(df$mon)
  df$act <- as.numeric(df$act)
  df$mtd <- res_t[,1]
  
  df1 <- df
  
  # Retain opening balances only for balance sheet, remove all closing balances
  df2 <- rbind(df1[df1$txn == "open" & df1$act >= 3000, ], df1[df1$txn %in% txn[-c(1, length(txn))], ])
  
  # Remove opening balance for P&L accounts
  #df2[df2$txn == "open" , c("mtd","ytd")] <- 0
  
  # Cumulative sum
  df2$yr <- ceiling(df2$mon / 12)
  df2 <- df2[with(df2, order(mon, act, txn)), ] # order
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
  df4 <- df2[df2$txn == "open" & df2$mon %in% (1:(max(as.numeric(mon))/12)*12 - 11) & df2$act >= 3000, ]  # Get annual P1 opening balance for B/S accounts
  for (i in unique(df4$act)) {
    for (j in (1:(max(as.numeric(mon))/12)) ) {
      insert = df4[df4$act == i & df4$yr == j, "mtd"]
      df2[df2$txn == "open" & df2$act == i & df2$yr == j, "ytd"] <- insert
    }
  }
  
  # Remove nil balances and write to csv
  df2 <- df2[rowSums(abs(df2[ ,5:7])) > 0, ]
  slr <- left_join(df2, chart[1:6], by = join_by(act == account_no))
  
  return(slr)
  
}

#res <- res_scenario[[1]]$txns
#slr <- slr_fun(res, chart)






# --------------------------------------------------------------------------------------------------------------------------
# Key performance indicators (KPI's)
# --------------------------------------------------------------------------------------------------------------------------

# https://www.audit.vic.gov.au/report/auditor-generals-report-annual-financial-report-state-victoria-2017-18?section=33061&show-sections=1#33059--appendix-f-water-sector-financial-sustainability-risk-indicators
# Cash interest cover      - Net operating cash flows before net interest and tax payments / Net interest payments
# Gearing ratio            - Total debt (including finance leases) / Total assets
# Internal financing ratio - Net operating cash flows less dividends / Net capital expenditure
# Current ratio            - Current assets / Current liabilities (excluding long-term employee provisions and revenue in advance)
# Return on assets         - Earnings before net interest and tax / average assets
# Return on equity         - Net profit after tax / average total equity
# EBITDA margin            - Earnings before interest, tax, depreciation and amortisation / Total revenue


# ***Cash interest cover*** - Net operating cash flows before net interest and tax payments / Net interest payments
cash_int_cover_fn <- function(m, return_df = FALSE) {
  mat <- m$txns
  
  # 1. Extract individual components for operating cashflows
  # Using drop = FALSE inside apply is a safety measure, but standard indexing works here
  cshd_val <- apply(mat, 3, function(x) x["3000", "cshd"], simplify = TRUE)
  exp2_val <- apply(mat, 3, function(x) x["3000", "exp2"], simplify = TRUE)
  crd1_val <- apply(mat, 3, function(x) x["3000", "crd1"], simplify = TRUE)
  
  # 2. Calculate totals and rolling sums
  op_cf      <- cshd_val + exp2_val + crd1_val
  op_cf_roll <- slide_sum(op_cf, before = 11)
  
  ip         <- apply(mat, 3, function(x) x["3000", "intp"], simplify = TRUE)
  ip_roll    <- slide_sum(ip, before = 11)
  
  # 3. Calculate intermediate ratio and handle division-by-zero / capping
  # If ip_roll is 0, dividing by it yields Inf or NaN. 
  # R handles -ip_roll naturally, but we force Inf/NaN to NA (nil) and cap at 20.
  ratio_inter <- op_cf_roll / -ip_roll
  
  ratio_final <- ratio_inter
  ratio_final[is.na(ratio_final) | is.nan(ratio_final) | is.infinite(ratio_final)] <- NA
  ratio_final[!is.na(ratio_final) & ratio_final > 20] <- 20
  
  # 3. Conditional Return logic
  if (return_df) {
    df <- data.frame(
      cshd          = cshd_val,
      exp2          = exp2_val,
      crd1          = crd1_val,
      op_cf_total   = op_cf,
      op_cf_rolling = op_cf_roll,
      ip            = ip,
      ip_rolling    = ip_roll,
      ratio_intermediate = ratio_inter,
      ratio         = ratio_final,
      row.names     = dimnames(mat)[[3]] # Preserves the time/slice identifiers if they exist
    )
    return(df)
  } else {
    return(ratio_final)
  }
}


# ***Gearing ratio*** - Total debt (including finance leases) / Total assets
gearing_fn <- function(m) {
  mat <- m$txns
  td <- apply(mat, 3, function(x) sum(x[c("4100","4500"), "clos"]), simplify = TRUE)              # total debt
  ta <- apply(mat, 3, function(x) sum(x[grepl("^3", rownames(x)), "clos"]), simplify = TRUE)      # total assets
  return( -td / ta )
}


# ***Internal financing ratio*** - Net operating cash flows less dividends / Net capital expenditure
int_fin_ratio_fn <- function(m) {
  mat <- m$txns
  op_cf <- apply(mat, 3, function(x) sum(x["3000", c("cshd","exp2","crd1","intp")]), simplify = TRUE)   # operating cashflows
  capex <- apply(mat, 3, function(x) sum(x["3000", "wipc"]), simplify = TRUE)                           # capex  To DO - confirm if this should be Net investing cashflows
  
  df <- data.frame(t(mat["3000", c("cshd","exp2","crd1","intp","wipc"), ]))
  df$op_cf <- df$cshd + df$exp2 + df$crd1 + df$int
  df$capex <- -df$wipc
  df$op_cf12 <- round(slide_sum(df$op_cf, before = 11), 0)
  df$capex12 <- round(slide_sum(df$capex, before = 11), 0)
  
  return( slide_sum(-op_cf, before = 11) / slide_sum(capex, before = 11) )
}
 

# ***Current ratio*** - Current assets / Current liabilities (excluding long-term employee provisions and revenue in advance)
current_ratio_fn <- function(m) {
  mat <- m$txns
  g <- "^30|^31|^32|^33"
  ca <- apply(mat, 3, function(x) sum(x[grepl(g, rownames(x)), "clos"]), simplify = TRUE)         # current assets
  g <- "^40|^41|^43|^44"
  cl <- apply(mat, 3, function(x) sum(x[grepl(g, rownames(x)), "clos"]), simplify = TRUE)         # current liabilities
  return( ca / -cl )
}


# ***Return on assets***
ret_on_asset_fn <- function(m) {
  mat <- m$txns
  ni <- apply(mat, 3, function(x) sum(x[grepl("^1|^2", rownames(x)), !colnames(x) %in% c("open","clos")]), simplify = TRUE)   # net income
  ta <- apply(mat, 3, function(x) sum(x[grepl("^3", rownames(x)), "clos"]), simplify = TRUE)                                  # total assets
  return( -slide_sum(ni, before = 11) / slide_mean(ta, before = 11) )
}




# --------------------------------------------------------------------------------------------------------------------------
# Plot KPI function
# https://aosmith.rbind.io/2019/10/14/background-color_gradient/
# --------------------------------------------------------------------------------------------------------------------------

plot_kpi <- function(d, initial_fcast_yr) {
  
  # Plot KPI's defined by individual functions
  #
  # Args:
  #   d        - a list object returned from the function "f"
  #
  # Returns
  #   a faceted plot of KPI's 
  
  mons <- dim(d[[1]]$txns)[3]
  
  bind_rows(
    data.frame( X = t(do.call(rbind, lapply(d, cash_int_cover_fn ))), kpi = "Cash interest cover" ),
    data.frame( X = t(do.call(rbind, lapply(d, gearing_fn        ))), kpi = "Gearing"),
    data.frame( X = t(do.call(rbind, lapply(d, int_fin_ratio_fn  ))), kpi = "Internal financing ratio"),
    data.frame( X = t(do.call(rbind, lapply(d, current_ratio_fn  ))), kpi = "Current ratio"),
    data.frame( X = t(do.call(rbind, lapply(d, ret_on_asset_fn   ))), kpi = "Return on asset")
  ) %>% 
  mutate(
    #month = rep(1:mons, 5), 
    month_end = rep(seq(as.Date(paste(initial_fcast_yr-1,"07","31", sep = "-")) + 1, by = "month", length.out = mons) - 1, 5)
  ) %>% 
  pivot_longer(!c(kpi, month_end), names_to = "scenario", values_to = "value") %>% 
  filter(value != Inf) %>% 
  filter(month(month_end) %in% seq(6, mons, by = 6)) %>% 
  ggplot(aes(x = month_end, y = value, group = scenario)) +
  geom_line(aes(linetype = scenario, colour = scenario)) + 
  scale_colour_manual(values = c("grey70","grey50","grey30","black","#d9230f","#6b1107")) +
  geom_abline(data = data.frame(
    kpi  =c("Cash interest cover","Current ratio","Gearing","Internal financing ratio","Return on asset"),
    bench=c(                 1.5 ,           1.2 ,     0.3 ,                     0.35 ,            0.01 ) 
    ), 
    aes(intercept = bench, slope = 0), color = "red", linetype = "dotdash", linewidth = 1) +
  facet_wrap(vars(kpi), scales = "free") + # https://teunbrand.github.io/ggh4x/reference/facetted_pos_scales.html#scale-transformations
  # ggthemes::theme_base() + 
  # ggthemes::theme_clean() + 
  # theme_minimal() +
  # theme(
  #   legend.position = "none",
  #   panel.grid.minor = element_blank(),
  #   panel.grid.major = element_line(
  #     color      = "grey80",
  #     linewidth  = 0.25,
  #     linetype   = 2
  #     ) 
  # ) +
  labs(x = "", y = "") + 
  cstm_theme1() +
  theme(axis.line = element_line(colour = "grey20"), legend.position = "none")
}



# --------------------------------------------------------------------------------------------------------------------------
# Return trial balance function
# --------------------------------------------------------------------------------------------------------------------------

tb <- function(sim, chart, initial_fcast_yr = 2024) {
  
  # https://shiny.posit.co/r/components/inputs/select-multiple/
  # https://stackoverflow.com/questions/39798042/r-shiny-how-to-use-multiple-inputs-from-selectinput-to-pass-onto-select-optio
  
  # Create a trial balance
  #
  # Args:
  #   d        - a list object returned from the function "f"
  #
  # Returns
  #   a dataframe containing the yearly trial balance
  
  m <- sim[[1]]$txns
  tb1   <- m[ , "clos", (1:20) * 12] / 1000
  
  # Year to date P&L
  tb2 <- tb1
  for (i in 2:20) {
    tb2[as.integer(row.names(tb1)) < 3000, i] <- tb1[as.integer(row.names(tb1)) < 3000, i] - tb1[as.integer(row.names(tb1)) < 3000, i-1]
  }
  
  # Roll P&L to retained earnings
  tb2["5200", ] <- tb2["5200", ] - colSums(tb2)
  
  tb2_df <- as.data.frame(tb2)
  names(tb2_df) <- paste0("FY", initial_fcast_yr:(initial_fcast_yr+19))
  tb2_df$account_no <- as.numeric(rownames(tb2_df))
  tb2_df <- tb2_df %>% 
    left_join(chart, by = join_by(account_no == account_no)) %>% 
    select(account_no, account_desc, paste0("FY", initial_fcast_yr:(initial_fcast_yr+19)))
  
  return(tb2_df)
}





# --------------------------------------------------------------------------------------------------------------------------
# Return cash flow function
# --------------------------------------------------------------------------------------------------------------------------

cf <- function(sim, ref, initial_fcast_yr = 2024) {
  
  # Create a cashflow report
  #
  # Args:
  #   sim - a list object returned from the function "f"
  #   ref - 
  #
  # Returns
  #   a dataframe containing the yearly cash flow
  
  txns <- sim[[1]]$txns
  
  cf1 <- list()
  for (i in ((1:20) * 12)) {
    cf1[[i]] <- rowSums(txns["3000", c("cshd","exp2","crd1","wipc","intp","borr","repy","saca"), (i-11):i])
  }
  cf2 <- do.call(rbind, cf1)
  cf2 <- cf2 / 1000
  cf <- data.frame(t(cf2))
  
  sum_data <- paste0("FY", initial_fcast_yr:(initial_fcast_yr+19))
  
  colnames(cf) <- sum_data
  cf$txn_type <- rownames(cf)
  
  cf <- cf %>% 
    full_join(ref[ref$ref_type == "cash_flow",], by = join_by(txn_type == lookup2)) %>% 
    group_by(ref1, ref2) %>% 
    summarise(across(all_of(sum_data), sum)) %>%
    arrange(ref2)
  
  return(cf)
}






# --------------------------------------------------------------------------------------------------------------------------
# Return rab function
# --------------------------------------------------------------------------------------------------------------------------

rab <- function(d) {
  
  # Return the RAB
  #
  # Args:
  #   d - a list object returned from the function "f"
  #
  # Returns
  #   a data frame containing the RAB for each price period
  
  r <- d[[1]]$rab
  

  return(data.frame(r))
}





# --------------------------------------------------------------------------------------------------------------------------
# Return revenue requirement function
# --------------------------------------------------------------------------------------------------------------------------

revreq <- function(d) {
  
  # Return the RAB
  #
  # Args:
  #   d - a list object returned from the function "f"
  #
  # Returns
  #   a data frame containing the RAB for each price period
  
  r <- d[[1]]$rev_req
  
  
  return(data.frame(r))
}


  
  
# --------------------------------------------------------------------------------------------------------------------------
# Plot financials function
# --------------------------------------------------------------------------------------------------------------------------

plot_fins <- function(sim, chart, ref, sel) {
  
  # https://shiny.posit.co/r/components/inputs/select-multiple/
  # https://stackoverflow.com/questions/39798042/r-shiny-how-to-use-multiple-inputs-from-selectinput-to-pass-onto-select-optio
  
  # Plot financial statements using kableextra
  #
  # Args:
  #   sim      - a list object returned from the function "f"
  #
  # Returns
  #   a table formatted P&L and B/S
  
  txns <- sim[[1]]$txns
  
  # Create trial balance
  tb2_df <- tb(sim=sim, chart=chart, initial_fcast_yr = 2024)
  
  sum_data <- names(tb2_df)[3:ncol(tb2_df)]  # columns to sum
  
  # Income statement
  inc <- tb2_df %>% 
    left_join(chart[1:6], by = join_by(account_no == account_no)) %>% 
    full_join(ref[ref$ref_type == "account_grp",], by = join_by(account_grp == lookup1)) %>% 
    group_by(ref1, ref2) %>% 
    summarise(across(all_of(sum_data), sum)) %>% 
    arrange(ref2) %>% 
    filter(!is.na(ref1)) 
  
  # Balance sheet data
  bal <- tb2_df %>% 
    left_join(chart[1:6], by = join_by(account_no == account_no)) %>% 
    full_join(ref[ref$ref_type == "account_type",], by = join_by(account_type == lookup1)) %>% 
    group_by(ref1, ref2) %>% 
    summarise(across(all_of(sum_data), sum)) %>%
    arrange(ref2) %>% 
    filter(!is.na(ref1)) 
  
  # Cashflow
  cf <- cf(sim, ref, initial_fcast_yr = 2024)
  
  
  # Join Income statement, balance sheet and cashflow
  rep <- as.data.frame(bind_rows(inc, bal, cf)) %>% 
    arrange(ref2) %>% 
    mutate(ref2 = as.character(ref2))
  
  # Insert totals and remove NA's (refer to formula_builder.xlsx to construct code below)
  rep[rep$ref2 == 8, 3:22]  <- colSums(rep[rep$ref2 %in% 2:7        , 3:22], na.rm = T) # Total revenue from transactions
  rep[rep$ref2 == 17, 3:22] <- colSums(rep[rep$ref2 %in% 11:16      , 3:22], na.rm = T) # Total expenses from transactions
  rep[rep$ref2 == 19, 3:22] <- colSums(rep[rep$ref2 %in% c(8,17)    , 3:22], na.rm = T) # Net result from operating transactions
  rep[rep$ref2 == 22, 3:22] <- colSums(rep[rep$ref2 %in% c(19,21)   , 3:22], na.rm = T) # Net result from transactions
  rep[rep$ref2 == 28, 3:22] <- colSums(rep[rep$ref2 %in% c(26,27)   , 3:22], na.rm = T) # Total assets
  rep[rep$ref2 == 36, 3:22] <- colSums(rep[rep$ref2 %in% c(34,35)   , 3:22], na.rm = T) # Total liabilities
  rep[rep$ref2 == 46, 3:22] <- colSums(rep[rep$ref2 %in% c(43,44,45), 3:22], na.rm = T) # Operating CF
  rep[rep$ref2 == 52, 3:22] <- colSums(rep[rep$ref2 %in% c(50,51)   , 3:22], na.rm = T) # Investing CF
  rep[rep$ref2 == 59, 3:22]  <- colSums(rep[rep$ref2 %in% 56:58, 3:22], na.rm = T) # Financing CF
  rep[rep$ref2 == 61, 3:22] <- colSums(rep[rep$ref2 %in% c(46,52,59), 3:22], na.rm = T) # Net increase/(decrease) in cash
  rep[rep$ref2 == 62, 3:22] <- txns["3000", "open", (1+(1:20) * 12 - 12)] / 1000 # Beginning cash
  rep[rep$ref2 == 63, 3:22]  <- colSums(rep[rep$ref2 %in% 61:62, 3:22], na.rm = T) # End cash
  
  
  
  # Calculate KPI's
  cic <- colSums(rep[rep$ref2 %in% c(43,44), 3:22], na.rm = T) / -rep[rep$ref2 == 45, 3:22] # Cash interest cover
  gr  <- abs(rep[rep$ref2 == 38, 3:22]) / abs(rep[rep$ref2 == 28, 3:22]) * 100              # Gearing ratio
  ifr <- rep[rep$ref2 == 46, 3:22] / -rep[rep$ref2 == 50, 3:22]                             # Internal financing ratio
  
  # Format numbers
  rep[ ,3:22] <- apply(rep[ ,3:22], 2, acc_num)
  rep[is.na(rep)] <- ""
  
  # Insert KPI's post character conversion
  rep[rep$ref2 == 39, 3:22] <- paste0(round(gr, 0), "%")
  rep[rep$ref2 == 47, 3:22] <- paste0(round(cic, 1), "x")
  rep[rep$ref2 == 53, 3:22] <- paste0(round(ifr, 1), "x")
  
  # # replace NA's with zero
  # nas <- which(is.na(cf[cf$ref2 == 49, 3:22]))
  # cf[cf$ref2 == 49, nas + 2 ] <- 0
  # 
  # # replace zero with dash (perform after character conversion)
  # zer <- which(cf[cf$ref2 == 53, 3:22] == 0)
  # cf[cf$ref2 == 49, zer + 2 ] <- "-"
  
  b <- unique(ref[grepl("b",ref$ref3), "ref2"]) # bold
  s <- unique(ref[grepl("s",ref$ref3), "ref2"]) # extra_css = "border-bottom: 1px solid"
  i <- unique(ref[grepl("i",ref$ref3), "ref2"]) # italic
  
  rep %>% 
    select(-ref2) %>% 
    select(c(ref1, all_of(sel))) %>%
    rename(" " = ref1) %>% 
    kbl(align = c("l", rep("r", length(sel)))
      #caption = "Comprehensive operating statement",
      #align = c("l", rep("r", length(all_of(sel))))  # TO DO - this needs to be dynamic
    ) %>% 
    kable_classic(full_width = F, html_font = "Cambria") %>% 
    column_spec(1, width = "25em") %>%
    column_spec(2:(1+length(sel)), width = "8em") %>% 
    row_spec(b, bold = TRUE) %>% 
    row_spec(i, italic = TRUE) %>% 
    row_spec(s, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(c(22,39,59), extra_css = "border-bottom: 2px solid") %>%
    row_spec(which(rep$ref1 == "blank"), color = "white") 
  
}






# --------------------------------------------------------------------------------------------------------------------------
# Opex & capex plots
# --------------------------------------------------------------------------------------------------------------------------

plot_opex_capex <- function(d) {
  rab        <- sim[[1]]$rab
  rev_req    <- sim[[1]]$rev_req
  
  data.frame(
    year = rep(rownames(rev_req), 2),
    exp  = c( rep("opex", 20), rep("capex", 20) ),
    nmnl = c( rev_req$opx, colSums(rab[c("capex","cust_cont"), ]) )
    ) %>% 
    ggplot(aes(x=year, y=nmnl, fill=exp)) + 
    geom_bar(stat="identity", position="stack") + scale_fill_grey() + #position=position_dodge()
    ggthemes::theme_base()
} 




# --------------------------------------------------------------------------------------------------------------------------
# Plot tariffs
# --------------------------------------------------------------------------------------------------------------------------

plot_tariffs <- function(d){
  
  tariff_rev <- d[[1]]$tariff_rev
  
  point_data <- data.frame(
    year = 2024:2043,
    perc_fixed = colSums(tariff_rev[grepl("Fixed", rownames(tariff_rev)),]) / colSums(tariff_rev) * max(colSums(tariff_rev)) / 1e3
  )
  
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
    geom_line(
      data = point_data,
      aes(x = year, y = perc_fixed),
      colour = "#1f3a5f",
      linetype = "solid",
      linewidth = 0.6
    ) +
    geom_point(
      data = point_data,
      aes(x = year, y = perc_fixed), colour="#1f3a5f", shape = 21, size = 4, fill = "white", stroke = 1.5
    ) +
    scale_y_continuous("", sec.axis = sec_axis(~ . / (max(colSums(tariff_rev)) / 1e3), name = "")) +
    scale_x_continuous(breaks = c(2028,2033,2038,2043)) +
    ggthemes::theme_base() +
    labs(
      x = "",
      y = "",
      title = "Tariff revenue - fixed vs variable (left axis, $m)",
      subtitle = "Proportion of fixed tariff revenue (right axis, round points)"
    ) + 
    theme(
      plot.subtitle = element_text(size = 11.5, face = 'italic'),
      legend.title = element_blank(),
      legend.position = c(0.1,0.9),
      legend.background = element_blank(),
      legend.key = element_blank(),
    )
}



# --------------------------------------------------------------------------------------------------------------------------
# Plot tariff prices for one tariff group (variable charges above fixed charges, price and % change)
# --------------------------------------------------------------------------------------------------------------------------

plot_tariff_prices <- function(d, group) {

  # Two stacked panels for a single tariff group - variable charges on top, fixed charges below.  In each panel
  # the lines are real prices (left axis) and the grey bars the year-on-year percentage change (right axis).
  #
  # The panels are built as separate ggplots and stacked with patchwork (rather than facet_wrap) because
  # sec_axis takes a single transform - each panel needs its own price-to-percentage rescale factor, since
  # variable charges ($/kL) and fixed charges ($ p.a.) differ by orders of magnitude.
  #
  # Args:
  #   d      - list of f() results (as returned by simA() / simB()), the first element is plotted
  #   group  - tariff group, i.e. the "lookup_key2" segment of the tariff name (e.g. "Water")
  #
  # Returns
  #   a patchwork object

  # Preparation ------------------------------------------------------------------------------------
  prices <- d[[1]]$prices

  # Group and charge type are derived from tariff naming, per the contract documented in R/f.R
  tariff_group    <- sub("\\..*", "", rownames(prices))
  is_variable     <- grepl("Variable", rownames(prices))
  group_prices    <- prices[tariff_group == group, , drop = FALSE]
  group_variable  <- is_variable[tariff_group == group]

  variable_panel <- tariff_price_panel(group_prices[group_variable,  , drop = FALSE], "Variable charges")
  fixed_panel    <- tariff_price_panel(group_prices[!group_variable, , drop = FALSE], "Fixed charges")


  # Output -----------------------------------------------------------------------------------------
  # The overall title sits on an empty header plot rather than patchwork::plot_annotation(theme = ...),
  # which the installed ggplot2 / patchwork versions don't style reliably - this keeps the KPI charts'
  # theme_base bold title and italic subtitle.
  header <- ggplot() +
    ggthemes::theme_base() +
    labs(
      title    = paste0(group, " tariffs (left axis, real $)"),
      subtitle = "Year-on-year price change (right axis, grey bars)"
    ) +
    theme(
      plot.subtitle   = element_text(size = 11.5, face = 'italic'),
      panel.border    = element_blank(),
      plot.background = element_blank()
    )

  patchwork::wrap_plots(header, variable_panel, fixed_panel, ncol = 1, heights = c(0.001, 1, 1))
}


tariff_year_limits <- function(years) {

  # x-axis limits shared by the two chart panels of plot_tariff_prices(), so their years line up.
  # The extra space after the final year holds the line labels.
  #
  # Args:
  #   years  - integer vector of the years plotted
  #
  # Returns
  #   numeric vector of length 2

  label_space_years <- 5 * 2 / 3
  c(min(years) - 0.5, max(years) + label_space_years)
}


plot_panel_margins_px <- function(plot) {

  # Distance (px) from the left and right edges of a rendered patchwork plot to its panel borders -
  # used to inset the tariff price table so its borders line up with the chart panels above it.
  #
  # Every layout column outside the panel (axis text, axis titles, secondary axis, plot margins) has a
  # fixed width in points; only the panel column ("null" unit) stretches with the output width.  So the
  # offsets are constant for a given plot regardless of browser width.  Shiny's renderPlot draws at
  # res = 72 (app.R doesn't override it), so 1 point = 1 CSS px.
  #
  # Args:
  #   plot  - a patchwork object, e.g. from plot_tariff_prices()
  #
  # Returns
  #   named numeric vector c(left, right), in px

  plot_gtable  <- patchwork::patchworkGrob(plot)
  panel_column <- plot_gtable$layout$l[plot_gtable$layout$name == "panel-1"]

  column_widths_pt <- numeric(length(plot_gtable$widths))
  for (column_index in seq_along(plot_gtable$widths)) {
    if (column_index == panel_column) next  # the stretching "null" panel column has no fixed width
    column_widths_pt[column_index] <- grid::convertWidth(plot_gtable$widths[column_index], "pt", valueOnly = TRUE)
  }

  c(
    left  = sum(column_widths_pt[seq_len(panel_column - 1)]),
    right = sum(column_widths_pt[(panel_column + 1):length(column_widths_pt)])
  )
}



# --------------------------------------------------------------------------------------------------------------------------
# Table of tariff prices for one tariff group (shown beneath plot_tariff_prices on the Tariffs tab)
# --------------------------------------------------------------------------------------------------------------------------

tariff_price_table <- function(d, group, panel_margins_px = c(left = 0, right = 0)) {

  # HTML table of the real price (2 decimal places for tariffs above 10, else 4) and year-on-year change (1 decimal place %) of each
  # tariff in a group, one column per year.  Styled with kable_classic to match the Financials tab.
  # The table fills its container and scrolls sideways when the 20 years don't fit, with the first
  # (tariff name) column held in place so each row stays identifiable while scrolling.
  #
  # Args:
  #   d      - list of f() results (as returned by simA() / simB()), the first element is tabled
  #   group             - tariff group, i.e. the "lookup_key2" segment of the tariff name (e.g. "Water")
  #   panel_margins_px  - left/right inset (px) of the table's border box, from plot_panel_margins_px(), so
  #                       its borders line up with the chart panels above.  Default 0 fills the container.
  #
  # Returns
  #   kableExtra HTML

  # Preparation - variable tariffs first, matching the order of the chart panels -------------------
  prices <- d[[1]]$prices

  # Group and charge type are derived from tariff naming, per the contract documented in R/f.R
  tariff_group   <- sub("\\..*", "", rownames(prices))
  is_variable    <- grepl("Variable", rownames(prices))
  group_prices   <- prices[tariff_group == group, , drop = FALSE]
  group_variable <- is_variable[tariff_group == group]

  tariff_order <- c(which(group_variable), which(!group_variable))
  n_variable   <- sum(group_variable)
  n_fixed      <- sum(!group_variable)

  years      <- as.integer(colnames(group_prices))
  year_names <- paste0("FY", substr(years, 3, 4))
  n_years    <- length(years)


  # Computation - a price row and a % change row per tariff ----------------------------------------
  table_rows <- list()

  for (tariff_index in tariff_order) {
    tariff_name    <- rownames(group_prices)[tariff_index]
    tariff_price   <- group_prices[tariff_index, ]
    prior_price    <- c(NA, head(tariff_price, -1))
    pct_change     <- tariff_price / prior_price - 1
    customer_class <- strsplit(tariff_name, ".", fixed = TRUE)[[1]][2]

    # Larger tariffs (e.g. fixed charges in $ p.a.) to 2 decimal places, smaller (e.g. $/kL) to 4.
    # Decided per tariff rather than per value so a row never mixes decimal places when a price
    # crosses 10 during the forecast.
    price_decimals <- if (max(tariff_price) > 10) 2 else 4
    price_text     <- formatC(tariff_price, format = "f", digits = price_decimals, big.mark = ",")

    # FY2024 has no prior-year price in the simulation output, so its change is shown as a dash
    change_text <- ifelse(is.na(pct_change), "–", scales::percent(pct_change, accuracy = 0.1))

    table_rows[[length(table_rows) + 1]] <- setNames(c(customer_class, price_text),  c("Tariff", year_names))
    table_rows[[length(table_rows) + 1]] <- setNames(c("yoy change",   change_text), c("Tariff", year_names))
  }

  table_df <- as.data.frame(do.call(rbind, table_rows), stringsAsFactors = FALSE)
  names(table_df)[1] <- " "

  n_rows             <- nrow(table_df)
  change_row_indices <- seq(2, n_rows, by = 2)


  # Header band - one heading per 5-year price-setting period (e.g. "FY24-28") ----------------------
  period_starts  <- seq(1, n_years, by = 5)
  period_headers <- c(" " = 1)
  for (period_start in period_starts) {
    period_end   <- min(period_start + 4, n_years)
    period_label <- paste0(year_names[period_start], "-", substr(years[period_end], 3, 4))
    period_headers[period_label] <- period_end - period_start + 1
  }


  # Output -----------------------------------------------------------------------------------------
  # Sticky first column: the tariff names stay visible while the year columns scroll sideways
  sticky_css <- "position: sticky; left: 0; background-color: white; z-index: 1;"

  price_table <- table_df %>%
    kbl(align = c("l", rep("r", n_years)), row.names = FALSE) %>%
    kable_classic(full_width = TRUE, html_font = "Cambria", font_size = 12) %>%
    add_header_above(period_headers)

  if (n_variable > 0) {
    price_table <- pack_rows(price_table, "Variable charges", 1, 2 * n_variable)
  }
  if (n_fixed > 0) {
    price_table <- pack_rows(price_table, "Fixed charges", 2 * n_variable + 1, n_rows)
  }

  price_table %>%
    row_spec(change_row_indices, italic = TRUE, color = "#555555") %>%
    row_spec(change_row_indices[-length(change_row_indices)], extra_css = "border-bottom: 1px solid #dddddd") %>%
    column_spec(1, width_min = "10em", extra_css = sticky_css, include_thead = TRUE) %>%
    column_spec(2:(n_years + 1), width_min = "5.5em") %>%
    scroll_box(
      # "auto" fills the space between the margins ("100%" plus margins would overflow).  A black 1px
      # border with no padding matches the chart panel borders it lines up with.
      width   = "auto",
      box_css = paste0(
        "border: 1px solid #000000; padding: 0; box-sizing: border-box; ",
        "margin-left: ",  round(panel_margins_px[["left"]]),  "px; ",
        "margin-right: ", round(panel_margins_px[["right"]]), "px;"
      )
    )
}


tariff_price_panel <- function(panel_prices, panel_title) {

  # Build one panel of plot_tariff_prices().
  #
  # Args:
  #   panel_prices  - matrix of real prices, tariffs (named rows) x years (named columns)
  #   panel_title   - title shown above the panel
  #
  # Returns
  #   a ggplot object

  # Validation - a group with no tariffs of this charge type gets a placeholder rather than an error
  if (nrow(panel_prices) == 0) {
    placeholder <- ggplot() +
      annotate("text", x = 0, y = 0, label = "No tariffs of this type in the selected group", size = 5) +
      theme_void() +
      labs(title = panel_title)
    return(placeholder)
  }


  # Preparation - long data, one row per tariff per year -------------------------------------------
  years      <- as.integer(colnames(panel_prices))
  panel_rows <- vector(mode = "list", length = nrow(panel_prices))

  for (tariff_index in seq_len(nrow(panel_prices))) {
    tariff_name  <- rownames(panel_prices)[tariff_index]
    tariff_price <- panel_prices[tariff_index, ]

    # FY2024 has no prior-year price in the simulation output, so its change is NA (no bar drawn)
    prior_price  <- c(NA, head(tariff_price, -1))

    panel_rows[[tariff_index]] <- data.frame(
      tariff     = tariff_name,
      label      = strsplit(tariff_name, ".", fixed = TRUE)[[1]][2],  # customer class, e.g. "Residential"
      year       = years,
      price      = tariff_price,
      pct_change = tariff_price / prior_price - 1
    )
  }
  panel_df <- do.call(rbind, panel_rows)


  # Computation - dual axis scaling ----------------------------------------------------------------
  # Same approach as the KPI charts (bar_axis_max / ratio_axis_max), with the axes swapped: the lines
  # carry the left axis, the bars are scaled onto it.  The 5% floor stops a flat price path from
  # stretching near-zero changes to full height.
  price_axis_max  <- max(panel_df$price)
  change_axis_max <- max(c(abs(panel_df$pct_change), 0.05), na.rm = TRUE)
  rescale_factor  <- price_axis_max / change_axis_max

  panel_df$pct_change_scaled <- panel_df$pct_change * rescale_factor


  # Computation - one bar per distinct price delta path -------------------------------------------
  # Tariffs whose year-on-year changes are identical over the whole horizon (e.g. under the uniform
  # price path modes) share a single bar rather than repeating the same bar side by side.  Changes are
  # rounded to 0.01 percentage points so solver floating-point noise doesn't split a group.  Grouping
  # is on the whole path (not year by year) so the number of bars, and their width, is constant.
  delta_path_key <- character(0)
  for (tariff_name in unique(panel_df$tariff)) {
    tariff_changes <- panel_df$pct_change[panel_df$tariff == tariff_name]
    delta_path_key[tariff_name] <- paste(round(tariff_changes, 4), collapse = "|")
  }

  bar_rows <- list()
  for (path_key in unique(delta_path_key)) {
    member_tariffs <- names(delta_path_key)[delta_path_key == path_key]
    member_labels  <- unique(panel_df$label[panel_df$tariff %in% member_tariffs])

    # The first member's rows stand in for the whole bar group (their changes are identical)
    first_member_rows <- panel_df[panel_df$tariff == member_tariffs[1], ]

    bar_rows[[length(bar_rows) + 1]] <- data.frame(
      bar_label         = paste(member_labels, collapse = " / "),
      year              = first_member_rows$year,
      pct_change        = first_member_rows$pct_change,
      pct_change_scaled = first_member_rows$pct_change_scaled
    )
  }
  bar_df <- do.call(rbind, bar_rows)
  bar_df <- bar_df[!is.na(bar_df$pct_change), ]  # FY2024 has no prior-year price, so no bar


  line_colours<- c("#1f3a5f", "#b5651d", "#2e7d32", "#6a1b9a", "#8b1a1a")
  n_labels     <- length(unique(panel_df$label))
  label_df     <- panel_df[panel_df$year == max(panel_df$year), ]


  # Output -----------------------------------------------------------------------------------------
  ggplot(panel_df, aes(x = year)) +
    geom_col(
      data = bar_df,
      aes(y = pct_change_scaled, fill = bar_label), position = "dodge", colour = "black"
    ) +
    scale_fill_grey(start = 0.8, end = 0.4) +
    geom_line(aes(y = price, colour = label), linewidth = 1) +
    geom_point(aes(y = price, colour = label), shape = 21, size = 4, fill = "white", stroke = 1.5) +
    ggrepel::geom_text_repel(
      data = label_df,
      aes(y = price, label = label, colour = label),
      nudge_x = 0.5, direction = "y", hjust = 0, segment.colour = "grey50", size = 4
    ) +
    scale_colour_manual(values = line_colours[seq_len(n_labels)], guide = "none") +
    scale_y_continuous("", sec.axis = sec_axis(~ . / rescale_factor, name = "", labels = scales::percent_format(accuracy = 1))) +
    scale_x_continuous(breaks = c(2028, 2033, 2038, 2043), limits = tariff_year_limits(panel_df$year)) +
    ggthemes::theme_base() +
    labs(x = "", y = "", title = panel_title) +
    theme(
      plot.title         = element_text(size = 13),
      legend.title       = element_blank(),
      legend.position    = "bottom",
      legend.background  = element_blank(),
      legend.key         = element_blank()
    )
}



# --------------------------------------------------------------------------------------------------------------------------
# Plot cash interest cover (net operating cashflow vs net interest payments, with the ratio)
# --------------------------------------------------------------------------------------------------------------------------

plot_cash_int_cover <- function(d){

  m             <- d[[1]]$txns
  yr_end_months <- (1:20) * 12

  cshd_monthly <- apply(m, 3, function(x) x["3000", "cshd"], simplify = TRUE)
  exp2_monthly <- apply(m, 3, function(x) x["3000", "exp2"], simplify = TRUE)
  crd1_monthly <- apply(m, 3, function(x) x["3000", "crd1"], simplify = TRUE)
  ip_monthly   <- apply(m, 3, function(x) x["3000", "intp"], simplify = TRUE)

  op_cf_monthly <- cshd_monthly + exp2_monthly + crd1_monthly

  net_operating_cf  <- slide_sum(op_cf_monthly, before = 11)[yr_end_months] / 1000   # $m
  net_interest_paid <- slide_sum(ip_monthly,    before = 11)[yr_end_months] / 1000   # $m
  cash_int_cover     <- net_operating_cf / -net_interest_paid

  # Cap / nil-out as per cash_int_cover_fn's convention
  cash_int_cover[is.na(cash_int_cover) | is.nan(cash_int_cover) | is.infinite(cash_int_cover)] <- NA
  cash_int_cover[!is.na(cash_int_cover) & cash_int_cover > 20] <- 20

  bar_axis_max   <- max(abs(net_operating_cf), abs(net_interest_paid))
  ratio_axis_max <- 3     # gives headroom above the 1.5 target minimum
  rescale_factor <- bar_axis_max / ratio_axis_max

  ggplot() +
    geom_bar(
      data = data.frame(
        category = factor(
          rep(c("Net operating cashflow", "Net interest payments"), each = 20),
          levels = c("Net operating cashflow", "Net interest payments")
        ),
        year  = rep(2024:2043, 2),
        value = c(net_operating_cf, net_interest_paid)
      ),
      aes(fill = category, y = value, x = year), position = "identity", stat = "identity", colour = "black"
    ) +
    scale_fill_manual(values = c("grey60", "grey30")) +
    geom_line(
      data = data.frame(year = 2024:2043, ratio_scaled = cash_int_cover * rescale_factor),
      aes(x = year, y = ratio_scaled, colour = "Cash interest cover (times)"), linewidth = 1
    ) +
    geom_point(
      data = data.frame(year = 2024:2043, ratio_scaled = cash_int_cover * rescale_factor),
      aes(x = year, y = ratio_scaled, colour = "Cash interest cover (times)"), shape = 21, size = 4, fill = "white", stroke = 1.5
    ) +
    geom_abline(aes(intercept = 1.5 * rescale_factor, slope = 0), color = "red", linetype = "dotdash", linewidth = 0.75) +
    scale_colour_manual(values = c("Cash interest cover (times)" = "#1f3a5f")) +
    scale_y_continuous("", sec.axis = sec_axis(~ . / rescale_factor, name = "")) +
    scale_x_continuous(breaks = c(2028, 2033, 2038, 2043)) +
    ggthemes::theme_base() +
    labs(
      x = "",
      y = "",
      title    = "Cash interest cover (left axis, $m)",
      subtitle = "Net operating cashflow / net interest payments (right axis, times); target minimum 1.5"
    ) +
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2, title = NULL)) +
    theme(
      plot.subtitle      = element_text(size = 11.5, face = 'italic'),
      legend.title       = element_blank(),
      legend.position    = "bottom",
      legend.box         = "horizontal",
      legend.background  = element_blank(),
      legend.key         = element_blank()
    )
}



# --------------------------------------------------------------------------------------------------------------------------
# Plot short-term liquidity (current assets vs current liabilities, with current ratio)
# --------------------------------------------------------------------------------------------------------------------------

plot_current_ratio <- function(d){

  m               <- d[[1]]$txns
  yr_end_months   <- (1:20) * 12
  clos_annual     <- m[ , "clos", yr_end_months]        # accounts x 20 FY-end snapshots

  ca_grp <- "^30|^31|^32|^33"   # current asset account codes    (current_ratio_fn convention)
  cl_grp <- "^40|^41|^43|^44"   # current liability account codes (current_ratio_fn convention)

  current_assets      <- colSums(clos_annual[grepl(ca_grp, rownames(clos_annual)), ]) / 1000
  current_liabilities <- colSums(clos_annual[grepl(cl_grp, rownames(clos_annual)), ]) / 1000
  current_ratio       <- current_assets / -current_liabilities

  bar_axis_max   <- max(current_assets, -current_liabilities)
  ratio_axis_max <- 2   # matches the "Current ratio" benchmark scale used in plot_kpi (bench = 1.2)
  rescale_factor <- bar_axis_max / ratio_axis_max

  ggplot() +
    geom_bar(
      data = data.frame(
        category = factor(
          rep(c("Total Current Assets", "Total Current Liabilities"), each = 20),
          levels = c("Total Current Assets", "Total Current Liabilities")
        ),
        year  = rep(2024:2043, 2),
        value = c(current_assets, current_liabilities)
      ),
      aes(fill = category, y = value, x = year), position = "identity", stat = "identity", colour = "black"
      #aes(fill = category, y = value, x = year), position = "dodge", stat = "identity", colour = "black"
    ) +
    scale_fill_manual(values = c("grey60", "grey30")) +
    geom_line(
      data = data.frame(year = 2024:2043, ratio_scaled = current_ratio * rescale_factor),
      aes(x = year, y = ratio_scaled, colour = "Current ratio (times)"), linewidth = 1
    ) +
    geom_point(
      data = data.frame(year = 2024:2043, ratio_scaled = current_ratio * rescale_factor),
      aes(x = year, y = ratio_scaled, colour = "Current ratio (times)"), shape = 21, size = 4, fill = "white", stroke = 1.5
    ) +
    geom_abline(aes(intercept = 1.2 * rescale_factor, slope = 0), color = "red", linetype = "dotdash", linewidth = 0.75) +
    scale_colour_manual(values = c("Current ratio (times)" = "#1f3a5f")) +
    scale_y_continuous("", sec.axis = sec_axis(~ . / rescale_factor, name = "")) +
    scale_x_continuous(breaks = c(2028, 2033, 2038, 2043)) +
    ggthemes::theme_base() +
    labs(
      x = "",
      y = "",
      title    = "Current ratio (left axis, $m)",
      subtitle = "Current assets / current liabilities (right axis, times)"
    ) +
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2, title = NULL)) +
    theme(
      plot.subtitle      = element_text(size = 11.5, face = 'italic'),
      legend.title       = element_blank(),
      legend.position    = "bottom",
      legend.box         = "horizontal",
      legend.background  = element_blank(),
      legend.key         = element_blank()
    )
}



# --------------------------------------------------------------------------------------------------------------------------
# Plot gearing (total debt vs total assets, with the gearing ratio)
# --------------------------------------------------------------------------------------------------------------------------

plot_gearing <- function(d){

  m               <- d[[1]]$txns
  yr_end_months   <- (1:20) * 12
  clos_annual     <- m[ , "clos", yr_end_months]        # accounts x 20 FY-end snapshots

  total_debt   <- colSums(clos_annual[c("4100","4500"), , drop = FALSE]) / 1000                # gearing_fn convention
  total_assets <- colSums(clos_annual[grepl("^3", rownames(clos_annual)), ]) / 1000             # gearing_fn convention
  gearing      <- -total_debt / total_assets

  bar_axis_max   <- max(abs(total_debt), abs(total_assets))
  ratio_axis_max <- 1   # gives headroom above the 0.3 target maximum
  rescale_factor <- bar_axis_max / ratio_axis_max

  ggplot() +
    geom_bar(
      data = data.frame(
        category = factor(
          rep(c("Total debt", "Total assets"), each = 20),
          levels = c("Total debt", "Total assets")
        ),
        year  = rep(2024:2043, 2),
        value = c(total_debt, total_assets)
      ),
      aes(fill = category, y = value, x = year), position = "identity", stat = "identity", colour = "black"
    ) +
    scale_fill_manual(values = c("grey60", "grey30")) +
    geom_line(
      data = data.frame(year = 2024:2043, ratio_scaled = gearing * rescale_factor),
      aes(x = year, y = ratio_scaled, colour = "Gearing ratio (times)"), linewidth = 1
    ) +
    geom_point(
      data = data.frame(year = 2024:2043, ratio_scaled = gearing * rescale_factor),
      aes(x = year, y = ratio_scaled, colour = "Gearing ratio (times)"), shape = 21, size = 4, fill = "white", stroke = 1.5
    ) +
    geom_abline(aes(intercept = 0.3 * rescale_factor, slope = 0), color = "red", linetype = "dotdash", linewidth = 0.75) +
    scale_colour_manual(values = c("Gearing ratio (times)" = "#1f3a5f")) +
    scale_y_continuous("", sec.axis = sec_axis(~ . / rescale_factor, name = "")) +
    scale_x_continuous(breaks = c(2028, 2033, 2038, 2043)) +
    ggthemes::theme_base() +
    labs(
      x = "",
      y = "",
      title    = "Gearing ratio (left axis, $m)",
      subtitle = "Total debt / total assets (right axis, times); target maximum 0.3"
    ) +
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2, title = NULL)) +
    theme(
      plot.subtitle      = element_text(size = 11.5, face = 'italic'),
      legend.title       = element_blank(),
      legend.position    = "bottom",
      legend.box         = "horizontal",
      legend.background  = element_blank(),
      legend.key         = element_blank()
    )
}



# --------------------------------------------------------------------------------------------------------------------------
# Plot internal financing ratio (net operating cashflow vs net investing cashflow, with the ratio)
# --------------------------------------------------------------------------------------------------------------------------

plot_int_fin_ratio <- function(d){

  m             <- d[[1]]$txns
  yr_end_months <- (1:20) * 12

  op_cf_monthly <- apply(m, 3, function(x) sum(x["3000", c("cshd","exp2","crd1","intp")]), simplify = TRUE)
  capex_monthly <- apply(m, 3, function(x) sum(x["3000", "wipc"]), simplify = TRUE)

  net_operating_cf <- slide_sum(op_cf_monthly, before = 11)[yr_end_months] / 1000   # $m
  net_investing_cf <- slide_sum( capex_monthly, before = 11)[yr_end_months] / 1000   # $m
  int_fin_ratio    <- -net_operating_cf / net_investing_cf

  bar_axis_max   <- max(abs(net_operating_cf), abs(net_investing_cf))
  ratio_axis_max <- 1     # gives headroom above the 0.35 target minimum
  rescale_factor <- bar_axis_max / ratio_axis_max

  ggplot() +
    geom_bar(
      data = data.frame(
        category = factor(
          rep(c("Net operating cashflow", "Net investing cashflow"), each = 20),
          levels = c("Net operating cashflow", "Net investing cashflow")
        ),
        year  = rep(2024:2043, 2),
        value = c(net_operating_cf, net_investing_cf)
      ),
      aes(fill = category, y = value, x = year), position = "identity", stat = "identity", colour = "black"
    ) +
    scale_fill_manual(values = c("grey60", "grey30")) +
    geom_line(
      data = data.frame(year = 2024:2043, ratio_scaled = int_fin_ratio * rescale_factor),
      aes(x = year, y = ratio_scaled, colour = "Internal financing ratio (times)"), linewidth = 1
    ) +
    geom_point(
      data = data.frame(year = 2024:2043, ratio_scaled = int_fin_ratio * rescale_factor),
      aes(x = year, y = ratio_scaled, colour = "Internal financing ratio (times)"), shape = 21, size = 4, fill = "white", stroke = 1.5
    ) +
    geom_abline(aes(intercept = 0.35 * rescale_factor, slope = 0), color = "red", linetype = "dotdash", linewidth = 0.75) +
    scale_colour_manual(values = c("Internal financing ratio (times)" = "#1f3a5f")) +
    scale_y_continuous("", sec.axis = sec_axis(~ . / rescale_factor, name = "")) +
    scale_x_continuous(breaks = c(2028, 2033, 2038, 2043)) +
    ggthemes::theme_base() +
    labs(
      x = "",
      y = "",
      title    = "Internal financing ratio (left axis, $m)",
      subtitle = "Net operating cashflow / net investing cashflow (right axis, times); target minimum 0.35"
    ) +
    guides(fill = guide_legend(order = 1), colour = guide_legend(order = 2, title = NULL)) +
    theme(
      plot.subtitle      = element_text(size = 11.5, face = 'italic'),
      legend.title       = element_blank(),
      legend.position    = "bottom",
      legend.box         = "horizontal",
      legend.background  = element_blank(),
      legend.key         = element_blank()
    )
}



# --------------------------------------------------------------------------------------------------------------------------
# Get data function
# --------------------------------------------------------------------------------------------------------------------------

get_data <- function(src = "local") {
  
  # Data sources
  if (src == "local") {
    dat_src   <- "./data/price_subm_2023.csv"
    ref_src   <- "./data/reference.csv"
    funs_src  <- "funs.R"
    chart_src <- "./data/chart.csv"
    txn_src   <- "./data/txn_type.csv"
  } else {
    dat_src   <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/price_subm_2023.csv"
    ref_src   <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/reference.csv"
    funs_src  <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/funs.R"
    chart_src <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/chart.csv"
    txn_src   <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/txn_type.csv"
  }
  
  dat      <- read.csv(dat_src, fileEncoding="UTF-8-BOM")
  ref      <- read.csv(ref_src, fileEncoding="UTF-8-BOM")
  chart    <- read.csv(chart_src, fileEncoding="UTF-8-BOM")
  txn_type <- read.csv(txn_src, fileEncoding="UTF-8-BOM")
  rownames(txn_type) <- txn_type$txn_code
  
  return(list(
    dat          = dat, 
    ref          = ref, 
    chart        = chart, 
    txn_type     = txn_type
  ))
}





# --------------------------------------------------------------------------------------------------------------------------
# Create growth factor series
# --------------------------------------------------------------------------------------------------------------------------

growth_fctr <- function(esc_rate = c(0.025, 0.01), len = c(5,3)) {
  
  # Create growth in quantities OR an inflation factor for prices
  # 
  # Args:.
  #   esc_rate    - escalation rate
  #   len         - length of series
  #
  # Returns:
  #   EXPLANATION HERE...
  
  stopifnot("The arguments 'esc_rate' and 'len' must be of equal length"= length(esc_rate) == length(len))
  
  if (length(len) == 1) r <- rep(esc_rate, each = len) else r <- rep(esc_rate, times = len)
  cumprod(1 + r)
}





# --------------------------------------------------------------------------------------------------------------------------
# Create annual financial data series
# --------------------------------------------------------------------------------------------------------------------------

real_series <- function(q, p, q_grow) {
  
  # Create 20 year annual financial data series (revenue, cost, capex, etc) from price, quantity and a quantity growth factor
  # - price and quantity data is multiplied to construct annual amounts for years 1 through 5 (ps1)
  # - this is escalated by the growth in quantities
  #
  # Args:
  #   q        - baseline quantities for year 1 of price submission 1
  #   p        - baseline prices for year 1 of price submission 1
  #   q_grow   - growth rate in quantities
  #
  # Returns
  #   a series 
  
  ps1.1 <- q * p
  ps1.2 <- ps1.1 * growth_fctr(esc_rate = q_grow, len = 5)
  ps1.3 <- tail(ps1.2, 1)
  nmnl <- c(ps1.2, rep(ps1.3, 15)) 
  nmnl
}





# --------------------------------------------------------------------------------------------------------------------------
# Expand a scenario parameter to one value per price-setting period
# --------------------------------------------------------------------------------------------------------------------------

expand_to_price_periods <- function(x, n_periods, arg_name) {
  
  # Allow a scenario parameter to be given either as a single value (applied to every price-setting period)
  # or as one value per price-setting period.  Scalar recycling keeps existing callers that pass a single
  # value working unchanged.
  #
  # Args:
  #   x          - numeric vector of length 1 or length n_periods
  #   n_periods  - number of price-setting periods (e.g. 4 for FY24-28, FY29-33, FY34-38, FY39-43)
  #   arg_name   - name of the argument, used only in the error message
  #
  # Returns
  #   numeric vector of length n_periods
  
  if (length(x) == 1) {
    return(rep(x, n_periods))
  }
  
  if (length(x) == n_periods) {
    return(x)
  }
  
  stop(
    "'", arg_name, "' must have length 1 or ", n_periods, 
    " (one value per price-setting period), not length ", length(x)
  )
}





# --------------------------------------------------------------------------------------------------------------------------
# Regulatory rate of return (real, vanilla WACC) for each price-setting period
# --------------------------------------------------------------------------------------------------------------------------

rrr_by_price_period <- function(roe, cost_of_debt_nmnl, fcast_infltn, gearing) {
  
  # Build the cost of capital schedule used to set the return on assets in each price-setting period.
  # All inputs are expected to already be one value per period (see expand_to_price_periods), and the
  # calculation is applied element-wise so each period's real cost of debt uses that period's inflation.
  #
  # Args:
  #   roe                - allowed (real) return on equity, one value per period
  #   cost_of_debt_nmnl  - nominal cost of debt, one value per period
  #   fcast_infltn       - forecast inflation, one value per period
  #   gearing            - gearing assumption for the WACC (scalar)
  #
  # Returns
  #   data frame with one row per price-setting period
  
  stopifnot(
    "'roe', 'cost_of_debt_nmnl' and 'fcast_infltn' must be of equal length" = 
      length(roe) == length(cost_of_debt_nmnl) && length(roe) == length(fcast_infltn)
  )
  
  # Fisher conversion of the nominal cost of debt to a real rate
  cost_of_debt_real <- (1 + cost_of_debt_nmnl) / (1 + fcast_infltn) - 1
  
  # Vanilla WACC - rounded to 4 d.p. to match the regulator's published rate of return
  rrr <- round((roe * (1 - gearing) + cost_of_debt_real * gearing), 4)
  
  data.frame(
    period             = seq_along(roe),
    roe                = roe,
    cost_of_debt_nmnl  = cost_of_debt_nmnl,
    fcast_infltn       = fcast_infltn,
    cost_of_debt_real  = cost_of_debt_real,
    rrr                = rrr
  )
}
