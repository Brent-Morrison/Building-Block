
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

npv_optim_func <- function(theta, pdyr, single, rev_req, p0, q, rtn_mode="obj") {
  
  # Args:
  #   theta   - a numeric vector of length 2 being regulatory rate of return and price delta
  #   pdyr    - an integer between 0 and 5 representing the year in which price delta 2 comes into effect
  #             , a value of zero returns an even price delta for each year 
  #   single  - logical, if true the only price delta (that of pdpar1) occurs
  #   rev_req - vector of revenue requirement (length 5)
  #   p0      - matrix of initial prices, dimension n * 1 
  #   q       - 
  #
  # Returns:
  #   error, the difference between two net present values
  
  pdpar1 <- theta[2]  # First price delta   
  pdpar2 <- theta[3]  # Second price delta
  rrr    <- theta[1]  # regulatory rate of return
  
  pdpar2 <- if (pdyr == 1) pdpar1 else pdpar2
  pdvec  <- c(rep(pdpar1, pdyr-1), rep(pdpar2, 5-pdyr+1))
  pdvecT <- rep(pdpar1, 5)
  pdvecT[(1:5)[-pdyr]] <- 0
  pdvec  <- if (single == T) pdvecT else pdvec
  
  pdcum        <- exp(cumsum( log(1 + pdvec) )) - 1
  pnew         <- p0 %*% (1 + pdcum)
  r            <- pnew * q
  
  tot_r        <- colSums(r) / 1e6
  npv_tot_r    <- sum(tot_r / (1 + rrr) ^ (1:length(tot_r))) * (1 + rrr) ^ 0.5
  npv_rev_req  <- sum(rev_req / (1 + rrr) ^ (1:length(rev_req))) * (1 + rrr) ^ 0.5
  obj          <- (npv_rev_req - npv_tot_r) ^ 2
  
  rtn_list <- list(price_delta = pdvec, prices = pnew)
  
  ifelse(rtn_mode == "obj", return(obj), return(rtn_list))
  
}





# --------------------------------------------------------------------------------------------------------------------------
# ROU / lease schedule
# --------------------------------------------------------------------------------------------------------------------------

rou_func <- function(lease_pay, periods, rate) {
  
  # Create a leease schedule detailing repayment, interest component & depreciation
  #
  # Args:
  #   lease_pay - the lease payment
  #   periods   - number of periods in the lease contract
  #   rate      - discount rate to be used in calculating the right of use asset / lease liability
  #
  # Returns:
  #   matrix being
  
  mat <- matrix(rep(0, periods * 7), nrow = periods)
  dimnames(mat)[[1]] <- 1:periods
  dimnames(mat)[[2]] <- c("open_liab","payment","interest","clos_liab","open_depn","depn_exp","clos_depn")
  pays <- rep(lease_pay, periods)
  
  for (i in 1:length(pays)) {
    pays_ <- pays[i:length(pays)]
    oll <- sum(pays_ / (1 + rate / 12) ^ (1:length(pays_)))
    mat[i,"open_liab"] <- -round(oll,2)
    mat[i,"payment"]   <- round(lease_pay,2)
    mat[i,"interest"]  <- round(mat[i,"open_liab"] * rate / 12 , 2)
    mat[i,"clos_liab"] <- sum(mat[i,1:3])
    mat[i,"open_depn"] <- ifelse(i == 1, 0, mat[i - 1, "clos_depn"])
    mat[i,"depn_exp"]  <- round(mat[1, "open_liab"] / periods, 2)
    mat[i,"clos_depn"] <- sum(mat[i,5:6])
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
  txn_type_parm = txn_df
  dc <- as.character(na.omit(unname(unlist(txn_type_parm[txn, c("dr", "cr","dr1", "cr1","dr2", "cr2","dr3", "cr3","dr4", "cr4","dr5", "cr5")]))))
  return(dc)
}

#drcr(txn = "incm", txn_df = txn_type)



# --------------------------------------------------------------------------------------------------------------------------
# Target cash payment re creditors / debtors based on debtor (creditors) days
# --------------------------------------------------------------------------------------------------------------------------
trgt_days <- function(i, d, trail, bal_acnt, pl_acnt, txn) {
  #
  # Return the cash receipt / payment to arrive at balance required for designated debtors / creditors days
  # - assumes presence of matrix "mat" in environment
  #
  # Args:
  #   i        - the iteration of the loop passing over "mat"
  #   d        - the target debtors (creditors) days, integer
  #   trail    - number of period to calculate over, integer
  #   bal_acnt - debtors / creditors account
  #   pl_acnt  - P&L account accruing to debtors / creditors account
  #   txn      - the transaction type 
  #
  # Returns
  #   the cash payment / receipt to arrive at balance required for designated debtors / creditors days 
  
  if (i < trail) s <- 1 else s <- i - (trail - 1)
  trail_exp <- -mean(mat[pl_acnt, txn, s:i]) * trail
  sum_days <- mean(days[s:i]) * trail
  prior_bals <- mean(mat[bal_acnt, "clos", s:(i-1)]) * (trail-1)
  desired_bal <- d * trail_exp / sum_days * trail - prior_bals
  
  # Check if the desired balance is the same sign as the natural balance associated with the account type
  # (asset-DR / liability-CR)
  if ( as.integer(substr(bal_acnt, 1, 1)) <= 3 ) exp_sign <- 1 else exp_sign <- -1
  if (sign(desired_bal) == exp_sign) desired_bal <- desired_bal else desired_bal <- mat[bal_acnt, "open", i]
  
  bal_pre <- mat[bal_acnt, "open", i] + mat[pl_acnt, txn, i]  # Account balance pre cash transaction
  
  rcpt0 <- min( 
    0, 
    max(
      -bal_pre, 
      round( desired_bal - bal_pre , 3) 
      ) 
    )
  
  if (i < trail) rcpt <- -mat[bal_acnt, txn, i] else rcpt <- rcpt0
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
