
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
# Depreciation
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
# Optimisation / price goal seek
# --------------------------------------------------------------------------------------------------------------------------

npv_optim_func <- function(theta, pdyr, rev_req, p0, q, rtn_mode="obj") {
  
  # Args:
  #   theta   - a numeric vector of length 2 being regulatory rate of return and price delta
  #   pdyr    - an integer between 0 and 5 representing the year in which price delta 2 comes into effect
  #             , a value of zero returns an even price delta for each year 
  #   rev_req - vector of revenue requirement (length 5)
  #   p0      - matrix of initial prices, dimension n * 1 
  #
  # Returns:
  #   error, the difference between two net present values
  
  pdpar        <- theta[-1]  # price delta
  rrr          <- theta[1]   # regulatory rate of return
  
  pdvec        <- rep(pdpar, 5)
  pdvec[(1:5)[-pdyr]] <- 0
  pdcum        <- exp(cumsum( log(1 + pdvec) )) - 1
  pnew         <- p0 %*% (1 + pdcum)
  r            <- pnew * q
  
  tot_r        <- colSums(r) / 1e6
  npv_tot_r    <- sum(tot_r / (1 + rrr) ^ (1:length(tot_r))) * (1 + rrr) ^ 0.5
  npv_rev_req  <- sum(rev_req / (1 + rrr) ^ (1:length(rev_req))) * (1 + rrr) ^ 0.5
  obj          <- (npv_rev_req - npv_tot_r) ^ 2
  
  rtn_list <- list(price_delta = pdvec, prices = pnew)
  #names(rtn_list) <- c("price_delta","prices")
  
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
  depn1 <- rep(round(open_rab_depn, 2), floor(open_rab_rem))
  depn2 <- open_rab_val - sum(depn1)
  depn <- c(depn1, depn2)
  
  return(depn)
}