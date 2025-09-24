
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
  pdcum        <- exp(cumsum( log(1 + pdvec) )) - 1   # cumulative vector of yearly price changes  
  pnew         <- p0 %*% (1 + pdcum)                  # apply cumulative price changes to p0 tariffs 
  r            <- pnew * q                            # revenue: prices * quantity by tariff
  tot_r        <- colSums(r) / 1e6                    # total revenue by year
  
  # Return desired proportion of fixed and variable tariffs
  f            <- c("Water.Residential.Fixed"   ,"Water.Non-residential.Fixed"   ,"Sewerage.Non-residential.Fixed"   )
  v            <- c("Water.Residential.Variable","Water.Non-residential.Variable","Sewerage.Non-residential.Variable")
  fix_r        <- colSums(r[f, ])                                    # current fixed revenue
  var_r        <- colSums(r[v, ])                                    # current variable revenue
  prop_f       <- sum(fix_r) / (sum(fix_r) + sum(var_r))             # current proportion fixed 
  des_f        <- 0.40                                               # desired proportion fixed
  nfix_r       <- r[f, ] / prop_f * des_f                            # desired fixed revenue  
  nvar_r       <- r[v, ] / (1 - prop_f) * (1 - des_f)                # desired variable revenue
  round(fix_r + var_r - colSums(nfix_r) - colSums(nvar_r), 3)        # check
  npnew        <- pnew                                               # copy prices matrix
  npnew[f,]    <- nfix_r / q[f,]                                     # assign new fixed prices
  npnew[v,]    <- nvar_r / q[v,]                                     # assign new variable prices
  nr           <- npnew * q
  tot_nr       <- colSums(nr) / 1e6
  stopifnot("Total revenue unmatched" = max(abs(round(tot_r, 3) - round(tot_nr, 3))) == 0) # check
  
  npv_tot_r    <- sum(tot_r   / (1 + rrr) ^ (1:length(tot_r))  ) * (1 + rrr) ^ 0.5  # CHANGE THIS TO USE TOT_NR
  npv_rev_req  <- sum(rev_req / (1 + rrr) ^ (1:length(rev_req))) * (1 + rrr) ^ 0.5
  
  obj          <- (npv_rev_req - npv_tot_r) ^ 2
  
  rtn_list     <- list(price_delta = pdvec, prices = npnew)
  
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
trgt_days <- function(mat, days, i, d, trail, bal_acnt, pl_acnt, txn) {
  
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
  bal_pre <- mat[bal_acnt, "open", i] + mat[pl_acnt, txn, i]  # Account balance pre cash transaction
  
  if ( sign(prior_bals) == 1 ) {
    rcpt0 <- min( 
      0, 
      max(
        -bal_pre, 
        round( desired_bal - bal_pre , 3) 
      ) 
    )
  } else {
    rcpt0 <- max( 
      0, 
      min(
        -bal_pre, 
        round( desired_bal - bal_pre , 3) 
      ) 
    )
  }
  
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
# Key performance indicators
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
cash_int_cover_fn <- function(m) {
  mat <- m$txns
  #g <- "^10|^11|^13|^15|^20|^235|^24"
  #eb <- apply(mat, 3, function(x) sum(x[grepl(g, rownames(x)), !colnames(x) %in% c("open","clos")]), simplify = TRUE)  # ebitda_ttm
  op_cf <- apply(mat, 3, function(x) sum(x["3000", c("cshd","exp2","crd1")]), simplify = TRUE)                          # operating cashflows
  ip <- apply(mat, 3, function(x) sum(x["3000", "intp"]), simplify = TRUE)                                              # net_int_pay_ttm 
  return( slide_sum(op_cf, before = 11) / slide_sum(ip, before = 11) )
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
  capex <- apply(mat, 3, function(x) sum(x["3000", "wipc"]), simplify = TRUE)                           # capex
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
  scale_colour_manual(values = c("black", "grey50", "grey30", "grey70", "#d9230f", "#6b1107")) +
  facet_wrap(vars(kpi), scales = "free") + 
  ggthemes::theme_base() +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(
      color      = "grey70",
      linewidth  = 0.25,
      linetype   = 2
      ) 
  ) +
  labs(x = "", y = "")
}



# --------------------------------------------------------------------------------------------------------------------------
# Return trial balance function
# --------------------------------------------------------------------------------------------------------------------------

tb <- function(d, chart, ref) {
  
  # https://shiny.posit.co/r/components/inputs/select-multiple/
  # https://stackoverflow.com/questions/39798042/r-shiny-how-to-use-multiple-inputs-from-selectinput-to-pass-onto-select-optio
  
  # Create a trial balance
  #
  # Args:
  #   d        - a list object returned from the function "f"
  #
  # Returns
  #   a dataframe containing the yearly trial balance
  
  m <- d[[1]]$txns
  #d     <- get_data()
  #chart <- d$chart
  #ref   <- d$ref
  tb1   <- m[ , "clos", (1:20) * 12] / 1000
  round(colSums(tb1), 3)
  
  # Year to date P&L
  tb2 <- tb1
  for (i in 2:20) {
    tb2[as.integer(row.names(tb1)) < 3000, i] <- tb1[as.integer(row.names(tb1)) < 3000, i] - tb1[as.integer(row.names(tb1)) < 3000, i-1]
  }
  round(colSums(tb2), 3)
  
  # Roll P&L to retained earnings
  tb2["5200", ] <- tb2["5200", ] - colSums(tb2)
  
  round(colSums(tb2), 3)
  
  tb2_df <- as.data.frame(tb2)
  tb2_df$account_no <- as.numeric(rownames(tb2_df))
  return(tb2_df)
}





# --------------------------------------------------------------------------------------------------------------------------
# Return rab function
# --------------------------------------------------------------------------------------------------------------------------

rab <- function(d) {
  
  # Return the RAB
  #
  # Args:
  #   d        - a list object returned from the function "f"
  #
  # Returns
  #   a dataframe containing the RAB for each pricing period
  
  r <- d[[1]]$rab
  

  return(data.frame(r))
}



  
  
# --------------------------------------------------------------------------------------------------------------------------
# Plot financials function
# --------------------------------------------------------------------------------------------------------------------------

plot_fins <- function(d, chart, ref, sel) {
  
  # https://shiny.posit.co/r/components/inputs/select-multiple/
  # https://stackoverflow.com/questions/39798042/r-shiny-how-to-use-multiple-inputs-from-selectinput-to-pass-onto-select-optio
  
  # Plot financial statements using kableextra
  #
  # Args:
  #   d        - a list object returned from the function "f"
  #
  # Returns
  #   a table formated P&L and B/S
  
  # sel <- c("FY2024","FY2025","FY2026") for testing
  
  m <- d[[1]]$txns
  #d     <- get_data()
  #chart <- d$chart
  #ref   <- d$ref
  tb1   <- m[ , "clos", (1:20) * 12] / 1000
  round(colSums(tb1), 3)
  
  # Year to date P&L
  tb2 <- tb1
  for (i in 2:20) {
    tb2[as.integer(row.names(tb1)) < 3000, i] <- tb1[as.integer(row.names(tb1)) < 3000, i] - tb1[as.integer(row.names(tb1)) < 3000, i-1]
  }
  round(colSums(tb2), 3)
  
  # Roll P&L to retained earnings
  tb2["5200", ] <- tb2["5200", ] - colSums(tb2)
  
  round(colSums(tb2), 3)
  
  tb2_df <- as.data.frame(tb2)
  tb2_df$account_no <- as.numeric(rownames(tb2_df))
  
  # Income statement data
  inc <- tb2_df %>% 
    left_join(chart[1:6], by = join_by(account_no == account_no)) %>% 
    full_join(ref[ref$ref_type == "account_grp",], by = join_by(account_grp == lookup1)) %>% 
    group_by(ref1, ref2) %>% 
    summarise(across(`12`:`240`, sum)) %>% 
    arrange(ref2) %>% 
    filter(!is.na(ref1)) 
  
  # Balance sheet data
  bal <- tb2_df %>% 
    left_join(chart[1:6], by = join_by(account_no == account_no)) %>% 
    full_join(ref[ref$ref_type == "account_type",], by = join_by(account_type == lookup1)) %>% 
    group_by(ref1, ref2) %>% 
    summarise(across(`12`:`240`, sum)) %>% 
    arrange(ref2) %>% 
    filter(!is.na(ref1)) 
  
  # Cashflow
  cf1 <- list()
  for (i in ((1:20) * 12)) {
    #print(c(i-11, i))
    cf1[[i]] <- rowSums(m["3000", c("cshd","exp2","crd1","wipc","intp","borr"), (i-11):i])
  }
  cf2 <- do.call(rbind, cf1)
  cf2 <- cf2 / 1000
  cf <- data.frame(t(cf2))
  colnames(cf) <- seq(from = 12, by = 12, length.out = 20)  # column names consistent with other tables for rbind
  #colnames(cf) <- paste("FY", 1:20 + 2023, sep = "")
  cf$txn_type <- rownames(cf)
  cf <- cf %>% 
    full_join(ref[ref$ref_type == "cash_flow",], by = join_by(txn_type == lookup2)) %>% 
    group_by(ref1, ref2) %>% 
    summarise(across(`12`:`240`, sum)) %>% 
    arrange(ref2)
  
  op_cash <- m["3000", "open", 1]  # Opening cash
  
  # Join Income statement and Balance sheet
  rep <- as.data.frame(bind_rows(inc, bal, cf)) %>% 
    arrange(ref2) %>% 
    mutate(ref2 = as.character(ref2))
  
  # Insert totals and remove NA's
  rep[rep$ref2 == 8, 3:22]  <- colSums(rep[rep$ref2 %in% 2:7, 3:22], na.rm = T)       # Total revenue from transactions
  rep[rep$ref2 == 17, 3:22] <- colSums(rep[rep$ref2 %in% 11:16, 3:22], na.rm = T)     # Total expenses from transactions
  rep[rep$ref2 == 19, 3:22] <- colSums(rep[rep$ref2 %in% c(8,17), 3:22], na.rm = T)   # Net result from operating transactions
  rep[rep$ref2 == 22, 3:22] <- colSums(rep[rep$ref2 %in% c(19,21), 3:22], na.rm = T)  # Net result from transactions
  rep[rep$ref2 == 28, 3:22] <- colSums(rep[rep$ref2 %in% c(26,27), 3:22], na.rm = T)  # Total assets
  rep[rep$ref2 == 36, 3:22] <- colSums(rep[rep$ref2 %in% c(34,35), 3:22], na.rm = T)  # Total liabilities
  rep[rep$ref2 == 45, 3:22] <- colSums(rep[rep$ref2 %in% c(42,43,44), 3:22], na.rm = T)  # Operating CF
  rep[rep$ref2 == 50, 3:22] <- colSums(rep[rep$ref2 %in% c(48,49), 3:22], na.rm = T)  # Investing CF
  rep[rep$ref2 == 55, 3:22] <- colSums(rep[rep$ref2 %in% c(53,54), 3:22], na.rm = T)  # Operating CF
  rep[rep$ref2 == 57, 3:22] <- colSums(rep[rep$ref2 %in% c(45,50,55), 3:22], na.rm = T)  # Net increase/(decrease) in cash
  rep[rep$ref2 == 58, 3:22] <- c(m["3000", "open", 1], m["3000", "clos", ((1:20) * 12 - 12)[-1]])  # Beginning cash
  rep[rep$ref2 == 59, 3:22] <- colSums(rep[rep$ref2 %in% c(57,58), 3:22], na.rm = T)  # End
  
  
  # Format numbers
  rep[ ,3:22] <- apply(rep[ ,3:22], 2, acc_num)
  rep[is.na(rep)] <- ""
  
  # Rename columns to financial year label
  colnames(rep) <- c("ref1","ref2", paste("FY", as.numeric(colnames(rep[3:22])) / 12 + 2024 - 1, sep = ""))
  
  b <- unique(ref[grepl("b",ref$ref3), "ref2"]) #bold
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
    row_spec(b, bold = TRUE) %>% # c(1,8,10,17,19,22,25,28,33,36,40,44,46,49,51,54,56,58)
    row_spec(i, italic = TRUE) %>%  # c(29,30,31,37,38)
    row_spec(s, extra_css = "border-bottom: 1px solid") %>%  # c(7,16,19,27,35,43,48,53,56)
    row_spec(c(22,38,59), extra_css = "border-bottom: 2px solid") %>%
    row_spec(which(rep$ref1 == "blank"), color = "white") 
  
  #return(list(tbl = rep, tb = tb2_df))
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
  chart    <- read.csv(chart_src, fileEncoding="UTF-8-BOM")
  txn_type <- read.csv(txn_src, fileEncoding="UTF-8-BOM")
  rownames(txn_type) <- txn_type$txn_code
  
  # Capex and opex scenarios.  These represent 
  ps <- c("ps23","ps28","ps33","ps38","ps43")
  
  cx_delta <- data.frame(
    scnr1 = c(0,1061,1205,398,429), 
    scnr2 = c(0,688,690,831,830),
    scnr3 = c(0,980,533,1031,473),
    scnr4 = c(0,1159,1110,551,629),
    row.names = ps
  )
  
  ox_delta <- data.frame(
    scnr1 = c(0,0,34,140,156), 
    scnr2 = c(0,0,28,55,71),
    scnr3 = c(0,0,19,100,131),
    scnr4 = c(0,0,34,148,193),
    row.names = ps
  )
  
  return(list(
    dat          = dat, 
    ref          = ref, 
    chart        = chart, 
    txn_type     = txn_type, 
    cx_delta     = cx_delta, 
    ox_delta     = ox_delta
  ))
}