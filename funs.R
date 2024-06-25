
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

# path <- "SEW_2023 Price Review Model - 2022-09-09 - DEPN.xlsm"
# capex <- read_xlsx("./test/SEW_2023 Price Review Model - 2022-09-09 - DEPN.xlsm", range = "Capex_FO input!Q5:Z14", col_names = FALSE)
# c <- as.matrix(capex)
# c1 <- c[1,]
# c4 <- c[4,]
# c7 <- c[7,]
# c6 <- c(5,5,5,0,0,0,0,0,0,0)
# yr_op1 <- 7
# yr_op4 <- 5
# yr_op7 <- 1
# yr_op6 <- 3
# l1 <- 50
# l4 <- 50
# l7 <- 3
# l6 <- 3

# Test function
# capex <- c[10,]
# yr_op <- 4
# life <- 3

reg_depn <- function(capex, yr_op, life) {
  
  ac <- rep(0,length(capex))
  ind <- 1:length(capex)
  
  if (yr_op == 1) {
    ac <- capex
  } else {
    ac[ind <  yr_op] <- 0
    ac[ind == yr_op] <- sum(capex[ind <= yr_op])
    ac[ind >  yr_op] <- capex[ind >  yr_op]
    
  }
  
  cpx.m <- diag(as.vector(ac)) + diag(rep(1e-9, length(ac)))
  yr1.dpn <- cpx.m / life * 0.5
  yr2p.dpn <- cpx.m
  for (i in 1:ncol(yr2p.dpn)) {
    yr2p.dpn[i,][yr2p.dpn[i,] == 0] <- rep(diag(yr2p.dpn)[i] / life, ncol(cpx.m) - 1)
  }
  yr2p.dpn[lower.tri(yr2p.dpn, diag = TRUE)] <- 0
  dpn <- yr1.dpn + yr2p.dpn
  
  # Filter out later years here
  for (i in 1:ncol(dpn)) {
    if (i + life <= ncol(dpn)) {
      dpn[i,][i + life]         <- dpn[i,][i + life] * 0.5
      dpn[i,][ind > (i + life)] <- 0
    }
  }
  
  dpn <- colSums(dpn)
  
  return(round(dpn, 4))
}

# reg_depn(capex, yr_op, life)
# 
# dpn_mtrix <- t(mapply(FUN = reg_depn, split(c, row(c)), yr_op = c(6,3,7,4,4,5,1,5,9,4), life = c(30,50,50,80,50,50,3,50,50,3)))
# dpn_mtrix
# 
# colSums(dpn_mtrix)





# --------------------------------------------------------------------------------------------------------------------------
# Optimisation / price goal seek
# --------------------------------------------------------------------------------------------------------------------------

npv_optim_func <- function(theta, pdyr, rev_req, p0, q) {
  
  # https://r-pkgs.org/man.html
  # theta   - a numeric vector of length 3, regulatory rate of return, price delta 1, price delta 2
  # pdyr    - an integer between 1 and 5 representing the year in which price delta 2 comes into effect
  # rev_req - vector of revenue requirement
  
  pdpar        <- theta[-1]
  rrr          <- theta[1]
  
  pdvec        <- c(rep(pdpar[1], pdyr - 1), rep(pdpar[2], 5 - pdyr + 1))
  pd           <- exp(cumsum( log(1 + pdvec) )) - 1
  pnew         <- p0 %*% (1 + pd)
  r            <- pnew * q
  
  tot_r        <- colSums(r) / 1e6
  npv_tot_r    <- sum(tot_r / (1 + rrr) ^ (1:length(tot_r)))* (1 + rrr) ^ 0.5
  npv_rev_req  <- sum(rev_req / (1 + rrr) ^ (1:length(rev_req)))* (1 + rrr) ^ 0.5
  obj          <- (npv_rev_req - npv_tot_r) ^ 2
  
  return(obj)
  
}
