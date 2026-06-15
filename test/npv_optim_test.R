# Create data
df <- data.frame(
  tariff = c(
    "Water.Rural.Fixed",
    "Water.Rural.Variable",
    "Sewerage.Non-residential.Fixed",
    "Sewerage.Non-residential.Variable",
    "Trade-Waste.Non-residential.Fixed",
    "Trade-Waste.Non-residential.Variable",
    "Water.Non-residential.Fixed",
    "Water.Non-residential.Variable",
    "Water.Residential.Fixed",
    "Water.Residential.Variable"
  ),
  p0 = c(
    180.1002,
    0.2653,
    696.4600,
    0.9926,
    163.1300,
    0.6565,
    414.4120,
    2.2860,
    234.4724,
    2.2860
  ),
  q0 = c(
    14213,
    4443792,
    6004,
    1386924,
    1463,
    3281205,
    7712,
    6627518,
    74383,
    14275036
  ),
  stringsAsFactors = FALSE
)

df

# Quantities (period 0)
q0 <- as.matrix(df[, "q0"])
rownames(q0) <- df$tariff
q0


# Prices (period 0)
p0 <- as.matrix(df[, "p0"])
rownames(p0) <- df$tariff
p0


# Quantities (period 1 - 5)
q <- q0 %*% cumprod(1 + rep(0, 5))
q


# Price delta matrix
pd <- q
pd[,] <- 0.0409954
pd
#pd[grep("Fixed", rownames(pd)), ] <- 0.005



# Price delta matrix (cumulative)
pdcum <- t(apply(pd, 1, function(x) cumprod(1 + x)))


# Prices matrix
p <- pdcum * p0[, 1]


r <- p * q
r

tot_r <- colSums(r)
tot_r

rrr <- 0.05
npv_tot_r <- sum(tot_r   / (1 + rrr) ^ (1:length(tot_r))  ) * (1 + rrr) ^ 0.5
npv_tot_r

rev_req <- c(83393000, 86812000, 90371000, 94076000, 97932000)
npv_rev_req <- sum(rev_req   / (1 + rrr) ^ (1:length(rev_req))  ) * (1 + rrr) ^ 0.5
npv_rev_req


# --------------------------------------------------------------------------------------------------------------------------
# Optimisation / price goal seek
# --------------------------------------------------------------------------------------------------------------------------

npv_optim_func1 <- function(theta, rev_req, p0, q, rtn_mode="obj") {
  
  # Net Present Value Optimisation Objective Function
  #
  # Computes the squared difference between the net present value (NPV) of expected revenue 
  # (based on price changes and quantities) and the regulatory revenue requirement. 
  # This function is typically used as an objective function in optimisation routines 
  # (e.g., to determine optimal price paths over a 5-year regulatory period).
  #
  # Args:
  #   theta    - a numeric vector of length 3 being regulatory rate of return, price delta 1 (used for specific cohort
  #              of tariffs), and price delta 2 (used for remaining tariffs)
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
  
  
  pdpar1 <- theta[2]  # First price delta   
  pdpar2 <- theta[3]  # Second price delta
  rrr    <- theta[1]  # Regulatory rate of return
  
  # Price delta matrix
  pd <- q
  pd[,] <- pdpar1 # 0.0409954
  pd[grep("Sewerage", rownames(pd)), ] <- pdpar2
  pd[grep("Residential", rownames(pd)), ] <- 0.035  # Fixed
  
  # TO DO - insert a parameter so that the proportion of income from usage vs service charges can be flexed
  # see below 
  
  # Price delta matrix (cumulative)
  pdcum        <- t(apply(pd, 1, function(x) cumprod(1 + x)))
  pnew         <- pdcum * p0[, 1]                     # new prices, apply cumulative price deltas to p0 tariffs 
  r            <- pnew * q                            # revenue: prices * quantity by tariff
  tot_r        <- colSums(r)                          # total revenue by year
  
  prop_pre <- sum(r[grep("Sewerage", rownames(r)), ]) / sum(r)
  prop_tgt <- 0.05
  prop_penalty <- (prop_pre - prop_tgt) ^ 2
  
  npv_tot_r    <- sum(tot_r   / (1 + rrr) ^ (1:length(tot_r))  ) * (1 + rrr) ^ 0.5  
  npv_rev_req  <- sum(rev_req / (1 + rrr) ^ (1:length(rev_req))) * (1 + rrr) ^ 0.5
  
  par_penalty  <- (pdpar1 - pdpar2) ^ 2
  rev_penalty  <- ((npv_rev_req - npv_tot_r) / npv_rev_req) ^ 2
  obj          <- rev_penalty + prop_penalty #+ par_penalty
  
  rtn_list     <- list(price_delta = pd, prices = pnew)
  
  ifelse(rtn_mode == "obj", return(obj), return(rtn_list))
  
}



optim_result <- optim(
  
  # Initial values for the parameters to be optimized over
  par = c(0.05, 0.05, 0.05),
  
  # Function to be minimized, first argument being vector of 
  # parameters over which minimization is applied
  fn  = npv_optim_func1,
  
  method = "L-BFGS-B",
  
  # Upper & lower constraints for parameters
  lower = c(rrr - .Machine$double.eps, -0.1, -0.1),
  upper = c(rrr + .Machine$double.eps,  0.1,  0.1),
  
  # ... Further arguments to be passed to fn
  rev_req = rev_req, #rev_req[i:(i+4)],
  p0      = p0,
  q       = q #q[,i:(i+4)]
  
)


# Recover price delta and prices using optimised parameters
res <- npv_optim_func1(
  theta      = optim_result$par, 
  rev_req    = rev_req, 
  p0         = p0, 
  q          = q, 
  rtn_mode   = "data"
)
res$price_delta
res$prices


# Check results
r_new <- res$prices * q
tot_rev_real <- colSums(r_new)
sum(rev_req / (1 + rrr) ^ (1:length(rev_req)))             # NPV of revenue requirement
sum(tot_rev_real / (1 + rrr) ^ (1:length(tot_rev_real)))   # NPV of revenue


sum(r_new[grep("Sewerage", rownames(r_new)), ]) / sum(r_new)