source("funs.R")


# Depreciation on opening RAB --------------------------------------------
dpn_open <- depn_fun_opn(571.85, 15.92)


# Capex data -------------------------------------------------------------
capex1 <- "
13.07  22.94 	 16.69 	 0.00    0.00
1.14 	 3.18 	 5.20 	 4.76 	 10.88
0.67 	 3.66 	 6.34 	 4.25 	 11.38
0.13 	 0.00    1.32 	 8.60 	 15.21
0.00	 0.12 	 14.94 	 0.69 	 0.00
0.00 	 2.22 	 6.19 	 6.19 	 0.00
0.19 	 6.71 	 6.71 	 0.00 	 0.00
0.00 	 0.00 	 0.43 	 5.50 	 5.50
4.29 	 6.96 	 0.00 	 0.00		 0.00
0.00 	 0.00	 	 1.34 	 3.02 	 4.56
67.0   53.0    45.0    75.0    61.0
"

capex1 <- as.numeric(unlist(strsplit(trimws(capex1), "\\s+")))
capex <- matrix(capex1, ncol=5, byrow=TRUE)
colSums(capex)


# Depreciation on capex --------------------------------------------------
dpn_matrix <- t(mapply(FUN = depn_fun, split(capex, row(capex)), yr_op = c(4,7,7,8,5,8,7,6,3,6,1), life = c(25,60,60,25,45,25,70,50,25,60,50)))
dpn_cpx <- colSums(dpn_matrix)


# Total depreciation -----------------------------------------------------
dpn <- dpn_open[1:5] + dpn_cpx


# Opex -------------------------------------------------------------------
opex <- c(90,89,89,89,89)


# RAB schedule -----------------------------------------------------------
open_rab_val <- 591.85
exist_rab_detail <- matrix(rep(0, 8*5), ncol=5)
rownames(exist_rab_detail) <- c("open","capex","cust_cont","gov_cont","reg_depn","disp","close","average")
colnames(exist_rab_detail) <- c(1:5)
exist_rab_detail["open", 1] <- open_rab_val
exist_rab_detail["capex", ] <- colSums(capex)
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
rev_req <- roa + opex + dpn


# Perform optimisation ---------------------------------------------------
optim(
  
  # Initial values for the parameters to be optimized over
  par = c(0.0255, 0, 0, 0),
  
  # Function to be minimized, first argument being vector of parameters over which minimization is applied
  fn  = npv_optim_func,
  
  method = "L-BFGS-B",
  
  # Upper & lower constraints for parameters
  lower = c(0.0255 - .Machine$double.eps, -0.1, 0-.Machine$double.eps, 0-.Machine$double.eps),
  upper = c(0.0255 + .Machine$double.eps,  0.1, 0+.Machine$double.eps, 0+.Machine$double.eps),
  
  # ... Further arguments to be passed to fn
  pdyr    = 1,
  rev_req = rev_req,
  p0      = as.matrix(10),
  q       = c(100,102,104,106,108)
  
)