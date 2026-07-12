
# --------------------------------------------------------------------------------------------------------------------------
# Test days receivable logic
# --------------------------------------------------------------------------------------------------------------------------

source("C:/Users/brent/Documents/repos/Building-Block/R/funs.R") # Contains trgt_days()

Rcpp::sourceCpp("R/trgt_days_cpp.cpp", rebuild = TRUE)
trgt_days_fast <- function(mat, days, i, d, trail, bal_acnt, pl_acnt, txn, open, clos) {
  
  trgt_days_cpp(
    mat = as.numeric(mat),
    dims = dim(mat),
    days = days,
    i = i,
    d = d,
    trail = trail,
    bal_i = bal_acnt,
    pl_i = pl_acnt,
    txn_i = txn,
    open_i = open,
    clos_i = clos
  )
}

txn_type <- get_data()$txn_type



# Create matrix and assign names 
days = c(31,30,31,31,30,31,31)
act <- c(1000,1001,1002,2000,2100,3000,3050,3100,3645,4000,4010)
txn <- c("open","aidb","incm","cshd","crd1","exp1","exp3","clos")
mon <- 1:7
mat <- array(rep(0, length(act) * length(txn) * length(mon)), dim=c(length(act), length(txn), length(mon)))
dimnames(mat)[[1]] <- act
dimnames(mat)[[2]] <- txn
dimnames(mat)[[3]] <- mon


# Insert data (COPY FROM EXCEL)
days <- c(31, 30, 31, 31, 30, 31, 31)		
mat['3050','open', 1:5] <- c(20346, 20346, 20346, 20346, 20346)		
mat['3050','aidb', 1:7] <- c(7717, 7717, 7717, 7717, 7717, 7717, 7717)	;	mat['1000','aidb', 1:7] <- c(-7717, -7717, -7717, -7717, -7717, -7717, -7717)
mat['3100','incm', 1:5] <- c(7717, 7717, 7717, 7717, 7717)	;	mat['3050','incm', 1:5] <- c(-7717, -7717, -7717, -7717, -7717)
mat['3050','clos', 1:5] <- c(20346, 20346, 20346, 20346, 20346)		
mat['3100','open', 1:5] <- c(20346, 20346, 20346, 20346, 20346)		
# PERFORMED ABOVE		
mat['3000','cshd', 1:5] <- c(-7717, -7717, -7717, -7717, -7717)	;	mat['3100','cshd', 1:5] <- c(7717, 7717, 7717, 7717, 7717)
mat['3100','clos', 1:5] <- c(20346, 20346, 20346, 20346, 20346)		


# Convert indices to integers
acct_idx <- setNames(seq_along(dimnames(mat)[[1]]), dimnames(mat)[[1]])
txn_idx  <- setNames(seq_along(dimnames(mat)[[2]]), dimnames(mat)[[2]])

idx <- list(
  aidb = list( # "incm"
    bal_acnt = acct_idx["3050"],
    pl_acnt  = acct_idx[c("1000","1001","1002")],
    txn      = txn_idx["aidb"]
  ),
  incm = list( # "cshd"
    bal_acnt = acct_idx["3100"],
    pl_acnt  = acct_idx["3050"],
    txn      = txn_idx["incm"]
  ),
  expx = list( # "crd1"
    bal_acnt = acct_idx["4000"],
    pl_acnt  = acct_idx["2000"],
    txn      = txn_idx[c("exp1","exp3")]
  ),
  cpx1 = list( # "wipc"
    bal_acnt = acct_idx["4010"],
    pl_acnt  = acct_idx["3645"],
    txn      = txn_idx["cpx1"]
  ),
  open = txn_idx["open"],
  clos = txn_idx["clos"]
)



# Run test
for (i in 6:7) {
  if (i > 1) mat[, "open", i] <- mat[, "clos", i-1]

  # Test here
  # Transfer to debtors to specify desired closing balance for accrued income days parameter -----------------------------
  trail <- 3
  t <- "incm"
  tfer <- trgt_days_fast(mat, days, i, d = 81, trail, bal_acnt=idx$aidb$bal_acnt, pl_acnt=idx$aidb$pl_acnt, txn=idx$aidb$txn, open=idx$open, clos=idx$clos) 
  mat[drcr(t, txn_type), t, i] <- c(-tfer, tfer)
  
  trail <- 3
  t <- "cshd"
  rcpt <- trgt_days_fast(mat, days, i, d = 75, trail, bal_acnt=idx$incm$bal_acnt, pl_acnt=idx$incm$pl_acnt, txn=idx$incm$txn, open=idx$open, clos=idx$clos)
  mat[drcr(t, txn_type), t, i] <- c(-rcpt, rcpt)
  
  # Closing balances
  mat[, "clos", i] <- rowSums(mat[,-ncol(mat[,,i]), i])
}
mat



# # Excel version
# library(openxlsx)
# wb <- createWorkbook()
# addWorksheet(wb, "debtors_days")
# writeData(wb, sheet = "debtors_days", x = debtors1, startCol = 2, startRow = 2)
# writeData(wb, sheet = "debtors_days", x = 60, startCol = 4, startRow = 1)
# writeData(wb, sheet = "debtors_days", x = debtors2, startCol = 2, startRow = 11)
# writeData(wb, sheet = "debtors_days", x = 60, startCol = 4, startRow = 10)
# writeFormula(wb, 1, x = "MIN(0, MAX(-SUM(D4:D5),ROUND(D1*SUM(B5:D5)/SUM(B3:D3)*3-SUM(C4:D4)-SUM(D4:D5),0)))", startCol = 4, startRow = 6)
# writeFormula(wb, 1, x = "MIN(0, MAX(-SUM(D13:D14),ROUND(D10*SUM(B14:D14)/SUM(B12:D12)*3-SUM(C13:D13)-SUM(D13:D14),0)))", startCol = 4, startRow = 15)
# writeFormula(wb, 1, x = "SUM(D4:D6)", startCol = 4, startRow = 7)
# writeFormula(wb, 1, x = "SUM(D13:D15)", startCol = 4, startRow = 16)
# writeFormula(wb, 1, x = "ROUND(AVERAGE(B7:D7)/SUM(B5:D5)*SUM(B3:D3), 0)", startCol = 4, startRow = 8)
# writeFormula(wb, 1, x = "ROUND(AVERAGE(B16:D16)/SUM(B14:D14)*SUM(B12:D12), 0)", startCol = 4, startRow = 17)
# saveWorkbook(wb, "./test/debtors_days_test1.xlsx", overwrite = TRUE)
