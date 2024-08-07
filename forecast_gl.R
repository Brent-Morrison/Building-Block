# ---------------------------------------------------------------------------------------------------------
# Matrix accounting
# ---------------------------------------------------------------------------------------------------------

library(readxl)

dat_src <- "local"   # "local" / "remote"


if (dat_src == "local") {
  chart_src   <- "./data/chart.csv"
  txn_src     <- "./data/txn_type.csv"
  bals_src    <- "./data/sew_bals.csv"
  txn_dat_src <- "./data/sew_txns.csv"
} else {
  chart_src   <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/chart.csv"
  txn_src     <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/txn_type.csv"
  bals_src    <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/sew_bals.csv"
  txn_dat_src <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/sew_txns.csv"
}

chart    <- read.csv(chart_src, fileEncoding="UTF-8-BOM")
txn_type <- read.csv(txn_src, fileEncoding="UTF-8-BOM")
rownames(txn_type) <- txn_type$txn_code
sew_bals <- read.csv(bals_src, fileEncoding="UTF-8-BOM")
sew_txns <- read.csv(txn_dat_src, fileEncoding="UTF-8-BOM")
sew_txns$month <- as.Date(sew_txns$month)

#sew_bals <- read_xlsx("./data/SEW.xlsx", range = "bals!A1:D52", col_names = TRUE, col_types = c("numeric","text","numeric","numeric"))
#sew_txns <- read_xlsx("./data/SEW.xlsx", range = "txns!A1:P25", col_names = TRUE, col_types = c("date",rep("numeric", 15)))


# Matrix dimensions
mon <- 1:12   # Number of months
txn <- unlist(txn_type[,"txn_code"], use.names = FALSE)  # Transaction types
act <- unlist(chart[,"account_no"], use.names = FALSE)  # GL accounts


# Transaction balances
# https://stackoverflow.com/questions/19340401/convert-a-row-of-a-data-frame-to-a-simple-vector-in-r
incm <- unlist(sew_txns[,"incm"], use.names = FALSE)
exp1 <- unlist(sew_txns[,"exp1"], use.names = FALSE)
cpx1 <- unlist(sew_txns[,"cpx1"], use.names = FALSE)
dpn1 <- unlist(sew_txns[,"dpn1"], use.names = FALSE)

# Opening balances
opn_bal <- merge(x = chart, y = sew_bals, by = "account_no", all.x = TRUE)
opn_bal[is.na(opn_bal)] <- 0
opn_bal <- unlist(opn_bal[,"sew_23"], use.names = FALSE)

# Create matrix and assign names and opening balances
mat <- array(rep(0, length(act) * length(txn) * length(mon)), dim=c(length(act), length(txn), length(mon)))
dimnames(mat)[[1]] <- act  #chart$account_no
dimnames(mat)[[2]] <- txn
dimnames(mat)[[3]] <- mon
mat[ , "open", 1] <- opn_bal


# Rollover and update retained earning
mat["5200","open",1] <- mat["5200","open",1] + sum(mat[as.character(chart[chart$account_type %in% c(10,25), ]$account_no), "open" ,1]) + sum(mat[c("5201","5202"),"open",1])
mat[as.character(chart[chart$account_type %in% c(10,25), ]$account_no), "open" ,1] <- 0
mat[c("5201","5202"), "open" ,1] <- 0
# Update ARR (note 2 groups of accounts)
mat["5100","open",1] <- mat["5100","open",1] + sum(mat[c("5101","5102"),"open",1])
mat["5121","open",1] <- mat["5121","open",1] + sum(mat[c("5122","5123"),"open",1])
mat[c("5101","5102","5122","5123"), "open" ,1] <- 0
# Check
round(colSums(mat[,,1]), 3)
mat[,,1]

# Transaction loop
for (i in 1:length(mon)) {
  
  # Opening balances & debtors ageing post loop / month 1
  if (i > 1) {
   
    # Opening balances
    mat[, "open", i] <- mat[, "clos", i-1]

  }
  
  # Post income
  # as.character(unname(unlist(txn_type["incm", c("dr", "cr")])))
  mat[c("3100", "1000"), "incm", i] <- c(incm[i], -incm[i])
  
  # Cash receipt to specify desired closing balance for debtors days parameter 
  trail <- 3
  days <- as.numeric(format(as.Date(sew_txns$month), "%d"))
  if (i < trail) s <- 1 else s <- i - (trail - 1)
  trail_inc <- -mean(mat["1000", "incm", s:i]) * trail
  sum_days <- mean(days[s:i]) * trail
  prior_bals <- mean(mat["3100", "open", (s+1):i]) * (trail - 1)
  rcpt <- abs(40 * trail_inc / sum_days * 3 - prior_bals - mat["3100", "open", i] + mat["1000", "incm", i])
  
  mat[c("3000", "3100"), "cshd", i] <- c(rcpt, -rcpt)
  
  # Bad debts WO
  # wo <- mat[,,i]["3053", "opn"] + mat[,,i]["3053", "csh"]
  # mat[,,i]["3053", "wof"] <- -wo
  # mat[,,i]["270", "wof"] <- wo
  
  # Expenses
  mat[c("2000", "3000"), "exp1", i] <- c(exp1[i], -exp1[i])
  
  # Capex
  mat[c("3500", "3000"), "cpx1", i] <- c(cpx1[i], -cpx1[i])
  
  # Interest (accrue)
  int <- mat["4500", "open", i] * 0.05 / 12
  mat[c("2300", "4020"), "inta", i] <- c(-int, int)
  
  # Interest (pay quarterly)
  if (i %in% c(3,6,9,12)) {
    mat["4020", "intp", i] <- -mat["4020", "open", i]
    mat["3000", "intp", i] <- mat["4020", "open", i]
  }
  
  # Depn
  mat[c("2200", "3505"), "dpn1", i] <- c(dpn1[i], -dpn1[i])
  
  # Determine if borrowings required
  cash_bal <- sum(mat["3000",-ncol(mat[,,i]), i])
  if (cash_bal < 0) {
    mat[c("3000", "4100"), "borr", i] <- c(10000,-10000)
  }
  
  # Update closing balance
  mat[, "clos", i] <- rowSums(mat[,-ncol(mat[,,i]), i])
  
}
mat[,,1]
t(mat["3100",,])


# Check balances
round(colSums(mat[,,6]), 3)



# ------------------------------------------------------------
# Stack 3 dim array and unpivot


mat1 <- mat
dim(mat1) <- c(length(act)*length(txn)*length(mon), 1)
mat1

df <- expand.grid(act = act, txn = txn, mon = mon)
df$mtd <- mat1[,1]

# Cumulative sum
df1 <- df[df$txn %in% txn[-c(1, length(txn))], ]
df1$yr <- round(df1$mon / 12, 0) + 1
df1$ltd <- ave(df1$mtd, df1$act, df1$txn, FUN=cumsum)
df1$ytd <- ave(df1$mtd, df1$act, df1$txn, df1$yr, FUN=cumsum)











# Create YTD matrix
new_mat <- rowSums(mat, dims = 2)
new_mat[ , "open"] <- mat[ , "open", 1]
new_mat[ , "clos"] <- mat[ , "clos", 6]
new_mat
round(colSums(new_mat), 3)



tbl <- matrix(mat, prod(dim(mat)[1:2]), dim(mat)[3])



vars1 <- c(1,2,3)
vars2 <- c(10,20,30)
vars3 <- c(4,5,6)
mult_one <- function(var1, var2, var3)
{
  var1*var2*var3
}
mapply(mult_one, vars1, vars2, vars3)


v1 <- c(1:9)
v2 <- c(1,1,1,1,2,2,2,2,2)
v1[v2 == 1]
v1[v2 == 2]
chart[chart$account_grp == 100, ]$account_no
as.character(chart[chart$account_grp == 100, ]$account_no)
mat[,,1]["3000",]
mat[,,1][as.character(chart[chart$account_grp == 100, ]$account_no),]
sum(mat[as.character(chart[chart$account_type %in% c(10,25), ]$account_no), "opn" ,1])
