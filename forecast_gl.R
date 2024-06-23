# ---------------------------------------------------------------------------------------------------------
# Matrix accounting
# ---------------------------------------------------------------------------------------------------------

library(readxl)

dat_src <- "local"   # "local" / "remote"


if (dat_src == "local") {
  chart_src <- "./data/chart.csv"
} else {
  chart_src <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/chart.csv"
}

chart <- read.csv(chart_src, fileEncoding="UTF-8-BOM")

sew <- read_xlsx("./data/SEW.xlsx", range = "sew!A1:F50", col_names = TRUE, col_types = c("numeric","numeric","numeric","text","numeric","numeric"))


# Matrix dimensions

mon <- 1:6                                                                                     # Number of months
txn <- c("opn","age","inc","csh","wof","exp","cpx","dpn","inta","intp","bor","cls")                          # Transaction types
# opn - opening balances
# age - re-allocate debtors ageing at month start
# inc - posting income
# csh - cash receipts from debtors
# wof - debtors write-off
# exp - posting expenses 
# cpx - capex
# dpn - depreciation
# inta - interest (accrue)
# intp - interest (pay)
# bor - borrow if cash balance is negative
# cls - closing balance

act <- c(100,200,250,260,270,300,3051,3052,3053,375,376,400,410,455,500,510)                       # GL accounts
# 100 - income
# 200 - operating expenses
# 250 - depreciation
# 260 - interest
# 270 - bad debts
# 300 - cash
# 305 - debtors
# 375 - assets / ppe
# 376 - acc depn
# 400 - trade debtors
# 410 - accrued interest
# 455 - debt
# 500 - equity
# 510 - retained earnings
opn_bal <- c(0, 0, 0, 0, 0, 18, 10, 5, 3.7, 1895.8, -58.8, -43.7, 0, -450.9, -307.8, -1071.3)     # Opening balances



#dat1 <- read_xlsx("model_data.xlsx", range = "act-opn_bal!A1:B15", col_names = TRUE)
#act <- unlist(dat1[,"act"], use.names = FALSE)
#opn_bal <- unlist(dat1[,"opn_bal"], use.names = FALSE)
#dat2 <- read_xlsx("model_data.xlsx", range = "txn!B2:G5", col_names = FALSE)


# Transaction balances
# https://stackoverflow.com/questions/19340401/convert-a-row-of-a-data-frame-to-a-simple-vector-in-r
income <- c(10.5, 10.5, 10.5, 11, 11, 11)
expenses <- c(9, 9, 9.5, 17, 17, 11)
capex <- c(5, 3, 4, 5, 3, 4)
depn <- c(2, 2, 1.9, 1.9, 1.8, 1.8)
rcpt1_rate <- c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
rcpt2_rate <- c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
rcpt2_rate <- c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8)


# Create matrix and assign names and opening balances
mat <- array(rep(0, length(act) * length(txn) * length(mon)), dim=c(length(act), length(txn), length(mon)))
dimnames(mat)[[1]] <- chart$account_no
dimnames(mat)[[2]] <- txn
dimnames(mat)[[3]] <- mon
mat[,,1][,"opn"] <- opn_bal

# Insert opening balance and rollover
mat[,"opn",1] <- sew$sew_23
mat[,,1]
# Update retained earning
mat["5200","opn",1] <- mat["5200","opn",1] + sum(mat[as.character(chart[chart$account_type %in% c(10,25), ]$account_no), "opn" ,1]) + sum(mat[c("5201","5202"),"opn",1])
mat[as.character(chart[chart$account_type %in% c(10,25), ]$account_no), "opn" ,1] <- 0
mat[c("5201","5202"), "opn" ,1] <- 0
# Update ARR
mat["5100","opn",1] <- mat["5100","opn",1] + sum(mat[c("5101","5102"),"opn",1])
mat["5121","opn",1] <- mat["5121","opn",1] + sum(mat[c("5122","5123"),"opn",1])
mat[c("5101","5102","5122","5123"), "opn" ,1] <- 0
# Check
round(colSums(mat[,,1]), 3)
mat[,,1]

# Transaction loop
for (i in 1:length(mon)) {
  
  # Opening balances & debtors ageing post loop / month 1
  if (i > 1) {
    
    # Opening balances
    mat[,,i][, "opn"] <- mat[,,i-1][, "cls"]
    
    # Apply debtors aging update
    mat[,,i]["3051", "age"] <- -m12 #mat[,,i]["3051", "opn"] - m12
    mat[,,i]["3052", "age"] <- m12  #mat[,,i]["3052", "opn"] + m12
    
    mat[,,i]["3052", "age"] <- -m23 #mat[,,i]["3052", "opn"] - m22
    mat[,,i]["3053", "age"] <- m23  #mat[,,i]["3053", "opn"] + m22
  }
  
  # Post income
  mat["1000","inc",i] <- -income[i]
  mat["3100","inc",i] <- income[i]
  
  # Post cash receipt from aged debtors (TO DO - THIS RESULTS IN NEGATIVE AGED BALANCES)
  rcpt1 <- round(sum(mat[,,i]["3051", c("opn","age")]) * rcpt1_rate[i], 2)
  mat[,,i]["300", "csh"] <- mat[,,i]["300", "csh"] + rcpt1
  mat[,,i]["3051", "csh"] <- -rcpt1
  
  rcpt2 <- round(sum(mat[,,i]["3052", c("opn","age")]) * rcpt1_rate[i], 2)
  mat[,,i]["300", "csh"] <- mat[,,i]["300", "csh"] + rcpt2
  mat[,,i]["3052", "csh"] <- -rcpt2
  
  rcpt3 <- round(sum(mat[,,i]["3053", c("opn","age")]) * rcpt1_rate[i], 2)
  mat[,,i]["300", "csh"] <- mat[,,i]["300", "csh"] + rcpt3
  mat[,,i]["3053", "csh"] <- -rcpt3
  
  # Bad debts WO
  wo <- mat[,,i]["3053", "opn"] + mat[,,i]["3053", "csh"]
  mat[,,i]["3053", "wof"] <- -wo
  mat[,,i]["270", "wof"] <- wo
  
  # Expenses
  mat[,,i]["200", "exp"] <- expenses[i]
  mat[,,i]["300", "exp"] <- -expenses[i]
  
  # Capex
  mat[,,i]["375", "cpx"] <- capex[i] 
  mat[,,i]["300", "cpx"] <- -capex[i]
  
  # Interest (accrue)
  mat[,,i]["260", "inta"] <- -mat[,,i]["455", "opn"] * 0.05 / 12
  mat[,,i]["410", "inta"] <- mat[,,i]["455", "opn"] * 0.05 / 12
  
  # Interest (pay quarterly)
  if (i %in% c(3,6,9,12)) {
    mat[,,i]["410", "intp"] <- -mat[,,i]["410", "opn"]
    mat[,,i]["300", "intp"] <- mat[,,i]["410", "opn"]
    #print(mat[,,i]["410", "opn"])
  }
  
  # Depn
  mat[,,i]["250", "dpn"] <- depn[i]
  mat[,,i]["376", "dpn"] <- -depn[i] 
  
  # Collect data for updating debtors aging (applied after rollover to following period)
  m12 <- sum(mat[,,i]["3051", c("opn","age","csh")])   # DR 3052 / CR 3051
  m23 <- sum(mat[,,i]["3052", c("opn","age","csh")])   # DR 3053 / CR 3052
  
  # Determine if borrowings required
  cash_bal <- sum(mat[,,i]["300",-ncol(mat[,,i])])
  if (cash_bal < 0) {
    mat[,,i]["300", "bor"] <- 20
    mat[,,i]["455", "bor"] <- -20
  }
  
  # Update closing balance
  mat[,,i][, "cls"] <- rowSums(mat[,,i][,-ncol(mat[,,i])])
  
}
mat


# Check balances
round(colSums(mat[,,6]), 3)

# YTD
new_mat <- rowSums(mat, dims = 2)
new_mat[,"opn"] <- mat[,"opn",1]
new_mat[,"cls"] <- mat[,"cls",6]
new_mat
round(colSums(new_mat), 3)

sum(new_mat["3051",c("opn","age")])



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
