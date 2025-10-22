
# --------------------------------------------------------------------------------------------------------------------------
# Test function reading table
# https://www.esc.vic.gov.au/sites/default/files/documents/SEW_2023%20Price%20Review%20Model%20-%202022-09-09%20-%20SUBMISSION%20FINAL.xlsm
# --------------------------------------------------------------------------------------------------------------------------

source("funs.R")
library(readxl)
path <- "./data/SEW_2023 Price Review Model - 2022-09-09 - SUBMISSION FINAL.xlsm"

capex1 <- read_xlsx(path, range = "Capex_FO input!C41:M129", col_names = TRUE)
names(capex1) <- gsub("-", "_", names(capex1))
names(capex1) <- gsub(" ", "_", names(capex1))
names(capex1) <- gsub("\\(", "", names(capex1))
names(capex1) <- gsub("\\)", "", names(capex1))
names(capex1) <- tolower(gsub("/", "_", names(capex1)))

capex2 <- read_xlsx(path, range = "Capex_FO input!Q42:Z129", col_names = FALSE)
cols_date <- read_xlsx(path, range = "Capex_FO input!Q39:Z39", col_names = FALSE)
names(capex2) <- t(cols_date)



# --------------------------------------------------------------------------------------------------------------------------
# Test "npv_optim_func" function
# --------------------------------------------------------------------------------------------------------------------------

source("funs.R")
library(readxl)
library(dplyr)
path <- "./test/optim_eg.xlsx"

# Price, quantity, revenue data
pqr <- read_xlsx(path, range = "optim!A13:L94", col_names = TRUE)

# Quantities
q <- as.matrix(pqr %>% filter(PQR == 'Qty') %>% select(`2023-24`:`2027-28`)) #select(c(PQR:Unit,`2023-24`:`2027-28`))
q[is.na(q)] <- 0

# Prices
p <- as.matrix(pqr %>% filter(PQR == 'Price') %>% select(`2023-24`:`2027-28`)) #select(c(PQR:Unit,`2023-24`:`2027-28`))
p[is.na(p)] <- 0

# Revenue
r <- p * q

tariff_revenue <- colSums(r) / 1e6
tariff_revenue
sum(tariff_revenue)  # should sum to 4,377.645


# Get period zero prices
p0 <- as.matrix(pqr %>% filter(PQR == 'Price') %>% select(`2022-23`)) 
p0[is.na(p0)] <- 0


pdyr <- 1                                                                                       # Price delta year
pdpar <- c(0.059, 0, 0)                                                                         # Price delta to optimise (parameter)
# Vector of price changes
#pdvec <- c(rep(pdpar[1], if (pdyr == 1) 1 else pdyr - 1), pdpar[2], rep(pdpar[3], (if (pdyr == 1) 4 else 5) - pdyr))
pdvec <- rep(pdpar, 5)
pdvec[(1:5)[-pdyr]] <- 0
pdcum <- exp( cumsum( log( 1 + pdvec ) ) ) - 1                                                  # Convert to cumulative change for matrix multiplication
pnew <- p0 %*% (1 + pdcum)

r <- pnew * q
tariff_revenue <- colSums(r) / 1e6
tariff_revenue
sum(tariff_revenue)

#p0 <- matrix(rep(10,10), ncol = 1)
#q <- matrix(rep(10,10*5), ncol = 5)


# Test function ---------------------------------
theta <- c(0.0255, 0)
npv_optim_func(theta, pdyr=0, rev_req=c(944.2496,923.408,921.6224,917.4634,926.2756), p0, q)


# Perform optimisation --------------------------
res <-optim(
  
  # Initial values for the parameters to be optimized over
  par = c(0, 0),
  
  # Function to be minimized, first argument being vector of parameters over which minimization is applied
  fn  = npv_optim_func,
  
  method = "L-BFGS-B",
  
  # Upper & lower constraints for parameters
  lower = c(0.0255 - .Machine$double.eps, -0.5),
  upper = c(0.0255 + .Machine$double.eps,  0.5),
  
  # ... Further arguments to be passed to fn
  pdyr    = 0,
  rev_req = c(944.2496,923.408,921.6224,917.4634,926.2756),
  p0      = p0,
  q       = q
  
)

res

res_list <- npv_optim_func(theta=res$par, pdyr=0, rev_req=c(944.2496,923.408,921.6224,917.4634,926.2756), p0=p0, q=q, rtn="xxx")
res_list$price_delta
res_list$prices

sum(res_list$prices * q) / 1e6



# --------------------------------------------------------------------------------------------------------------------------
# Test "depn_fun" function
# --------------------------------------------------------------------------------------------------------------------------

source("funs.R")
library(readxl)

# Extract capex data
capex <- read_xlsx("./test/SEW_2023 Price Review Model - 2022-09-09 - DEPN.xlsm", range = "Capex_FO input!Q5:Z14", col_names = FALSE)
c <- as.matrix(capex)

# Extract depreciation life and year operational data 
life_yr_op <- read_xlsx("./test/SEW_2023 Price Review Model - 2022-09-09 - DEPN.xlsm", range = "Capex_FO input!B4:M14", col_names = TRUE)

# Extract expected result
result <- read_xlsx("./test/SEW_2023 Price Review Model - 2022-09-09 - DEPN.xlsm", range = "Capex_FO input!BI5:BR14", col_names = FALSE)


# Single line calculation
dpn_line <- depn_fun(c[1,], yr_op = life_yr_op$yr_op[1], life = life_yr_op$`Asset life (Regulatory)`[1])
dpn_line

# Test equality
all.equal(round(dpn_line, 4), round(unlist(result[1,], use.names = FALSE), 4))

# Matrix calculation
dpn_mtrix <- t(mapply(FUN = depn_fun, split(c, row(c)), yr_op = life_yr_op$yr_op, life = life_yr_op$`Asset life (Regulatory)`))
dpn_mtrix

# Test equality
colSums(dpn_mtrix)
all.equal(round(colSums(dpn_mtrix), 4), round(unname(colSums(result)), 4))



# --------------------------------------------------------------------------------------------------------------------------
# Test "add_trend_season" function
# --------------------------------------------------------------------------------------------------------------------------

source("funs.R")
y <- add_trend_season(y=100, s=0, a=1, p=1.5)
plot(1:12, y ,type="l", main = "trend")
sum(y)

y <- round(as.vector(sapply(X = c(100,110,120), FUN = add_trend_season, s=0, a=1, p=1.5)), 3)
plot(1:36, y ,type="l", main = "trend")
sum(y)



# --------------------------------------------------------------------------------------------------------------------------
# Test days receivable logic
# --------------------------------------------------------------------------------------------------------------------------

source("funs.R")

# 
debtors.r1 <- "
   30     30     31
15377  21465  20257
11803  11948  11898
-5714 -13157      0
21465  20257      0
"

debtors1 <- as.numeric(unlist(strsplit(trimws(debtors.r1), "\\s+")))
debtors1 <- matrix(debtors1, ncol=3, byrow=TRUE)
rownames(debtors1) <- c("Days","Open","Incm","Cash","Close")
debtors1

# 
debtors.r2 <- "
   30     30     31
 5000   5000   5000
 2466   2466   2548
-2466  -2466      0
 5000   5000      0
"

debtors2 <- as.numeric(unlist(strsplit(trimws(debtors.r2), "\\s+")))
debtors2 <- matrix(debtors2, ncol=3, byrow=TRUE)
rownames(debtors2) <- c("Days","Open","Incm","Cash","Close")
debtors2

# 
debtors.r3 <- "
    30     30     31
-20346   -20346   -20346
 -7717    -7717    -7717
  7717     7717        0
-20346   -20346        0
"

debtors3 <- as.numeric(unlist(strsplit(trimws(debtors.r3), "\\s+")))
debtors3 <- matrix(debtors3, ncol=3, byrow=TRUE)
rownames(debtors3) <- c("Days","Open","Exps","Cash","Close")
debtors3

# Excel version
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "debtors_days")
writeData(wb, sheet = "debtors_days", x = debtors1, startCol = 2, startRow = 2)
writeData(wb, sheet = "debtors_days", x = 60, startCol = 4, startRow = 1)
writeData(wb, sheet = "debtors_days", x = debtors2, startCol = 2, startRow = 11)
writeData(wb, sheet = "debtors_days", x = 60, startCol = 4, startRow = 10)
writeFormula(wb, 1, x = "MIN(0, MAX(-SUM(D4:D5),ROUND(D1*SUM(B5:D5)/SUM(B3:D3)*3-SUM(C4:D4)-SUM(D4:D5),0)))", startCol = 4, startRow = 6)
writeFormula(wb, 1, x = "MIN(0, MAX(-SUM(D13:D14),ROUND(D10*SUM(B14:D14)/SUM(B12:D12)*3-SUM(C13:D13)-SUM(D13:D14),0)))", startCol = 4, startRow = 15)
writeFormula(wb, 1, x = "SUM(D4:D6)", startCol = 4, startRow = 7)
writeFormula(wb, 1, x = "SUM(D13:D15)", startCol = 4, startRow = 16)
writeFormula(wb, 1, x = "ROUND(AVERAGE(B7:D7)/SUM(B5:D5)*SUM(B3:D3), 0)", startCol = 4, startRow = 8)
writeFormula(wb, 1, x = "ROUND(AVERAGE(B16:D16)/SUM(B14:D14)*SUM(B12:D12), 0)", startCol = 4, startRow = 17)
saveWorkbook(wb, "./test/debtors_days_test.xlsx", overwrite = TRUE)


lookback <- 3
debtors_days <- 60
debtors <- debtors3
trail_inc <- sum(debtors["Incm", 1:3])    # 35,649
sum_days <- 91
prior_bals <- sum(debtors["Close", 1:2]) # 41,722
cash <- round(
  abs( (debtors_days * trail_inc / sum_days * lookback )
  - prior_bals         # this should be lookback less 1
  - debtors["Open", 3] # 20,257 #mat["3100", "open", i]
  - debtors["Incm", 3] # 11,898 #mat["1000", "incm", i]
  ) , 3)

cash                        #15,753
debtors["Cash", 3] <- -cash
debtors["Close", 3] <- sum(debtors[1:3, 3])
debtors

days_calc <- mean(debtors["Close", ]) / sum(debtors["Incm", ]) * sum_days
days_calc