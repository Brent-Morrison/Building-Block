
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
pdpar <- c(0.059, 0, 0)                                                                             # Price delta to optimise (parameter)
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
theta <- c(0.0255, 0.01940774)
npv_optim_func(theta, pdyr=0, rev_req=c(944.2496,923.408,921.6224,917.4634,926.2756), p0, q)


# Perform optimisation --------------------------
optim(
  
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
