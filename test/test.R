
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

library(dplyr)

# Data
pqr <- read_xlsx(path, range = paste0("RevenuePriceCap_FO","!","E68:AL443"), col_names = TRUE)
q <- as.matrix(pqr %>% filter(PQR == 'Qty') %>% select(`2023-24`:`2027-28`)) #select(c(PQR:Unit,`2023-24`:`2027-28`))
q[is.na(q)] <- 0

p <- as.matrix(pqr %>% filter(PQR == 'Price') %>% select(`2023-24`:`2027-28`)) #select(c(PQR:Unit,`2023-24`:`2027-28`))
p[is.na(p)] <- 0

r <- p* q

tariff_revenue <- colSums(r) / 1e6
tariff_revenue
sum(tariff_revenue)  # should sum to 4,571.47


# Get period zero prices
p0 <- as.matrix(pqr %>% filter(PQR == 'Price') %>% select(`2022-23`)) #select(c(PQR:Unit,`2023-24`:`2027-28`))
p0[is.na(p0)] <- 0


# Define price delta (pd) & convert to cumulative change for matrix multiplication
pd1 <- c(rep(0, 2), rep(0.0483, 3))            # option 1, zero in first two years
pd2 <- c(rep(0.009, 2), rep(0.025, 3))         # option 2, 0.9% in first year

pdyr <- 2                                                 # Price delta year
pdpar <- c(0.009, 0.025)                                  # Price delta to optimise (parameter)
pdvec <- c(rep(pdpar[1], pdyr - 1), rep(pdpar[2], 5 - pdyr + 1))  # Vector of price changes
pd <- exp(cumsum( log(1 + pdvec) )) - 1
pnew <- p0 %*% (1 + pd)

r <- pnew * q
tariff_revenue <- colSums(r) / 1e6
tariff_revenue
sum(tariff_revenue)

p0 <- matrix(rep(10,10), ncol = 1)
q <- matrix(rep(10,10*5), ncol = 5)


# Test function
pdpar <- c(-0.057, 0)
theta <- c(0.0255, pdpar)
npv_optim_func(theta, pdyr=1, rev_req=c(944.25,923.41,921.62,917.46,926.28), p0, q)




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
