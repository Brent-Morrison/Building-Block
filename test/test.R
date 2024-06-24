
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
