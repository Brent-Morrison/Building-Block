library(tidyverse)

# Capex data --------------------------------------------------------------------------------------
df_raw <- read.csv("./data/capex.csv", fileEncoding="UTF-8-BOM")

capex <- df_raw %>% pivot_longer(
  cols = !c(entity, service, cost_driver, asset_category, year_operational, regulatory_life, tax_life), 
  names_to = "spend_type", 
  values_to = "amount"
) %>% 
  filter(amount != 0) %>% 
  mutate(
    balance_type = sub("_.*", "", spend_type),
    balance_type = sub("\\.", "_", balance_type),
    year = as.integer(gsub("[^0-9]", "", spend_type))
  ) %>% 
  select(-spend_type) %>% 
  select(entity, year, balance_type, service, asset_category, cost_driver, year_operational, regulatory_life, tax_life, amount)


# Opex data ---------------------------------------------------------------------------------------
df_raw <- read.csv("./data/opex.csv", fileEncoding="UTF-8-BOM")

opex <- df_raw %>% pivot_longer(
  cols = !c(service, balance_type), 
  names_to = "col_name", 
  values_to = "amount"
) %>% 
  filter(amount != 0) %>% 
  mutate(
    entity = sub("_.*", "", col_name),
    #balance_type = sub("\\.", "_", col_name),
    year = as.integer(gsub("[^0-9]", "", col_name))
  ) %>% 
  mutate(
    cost_driver = NA, 
    asset_category = NA, 
    year_operational = NA, 
    regulatory_life = NA, 
    tax_life = NA
  ) %>% 
  select(entity, year, balance_type, service, asset_category, cost_driver, year_operational, regulatory_life, tax_life, amount)


# Combine -----------------------------------------------------------------------------------------
capex_opex <- bind_rows(capex, opex)

write.csv(capex_opex, "./data/price_subm_2023.csv")



# --------------------------------------------------------------------------------------------------------------------------
# Load from text
# --------------------------------------------------------------------------------------------------------------------------

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


# --------------------------------------------------------------------------------------------------------------------------
# Convert names
# --------------------------------------------------------------------------------------------------------------------------
dat <- read.csv("./data/price_subm_2023.csv")

dat$asset_category <- gsub("-", "_", dat$asset_category)
dat$asset_category <- gsub(" ", "_", dat$asset_category)
dat$asset_category <- gsub("\\(", "", dat$asset_category)
dat$asset_category <- gsub("\\)", "", dat$asset_category)
dat$asset_category <- tolower(gsub("/", "_", dat$asset_category))
unique(dat$asset_category)



