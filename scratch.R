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


