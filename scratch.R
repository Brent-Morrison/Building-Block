library(tidyverse)

df_raw <- read.csv("./data/scratch.csv", fileEncoding="UTF-8-BOM")

df <- df_raw %>% pivot_longer(
  cols = !c(entity, service, cost_driver, asset_category, year_operational, regulatory_life, tax_life), 
  names_to = "spend_type", 
  values_to = "amount"
  ) %>% 
  filter(amount != 0) %>% 
  mutate(
    balance_type = sub("_.*", "", spend_type),  #str_split(spend_type , "_")[[1]][1],
    balance_type = sub("\\.", "_", balance_type),
    year = as.integer(gsub("[^0-9]", "", spend_type))
    ) %>% 
  select(-spend_type)

write.csv(df, "./data/price_subm_2023.csv")
