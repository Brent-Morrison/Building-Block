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


# ----------------------------------------------------------------------------


dat <- read.csv("./data/price_subm_2023.csv")
source("funs.R")

library(tidyr)
library(dplyr)

cw <- dat %>% 
  filter(entity == "CW", balance_type == "gross_capex") %>% 
  select(-c(X, entity, balance_type, service, asset_category, cost_driver, tax_life)) %>% 
  pivot_wider(names_from = year, values_from = amount, values_fn = sum, values_fill = 0)

c <- as.matrix(cw[, 3:(ncol(cw))])
c

yr_int <- as.integer(colnames(cw)[-c(1:2)])
yr_int

year_operational <- as.integer(sub(".*-", "", cw$year_operational))+2000
year_operational[is.na(year_operational)] <- yr_int[1]
year_operational

yr_op <- match(year_operational, yr_int)
yr_op

life <- ifelse(cw$regulatory_life == 0,1,cw$regulatory_life)

dpn_mtrix <- t(mapply(FUN = depn_fun, split(c, row(c)), yr_op = yr_op, life = life))
dpn_mtrix
colSums(dpn_mtrix)



#xc <- xtabs(amount ~ regulatory_life + year + year_operational, dat[dat$entity == "CW" & dat$balance_type == "gross_capex", ])
#xc[1]
#xc1 <- ftable(xc)
#colnames(xc1)

#xeg <- xtabs(amount ~ regulatory_life + year , dat[dat$entity == "CW" & dat$balance_type == "gross_capex", ])
#as.integer(row.names(xeg))
#yr_int <- colnames(xeg)
#as.integer(row.names(xeg))