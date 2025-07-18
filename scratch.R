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


# Depreciation estimate
depn_rate_infa <- 29560 / chart[chart$account_no == 3540, "cw_23"] # p.92 accounts
depn_rate_infa





# https://stackoverflow.com/questions/35889954/mapply-for-all-arguments-combinations-r
toy <- function(x, y, z){
  paste(x, y, z)
}

args <- expand.grid(x = 1:2, y = c("#", "$"), z = c("a", "b"))

mapply(FUN = toy, x = args$x, y = args$y, z = args$z)

# Rolling sum on 3d array
x <- array(1:24, c(4, 2, 3))
x
sum(x[1,1,c(1,2)])
sum(x[1,1,c(2,3)])

r <- rep(0,2)
counter <- 0
for (i in 2:3) {
  counter <- counter + 1
  t <- sum(x[1,1,c(i-1,i)])
  print(t)
  r[counter] <- t
}
r


# Filter matrix for all income
# https://stackoverflow.com/questions/64999483/how-to-do-rolling-sum-over-columns-in-r
mat <- res_scenario[[1]]$txns
mat[grepl("^1", rownames(mat)), c("open","clos") , 1]
sum(mat[grepl("^10", rownames(mat)), c("open","clos") , 13])
sum( mat[grepl("^1|^2", rownames(mat)), !colnames(mat) %in% c("open","clos"), 12] )
apply(mat, 3, function(x) sum(x[grepl("^1|^2", rownames(x)), !colnames(mat) %in% c("open","clos")]), simplify = TRUE)

ni <- apply(mat, 3, function(x) sum(x[grepl("^1|^2", rownames(x)), !colnames(mat) %in% c("open","clos")]), simplify = TRUE) # net income
ta <- apply(mat, 3, function(x) sum(x[grepl("^3", rownames(x)), "clos"]), simplify = TRUE) # total assets
ta
slide_sum(ni, before = 12) / slide_mean(ta, before = 12)
monthly_indicators$ret_on_asset
plot(1:240, -slide_sum(ni, before = 12) / slide_mean(ta, before = 12), type = "S")
lines(1:240, monthly_indicators$ret_on_asset, col = "blue")

ret_on_asset_fun <- function(m) {
  mat <- m$txns
  ni <- apply(mat, 3, function(x) sum(x[grepl("^1|^2", rownames(x)), !colnames(x) %in% c("open","clos")]), simplify = TRUE)   # net income
  ta <- apply(mat, 3, function(x) sum(x[grepl("^3", rownames(x)), "clos"]), simplify = TRUE)                                  # total assets
  return( slide_sum(ni, before = 12) / slide_mean(ta, before = 12) )
}

int_fin_ratio_fun <- function(m) {
  #mat <- res$txns
  mat <- m$txns
  op_cf <- apply(mat, 3, function(x) sum(x["3000", c("cshd","exp2","crd1")]), simplify = TRUE)   # operating cashflows
  capex <- apply(mat, 3, function(x) sum(x["3000", "wipc"]), simplify = TRUE)                    # capex
  return( slide_sum(op_cf, before = 12) / slide_sum(capex, before = 12) )
}
          
z <- lapply(res_scenario, int_fin_ratio_fun)

for (i in 1:length(z)) {
  if (i == 1) plot(1:240, z[[i]], type = "l") else lines(1:240, z[[i]], col = "blue")
}
