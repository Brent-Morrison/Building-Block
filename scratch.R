# TB from matrix ----------------------------------------------------------------------------------
m <- sim[[1]]$txns
d     <- get_data()
chart <- d$chart
ref   <- d$ref
tb1   <- m[ , "clos", (1:20) * 12]
round(colSums(tb1), 3)

# Year to date P&L
tb2 <- tb1
for (i in 2:20) {
  tb2[as.integer(row.names(tb1)) < 3000, i] <- tb1[as.integer(row.names(tb1)) < 3000, i] - tb1[as.integer(row.names(tb1)) < 3000, i-1]
}
round(colSums(tb2), 3)

# Roll P&L to retained earnings
tb2["5200", ] <- tb2["5200", ] - colSums(tb2)

round(colSums(tb2), 3)

tb2_df <- as.data.frame(tb2)
tb2_df$account_no <- as.numeric(rownames(tb2_df))

# tb <- tb2_df %>% 
#   left_join(chart[1:6], by = join_by(account_no == account_no)) %>% 
#   full_join(ref[ref$ref_type == "account_grp",], by = join_by(account_grp == lookup1)) %>% 
#   full_join(ref[ref$ref_type == "account_type",], by = join_by(account_type == lookup1)) 

inc <- tb2_df %>% 
  left_join(chart[1:6], by = join_by(account_no == account_no)) %>% 
  full_join(ref[ref$ref_type == "account_grp",], by = join_by(account_grp == lookup1)) %>% 
  group_by(ref1, ref2) %>% 
  summarise(across(`12`:`240`, sum)) %>% 
  arrange(ref2) %>% 
  filter(!is.na(ref1)) 

bal <- tb2_df %>% 
  left_join(chart[1:6], by = join_by(account_no == account_no)) %>% 
  full_join(ref[ref$ref_type == "account_type",], by = join_by(account_type == lookup1)) %>% 
  group_by(ref1, ref2) %>% 
  summarise(across(`12`:`240`, sum)) %>% 
  arrange(ref2) %>% 
  filter(!is.na(ref1)) 

rep <- as.data.frame(bind_rows(inc, bal) %>% 
  arrange(ref2)) %>% 
  mutate(ref2 = as.character(ref2))

# Insert totals and remove NA's
rep[rep$ref2 == 8, 3:22] <- colSums(rep[rep$ref2 %in% 2:7, 3:22])        # Total revenue from transactions
rep[rep$ref2 == 17, 3:22] <- colSums(rep[rep$ref2 %in% 11:16, 3:22])     # Total expenses from transactions
rep[rep$ref2 == 19, 3:22] <- colSums(rep[rep$ref2 %in% c(8,17), 3:22])   # Net result from operating transactions
rep[rep$ref2 == 22, 3:22] <- colSums(rep[rep$ref2 %in% c(19,21), 3:22])  # Net result from transactions
rep[rep$ref2 == 27, 3:22] <- colSums(rep[rep$ref2 %in% c(25,26), 3:22])  # Total assets
rep[rep$ref2 == 35, 3:22] <- colSums(rep[rep$ref2 %in% c(33,34), 3:22])  # Total liabilities



# Format numbers
rep[ ,3:22] <- apply(rep[ ,3:22], 2, acc_num)
rep[is.na(rep)] <- ""
#rep[] <- lapply(rep, gsub, pattern = is.na, replacement = "xx", fixed = TRUE)
#rep <- rep %>% mutate(across(everything(), \(x) gsub("NA", " ", x)))
#rep <- gsub("NA", " ", rep)
#rep <- data.frame(lapply(rep, function(x) gsub("NA", " ", x)))

colnames(rep) <- c("ref1","ref2",paste("FY", as.numeric(colnames(rep[3:22])) / 12 + 2024 - 1, sep = ""))

rep %>% 
  select(-ref2) %>% 
  select(1:6) %>% 
  rename(" " = ref1) %>% 
  kbl(
    #caption = "Comprehensive operating statement",
    align = c("l",rep("r", 5))
  ) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  column_spec(1,  width = "25em") %>%
  column_spec(2:5,  width = "8em") %>% 
  row_spec(c(1,8,10,17,19,22,24,27,32,35), bold = TRUE) %>%
  row_spec(c(28,29,30,36,37), italic = TRUE) %>%
  row_spec(c(7,16,19,26,34), extra_css = "border-bottom: 1px solid") %>%
  row_spec(22, extra_css = "border-bottom: 2px solid") %>%
  #row_spec(8, bold = TRUE) %>% #, extra_css = "padding: 10px") %>% 
  row_spec(which(rep$ref1 == "blank"), color = "white") 
  











zf <- function(df, sel) {
  select(df, c(oxcx_scenario, all_of(sel)))
}
zf(args, c("q_grow","roe"))



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




# Growth components
q0       <- matrix(rep(100,5), ncol = 1)
q_grow   <- rep(0, 5)
q_grow_f <- exp( cumsum( log( 1 + q_grow ) ) )
q        <- q0 %*% q_grow_f

p0       <- matrix(c(2,3,4,5,5), ncol = 1)
infltn   <- rep(0.02, 4)
ni_pc    <- c(0.005,0.005,0.01,0.01)
p_grow   <- infltn + ni_pc
p_grow_f <- exp( cumsum( log( 1 + p_grow ) ) )
p        <- p0 %*% p_grow_f
p



infltn_factor <- exp( cumsum( log( 1 + infltn ) ) )
ni_pc_factor  <- rep(0.01, 4)
p_grow  <- infltn_factor + ni_pc_factor
p <- p0 %*% p_grow


# --------------------------------------------------------------------------------------------------------------------------



colSums(tariff_rev)[1] - sum(txns[c("1000","1001","1002","1003","1101","1102","1103","1104","1105"),"clos",12])

colSums(tariff_rev[c("Water.Residential.Fixed","Sewerage.Residential.Fixed","Water.Residential.Variable"),]) / colSums(tariff_rev)

z <- data.frame(
  Fixed = colSums(tariff_rev[c("Water.Residential.Fixed","Sewerage.Residential.Fixed"),]),
  Variable = tariff_rev["Water.Residential.Variable", ]
  )


prices <- sim[[1]]$prices
txns <- sim[[1]]$txns
tariff_rev <- sim[[1]]$tariff_rev

cust_theme_1 <- theme(
  legend.title = element_blank(),
  legend.position = c(0.9,0.9),
  legend.background = element_blank(),
  legend.key = element_blank(),
  plot.caption = element_text(size = 8, color = "grey55", face = 'italic'), 
  axis.title.y = element_text(size = 8, color = "darkslategrey"),
  axis.title.x = element_text(size = 8, color = "darkslategrey"),
  axis.text.y = element_text(size = 7, color = "darkslategrey"),
  axis.text.x = element_text(size = 7, color = "darkslategrey")
  )

data.frame(
  year = 2024:2043,
  perc_fixed = colSums( tariff_rev[grepl("Fixed", rownames(tariff_rev)),] ) / colSums(tariff_rev)
  ) 


# --------------------------------------------------------------------------------------------------------------------------
# Create annual financial data series
# --------------------------------------------------------------------------------------------------------------------------

nmnl_series <- function(q, p, q_grow, ps2, ps3, ps4) {
  
  # Create annual financial data series (revenue, cost, capex, etc) from price, quantity and an uplift factor
  # - price and quantity data is multiplied to construct annual amounts for years 1 through 5 (ps1)
  # - price period specific uplift amount are applied to incrementally escalation ps1 amounts for ps2 through ps4
  #
  # Args:
  #   q        - baseline quantities for year 1 of price submission 1
  #   p        - baseline prices for year 1 of price submission 1
  #   ps2      - uplift for price period 2
  #   ps3      - uplift for price period 3
  #   ps4      - uplift for price period 4
  #
  # Returns
  #   a series 
  
  ps1.1 <- q * p
  ps1.2 <- ps1.1 * growth_fctr(esc_rate = q_grow, len = 5)
  ps1.3 <- tail(ps1.2, 1)
  nmnl <- c(ps1.2, ps1.3 + rep(ps2, 5), ps1.3 + rep(ps3, 5), ps1.3 + rep(ps4, 5))
  nmnl
}

opex1 <- data.frame(
  year = initial_fcast_yr:(initial_fcast_yr+19), 
  amount = nmnl_series(q = 200, p = 100, q_grow = 0.03, ps2 = 0, ps3 = 110, ps4 = 120)
  )





nmnl_labour <- c(rep(fte * cost_fte, 5), rep(opex_ps2, 5), rep(opex_ps3, 5), rep(opex_ps4, 5))
nmnl_labour

infl_fctr_fte <- exp(cumsum( log(1 + rep( (q_grow_fte + ni_cost_fte) / 100 , 5)) ))
infl_fctr_fte <- real_nmnl_fctr(esc_rate = (q_grow_fte + ni_cost_fte) /100, len = 5)
real_labour <- nmnl_labour * infl_fctr_fte
nmnl_labour
real_labour





# --------------------------------------------------------------------------------------------------------------------------
# Loan portfolio
# --------------------------------------------------------------------------------------------------------------------------

rou_func(100, 50, 0.05)

initial_fcast_yr <- 2024
mons         <- 60
initial_date <- as.Date(paste0(initial_fcast_yr,"-06-01"))
dates        <- seq(initial_date, length.out = mons, by = "month")



loans <- data.frame(
  amnt = c(1000,2000,3000,4000),
  start = as.Date(c("2024-01-01","2023-06-01","2025-08-01","2025-01-01")),
  end = as.Date(c("2030-01-01","2029-02-01","2026-08-01","2029-01-01")),
  rate = c(0.05,0.06,0.03,0.02)
)

loan_mat <- matrix(rep(0, mons * 4), nrow = mons, ncol = ncol(loans), dimnames = list(dates, 1:ncol(loans)))

mask <- 
  dates >= rep(loans$start, each = mons) &
  dates <= rep(loans$end,   each = mons)

loan_mat[,] <- matrix(mask, nrow = mons) * matrix(loans$amnt, nrow = mons, ncol = length(loans$amnt), byrow = TRUE)
loan_mat
cbind(loan_mat, rep(10, 60))








# --------------------------------------------------------------------------------------------------------------------------
# Sparklines
# --------------------------------------------------------------------------------------------------------------------------

library(DT)
library(sparkline)

datatable(
  data.frame(
    Series = c("A", "B"),
    Trend = I(list(
      sparkline(c(1, 3, 2, 4, 6), type = "line"),
      sparkline(c(5, 4, 3, 2, 1), type = "line")
    ))
  ),
  escape = FALSE
)