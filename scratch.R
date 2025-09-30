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




# Mapply
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


plot_tariffs <- function(d){
  
  tariff_rev <- d[[1]]$tariff_rev
  
  # Plot tariff revenue
  ggplot() +
    geom_bar(
      data = data.frame(
        income_type = c( rep("Fixed", 20), rep("Variable", 20) ),
        year = rep(2024:2043, 2),
        income = c(colSums(tariff_rev[grepl("Fixed", rownames(tariff_rev)),]), colSums(tariff_rev[grepl("Variable", rownames(tariff_rev)),]) ) /1e3
      ),
      aes(fill=forcats::fct_rev(income_type), y=income, x=year), position="stack", stat="identity", colour="black"
    ) +
    scale_fill_manual(values = c("grey60", "grey30")) +
    geom_point(
      data = data.frame(
        year = 2024:2043,
        perc_fixed = colSums( tariff_rev[grepl("Fixed", rownames(tariff_rev)),] ) / colSums(tariff_rev) * max(colSums(tariff_rev)) / 1e3
      ),
      aes(x = year, y = perc_fixed), colour="black", shape = 15, size = 3
    ) +
    scale_y_continuous("", sec.axis = sec_axis(~ . / (max(colSums(tariff_rev)) / 1e3), name = "")) +
    scale_x_continuous(breaks = c(2028,2033,2038,2043)) +
    ggthemes::theme_base() +
    labs(
      x = "",
      y = "",
      title = "Tariff revenue - fixed vs variable (left axis, $m)",
      subtitle = "Proportion of fixed tariff revenue (right axis, square points)"
    ) + 
    theme(
      plot.subtitle = element_text(size = 11.5, face = 'italic'),
      legend.title = element_blank(),
      #legend.position = c(0.1,0.9),
      legend.background = element_blank(),
      legend.key = element_blank(),
    )
}
