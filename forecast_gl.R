# ---------------------------------------------------------------------------------------------------------
# Matrix accounting set up
# ---------------------------------------------------------------------------------------------------------

# Parameters -------------------------------------------------------------
dat_src       <- "local"   # "local" / "remote"
mons          <- 60           # months to forecast
open_bals_col <- "cw_23"
infltn_factor <- exp(cumsum( log(1 + rep(fcast_infltn, 5)) ))
month_end     <- seq(as.Date("2024-01-31") + 1, by = "month", length.out = mons) - 1
days          <- as.numeric(format(month_end, "%d"))
accrued_days  <- 60
debtors_days  <- 45
crdtr_days_ox <- 90
crdtr_days_cx <- 45

# Data sources
if (dat_src == "local") {
  chart_src   <- "./data/chart.csv"
  txn_src     <- "./data/txn_type.csv"
} else {
  chart_src   <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/chart.csv"
  txn_src     <- "https://raw.githubusercontent.com/Brent-Morrison/Building-Block/master/data/txn_type.csv"
}

chart    <- read.csv(chart_src, fileEncoding="UTF-8-BOM")
txn_type <- read.csv(txn_src, fileEncoding="UTF-8-BOM")
rownames(txn_type) <- txn_type$txn_code


# Matrix -----------------------------------------------------------------
mon <- 1:mons   # Number of months
txn <- unlist(txn_type[,"txn_code"], use.names = FALSE)  # Transaction types
act <- unlist(chart[,"account_no"], use.names = FALSE)  # GL accounts

# Create matrix and assign names 
mat <- array(rep(0, length(act) * length(txn) * length(mon)), dim=c(length(act), length(txn), length(mon)))
dimnames(mat)[[1]] <- act
dimnames(mat)[[2]] <- txn
dimnames(mat)[[3]] <- mon

# Insert opening balances
opn_bal <- unlist(chart[,open_bals_col], use.names = FALSE)
mat[ , "open", 1]  <- opn_bal

# Rollover and update retained earning
mat["5200","open",1] <- mat["5200","open",1] + sum(mat[as.character(chart[chart$statement_type == 1, ]$account_no), "open" ,1])
mat[as.character(chart[chart$statement_type == 1, ]$account_no), "open" ,1] <- 0

# Check
round(colSums(mat[,,1]), 3)
mat[,,1]


# ---------------------------------------------------------------------------------------------------------
# Transaction balances
# https://stackoverflow.com/questions/19340401/convert-a-row-of-a-data-frame-to-a-simple-vector-in-r
# ---------------------------------------------------------------------------------------------------------

# Income -----------------------------------------------------------------
tot_rev_nmnl <- tot_rev_real * infltn_factor * 1e3
incm <- round(as.vector(sapply(X = tot_rev_nmnl, FUN = add_trend_season, s=0, a=1, p=1.5)), 3)
gift <- round(rep(cc / 12, each = 12), 3)


# Expenses ---------------------------------------------------------------
exp1 <- unlist(opex[opex$year %in% initial_fcast_yr:(initial_fcast_yr + 4), "amount"], use.names = FALSE) * 1000 * infltn_factor
exp1 <- round(as.vector(sapply(X = exp1, FUN = add_trend_season, s=0, a=0, p=0)), 3)


# Capex ------------------------------------------------------------------
cpx1 <- round(rep(cx / 12, each = 12), 3) * 1000


# Depreciation -----------------------------------------------------------
# - on opening balance
stat_depn_bld <- depn_bv(
  yrs=5, 
  de=chart[chart$account_no == 2215, "cw_23"], 
  gr=chart[chart$account_no == 3510, "cw_23"], 
  ad=chart[chart$account_no == 3515, "cw_23"]
  )

stat_depn_lhi <- depn_bv(
  yrs=5, 
  de=chart[chart$account_no == 2225, "cw_23"], 
  gr=chart[chart$account_no == 3520, "cw_23"], 
  ad=chart[chart$account_no == 3525, "cw_23"]
  )

stat_depn_pae <- depn_bv(
  yrs=5, 
  de=chart[chart$account_no == 2235, "cw_23"], 
  gr=chart[chart$account_no == 3530, "cw_23"], 
  ad=chart[chart$account_no == 3535, "cw_23"]
  )

stat_depn_inf <- depn_bv(
  yrs=5, 
  de=chart[chart$account_no == 2245, "cw_23"], 
  gr=chart[chart$account_no == 3540, "cw_23"], 
  ad=chart[chart$account_no == 3545, "cw_23"]
  )

stat_depn_sca <- depn_bv(
  yrs=5, 
  de=chart[chart$account_no == 2265, "cw_23"], 
  gr=chart[chart$account_no == 3560, "cw_23"], 
  ad=chart[chart$account_no == 3565, "cw_23"]
  )

stat_depn_int <- depn_bv(
  yrs=5, 
  de=chart[chart$account_no == 2205, "cw_23"], 
  gr=chart[chart$account_no == 3600, "cw_23"], 
  ad=chart[chart$account_no == 3605, "cw_23"]
  )



# - on capex (balances moved from WIP)
dpn1 <- rep(50, mons)                      # TO DO - create depn schedule re opening balances and capex, assume transfer from WIP to asset register
dpn_mtrix <- t(mapply(FUN = depn_fun, split(c, row(c)), yr_op = yr_op, life = life))
dpn_mtrix



# -------------------------------------------------------------------------------------------------
# Transaction loop
# -------------------------------------------------------------------------------------------------

for (i in 1:length(mon)) {
  
  # Opening balances & debtors ageing post loop / month 1 -----------------------------------------
  if (i > 1) {
   
    # Opening balances
    mat[, "open", i] <- mat[, "clos", i-1]

  }
  
  
  # Post income (DR accrued income / CR income) ---------------------------------------------------
  t <- "aidb"
  mat[drcr(t, txn_type), t, i] <- c(incm[i], -incm[i])

  
  # Cash receipt to specify desired closing balance for accrued income days parameter -------------
  trail <- 3
  if (i < trail) s <- 1 else s <- i - (trail - 1)
  trail_inc <- -mean(mat["1000", "aidb", s:i]) * trail
  sum_days <- mean(days[s:i]) * trail
  prior_bals <- mean(mat["3050", "open", s:i]) * (trail - 1)
  tfer <- round( abs(accrued_days * trail_inc / sum_days * 3 - prior_bals - mat["3050", "open", i] + mat["1000", "aidb", i]) , 3)
  t <- "incm"
  mat[drcr(t, txn_type), t, i] <- c(tfer, -tfer)
  
  
  # Cash receipt to specify desired closing balance for debtors days parameter --------------------
  trail <- 3
  if (i < trail) s <- 1 else s <- i - (trail - 1)
  trail_inc <- -mean(mat["3050", "incm", s:i]) * trail
  sum_days <- mean(days[s:i]) * trail
  prior_bals <- mean(mat["3100", "open", s:i]) * (trail - 1)
  desired_bal <- debtors_days * trail_inc / sum_days * 3 - prior_bals
  rcpt <- round( abs(desired_bal - mat["3100", "open", i] + mat["3050", "incm", i]) , 3)
  t <- "cshd"
  mat[drcr(t, txn_type), t, i] <- c(rcpt, -rcpt)
  
  
  # Bad debts WO
  # wo <- mat[,,i]["3053", "opn"] + mat[,,i]["3053", "csh"]
  # mat[,,i]["3053", "wof"] <- -wo
  # mat[,,i]["270", "wof"] <- wo
  
  
  # Income re gifted assets -----------------------------------------------------------------------
  t <- "gift"
  mat[drcr(t, txn_type), t, i] <- c(gift[i], -gift[i])
  
  
  # Expenses --------------------------------------------------------------------------------------
  t <- "exp1"
  mat[drcr(t, txn_type), t, i] <- c(exp1[i], -exp1[i])
  
  
  # Cash payment re trade creditors ---------------------------------------------------------------
  trail <- 3
  #if (i < trail) s <- 1 else s <- i - (trail - 1)
  #trail_exp <- -mean(mat["2000", "exp1", s:i]) * trail
  #sum_days <- mean(days[s:i]) * trail
  #prior_bals <- mean(mat["4000", "open", (s-1):i]) * pmin(max(s,i), trail-1)
  #desired_bal <- crdtr_days_ox * trail_exp / sum_days * 3 - prior_bals
  #rcpt <- round( abs(desired_bal - mat["4000", "open", i] + mat["2000", "exp1", i]) , 3)
  
  rcpt <- trgt_days(i, d=crdtr_days_ox, trail=3, bal_acnt="4000", pl_acnt="2000", txn="exp1")
  t <- "crd1"
  mat[drcr(t, txn_type), t, i] <- c(rcpt, -rcpt)
  
  
  # Capex
  t <- "cpx1"
  mat[drcr(t, txn_type), t, i] <- c(cpx1[i], -cpx1[i])
  
  
  # Cash payment re capex -------------------------------------------------------------------------
  #trail <- 3
  #if (i < trail) s <- 1 else s <- i - (trail - 1)
  #trail_exp <- -mean(mat["3645", "cpx1", s:i]) * trail
  #sum_days <- mean(days[s:i]) * trail
  #prior_bals <- mean(mat["4010", "clos", (s-1):i]) * pmin(max(s,i), trail-1)
  #desired_bal <- crdtr_days_ox * trail_inc / sum_days * 3 - prior_bals
  #rcpt <- round( abs(desired_bal - mat["4010", "open", i] + mat["3645", "exp1", i]) , 3)
  
  rcpt <- trgt_days(i, d=crdtr_days_cx, trail=3, bal_acnt="4010", pl_acnt="3645", txn="cpx1")
  t <- "wipc"
  mat[drcr(t, txn_type), t, i] <- c(rcpt, -rcpt)
  
  
  # Interest (accrue) -----------------------------------------------------------------------------
  int <- round(mat["4500", "open", i] * cost_of_debt_nmnl / 12, 3)
  t <- "inta"
  mat[drcr(t, txn_type), t, i] <- c(-int, int)
  
  
  # Interest (pay quarterly) ----------------------------------------------------------------------
  if (i %in% c(3,6,9,12)) {
    t <- "intp"
    intp <- -mat["4020", "open", i]
    mat[drcr(t, txn_type), t, i] <- c(intp, -intp)
  }
  
  
  # Depreciation ----------------------------------------------------------------------------------
  t <- "dpn1"
  p <- c(
    stat_depn_bld[i], -stat_depn_bld[i],
    stat_depn_lhi[i], -stat_depn_lhi[i],
    stat_depn_pae[i], -stat_depn_pae[i],
    stat_depn_pae[i], -stat_depn_pae[i],
    stat_depn_sca[i], -stat_depn_sca[i],
    0, 0
    )
  a <- drcr(t, txn_type)
  if (sum(p) == 0 & length(p) == length(a)) {
    mat[a, t, i] <- p
  } else if (sum(p) != 0) {
    print("Depreciation not posted, accounting entries do not balance to nil")
  } else if (length(p) != length(a)) {
    print(paste0("Depreciation not posted.  Posting data has length ", length(p), " and posting rule has length ", length(a)))
  }
    
  
  # Determine if borrowings required --------------------------------------------------------------
  # TODO - determine amount to borrow dynamically
  borrow_amt <- 10000
  cash_bal <- sum(mat["3000",-ncol(mat[,,i]), i])
  if (cash_bal < 0) {
    t <- "borr"
    mat[drcr(t, txn_type), t, i] <- c(borrow_amt,-borrow_amt)
  }
  
  # Update closing balance
  mat[, "clos", i] <- rowSums(mat[,-ncol(mat[,,i]), i])
  
}
mat[,,1]
mat[,,60]
t(mat["3100",,])


# Check balances
round(colSums(mat[,,6]), 3)



# ------------------------------------------------------------
# Stack 3 dim array and unpivot

mat1 <- mat
dim(mat1) <- c(length(act)*length(txn)*length(mon), 1)

df <- expand.grid(act = act, txn = txn, mon = mon)
df$mtd <- mat1[,1]

# Remove nil balances
df1 <- df[df$mtd != 0, ]

# Retain opening balances only for balance sheet, remove all closing balances
df2 <- rbind(df1[df1$txn == "open" & df1$act >= 3000, ], df1[df1$txn %in% txn[-c(1, length(txn))], ])

# Remove opening balance for P&L accounts
#df2[df2$txn == "open" , c("mtd","ytd")] <- 0

# Cumulative sum
df2$yr <- ceiling(df2$mon / 12)
df2$ltd <- ave(df2$mtd, df2$act, df2$txn, FUN=cumsum)
df2$ytd <- ave(df2$mtd, df2$act, df2$txn, df2$yr, FUN=cumsum)
df2[df2$txn == "open" , c("mtd","ytd")] <- 0
df2 <- df2[with(df2, order(mon, act, txn)), c("yr","mon","act","txn","mtd","ytd","ltd")]
df3 <- df2[df2$txn == "open" & df2$yr == 1 & df2$mon == 1, ]
for (i in df3$act) {
  insert = df3[df3$act == i, "ltd"]
  df2[df2$txn == "open" & df2$act == i, "ltd"] <- insert
}

write.csv(df2, file = "slr.csv")


rpt <- df2 %>%
  left_join(chart, by = c("act" = "account_no")) %>% 
  filter(mon %in% c(12,24,36,48,60)) %>%
  group_by(yr, account_grp) %>%
  #summarise(balance = round(sum(ytd), 0)) %>% 
  summarise(balance = round(sum(if_else(account_grp >= 300, ltd, ytd)), 0)) %>% 
  pivot_wider(
    names_from = yr,
    values_from = balance
  )

#write.csv(rpt, file = "rpt.csv")
rpt1 <- chart[,c("account_grp","account_no",open_bals_col)] %>% 
  group_by(account_grp) %>% 
  summarise(balance = round(sum(cw_23), 0)) %>% 
  filter(balance != 0)




# Create YTD matrix
new_mat <- rowSums(mat, dims = 2)
new_mat[ , "open"] <- mat[ , "open", 1]
new_mat[ , "clos"] <- mat[ , "clos", 6]
new_mat
round(colSums(new_mat), 3)



tbl <- matrix(mat, prod(dim(mat)[1:2]), dim(mat)[3])



vars1 <- c(1,2,3)
vars2 <- c(10,20,30)
vars3 <- c(4,5,6)
mult_one <- function(var1, var2, var3)
{
  var1*var2*var3
}
mapply(mult_one, vars1, vars2, vars3)


v1 <- c(1:9)
v2 <- c(1,1,1,1,2,2,2,2,2)
v1[v2 == 1]
v1[v2 == 2]
chart[chart$account_grp == 100, ]$account_no
as.character(chart[chart$account_grp == 100, ]$account_no)
mat[,,1]["3000",]
mat[,,1][as.character(chart[chart$account_grp == 100, ]$account_no),]
sum(mat[as.character(chart[chart$account_type %in% c(10,25), ]$account_no), "opn" ,1])
