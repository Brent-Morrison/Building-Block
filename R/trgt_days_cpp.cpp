#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
double trgt_days_cpp(const NumericVector& mat,
                     const IntegerVector& dims,
                     const NumericVector& days,
                     int i, double d, int trail,
                     IntegerVector bal_i,
                     IntegerVector pl_i,
                     IntegerVector txn_i,      // <-- was int txn_i
                     int open_i, int clos_i) {
  
  int n_acct = dims[0];
  int n_txn  = dims[1];
  
  auto idx = [&](int a, int t, int m) {
    return a + n_acct * (t + n_txn * m);
  };
  
  int s = (i < trail) ? 0 : i - trail;
  int i0 = i - 1;
  int n_months = i0 - s + 1;
  
  // ---- trail_exp: sum over pl_i x txn_i x months, divided by (n_txn * n_months) ----
  double total = 0.0;
  for (int m = s; m <= i0; ++m)
    for (int p = 0; p < pl_i.size(); ++p)
      for (int t = 0; t < txn_i.size(); ++t)
        total += mat[idx(pl_i[p]-1, txn_i[t]-1, m)];
  
  double trail_exp = -total / (txn_i.size() * n_months) * trail;
  
  // ---- sum_days: unaffected by txn ----
  double sum_days = 0.0;
  for (int m = s; m <= i0; ++m)
    sum_days += days[m];
  sum_days = sum_days / n_months * trail;
  
  // ---- prior_bals: unaffected by txn (bal_i assumed length 1, as in R usage) ----
  double prior_bals = 0.0;
  if (i > 1) {
    for (int m = s; m < i0; ++m)
      prior_bals += mat[idx(bal_i[0]-1, clos_i-1, m)];
    prior_bals = prior_bals / (i0 - s) * (trail - 1);
  }
  
  double desired_bal = d * trail_exp / sum_days * trail - prior_bals;
  
  // ---- bal_pre: opening balance + |accrual|, accrual summed over pl_i x txn_i for month i0 ----
  double bal_pre = mat[idx(bal_i[0]-1, open_i-1, i0)];
  
  double accr = 0.0;
  for (int p = 0; p < pl_i.size(); ++p)
    for (int t = 0; t < txn_i.size(); ++t)
      accr += mat[idx(pl_i[p]-1, txn_i[t]-1, i0)];
  
  bal_pre += std::abs(accr);
  
  double delta = std::round((desired_bal - bal_pre) * 1000.0) / 1000.0;
  
  // ---- diagnostic print ----------------------
  //Rcpp::Rcout << "i=" << i << " s=" << s << " i0=" << i0
  //            << " trail_exp=" << trail_exp
  //            << " sum_days=" << sum_days
  //            << " prior_bals=" << prior_bals
  //            << " bal_pre=" << bal_pre
  //            << " delta=" << delta << "\n";
  // --------------------------------------------
  
  double rcpt0;
  if (prior_bals > 0)
    rcpt0 = std::min(0.0, std::max(-bal_pre, delta));
  else
    rcpt0 = std::max(0.0, std::min(-bal_pre, delta));
  
  if (i < trail) {
    // sum over bal_i x txn_i for month i0
    double bal_txn = 0.0;
    for (int b = 0; b < bal_i.size(); ++b)
      for (int t = 0; t < txn_i.size(); ++t)
        bal_txn += mat[idx(bal_i[b]-1, txn_i[t]-1, i0)];
    return -bal_txn;
  }
  
  return rcpt0;
}