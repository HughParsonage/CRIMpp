//' @title Balance via Pachinko
//' @name SuperBalanceViaPachinko
//' @description Balance of super via a machine.
//' @param age Age of individual (will be rounded down to nearest 5).
//' @param percentile Income percentile.
//' @param starting_year Year of initial.
//' @param retirement_age Age at retirement.
//' @param AWOTE_prop Proportion of AWOTE.
//' @export SuperBalanceViaPachinko

#include <Rcpp.h>
using namespace Rcpp;

// NumericVector AWOTE_instance = NumericVector::create(2.49749579681088, 2.49749579681088, 2.49749579681088, 2.49749579681088, 2.49749579681088, 1.83540790696631, 1.83540790696631, 1.83540790696631, 1.83540790696631, 1.83540790696631, 3.10692373804242, 3.10692373804242, 3.10692373804242, 3.10692373804242, 3.10692373804242, 1.3214155564292, 1.3214155564292, 1.3214155564292, 1.3214155564292, 1.3214155564292, 2.0659800391047, 2.0659800391047, 2.0659800391047, 2.0659800391047, 2.0659800391047, 0.408319750145035, 0.408319750145035, 0.408319750145035, 0.408319750145035, 0.408319750145035, 2.09322792939668, 2.09322792939668, 2.09322792939668, 2.09322792939668, 2.09322792939668, 0.67274390594706, 0.67274390594706, 0.67274390594706, 0.67274390594706, 0.67274390594706),

// [[Rcpp::export]]
double SuperBalanceViaPachinko(int age, 
                               int percentile,
                               int starting_year = 2016L,
                               int retirement_age = 70L,
                               int death_age = 92L,
                               NumericVector short_run_wage_index = NumericVector::create(0.0225, 0.0250, 0.0275, 0.0325, 0.0350),
                               double long_run_wage_index = 0.0404, 
                               double long_term_CPI = 0.0250,
                               double contribution_tax = 0.15,
                               double asset_earnings_accumulation = 0.0650,
                               double earnings_tax_accumulation = 0.15,
                               double asset_earnings_pension = 0.0550,
                               double earnings_tax_pension = 0.0,
                               double taxable_earnings = 0.53, 
                               double super_account_fee_2015 = 320.0) {
  return long_term_CPI;
}
