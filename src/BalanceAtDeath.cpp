//' @title Balance after n real level payments
//' @name BalanceAfterRealLevelPayments
//' @description Super balance after level payments.
//' @param nominal_payment An integer representing the payment (in nominal terms with respect to \code{balance}).
//' @param n Number of periods (years).
//' @param balance The super balance prior to retirement.
//' @param r_earnings The rate of earnings growth.
//' @param cpi The rate of CPI growth.
//' @param inArrears (logical) Are the payments made at the end of 12 months (\emph{i.e.} after the balance has grown)?
//' @export



#include <Rcpp.h>
// #include "BalanceAfterRealLevelPayments.h"
using namespace Rcpp;

// [[Rcpp::export]]
int BalanceAfterRealLevelPayments(int nominal_payment, int n, int balance, double r_earnings, double cpi, bool inArrears) {
  int outbalance = balance;
  double outbalanceDbl = outbalance;
  if (inArrears) {
    // Payment comes out after balance has grown for the year
    for (int i = 1; i <= n; ++i) {
      outbalance += floor(outbalanceDbl * r_earnings);
      outbalance -= nominal_payment * pow(1 + cpi, i);
      outbalanceDbl = outbalance;
    }
  } else {
    for (int i = 1; i <= n; ++i) {
      outbalance -= nominal_payment * pow(1 + cpi, i);
      outbalance += floor(outbalanceDbl * r_earnings);
      outbalanceDbl = outbalance;
    }
  }
  return outbalance;
}

