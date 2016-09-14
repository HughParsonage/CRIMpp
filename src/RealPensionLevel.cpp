//' @title Real level of pension payments giving starting retirement conditions
//' @param n The number of periods.
//' @param balance The balance at retirement.
//' @param r_earnings The rate of earnings growth (\code{r_earnings = 0.065} means 6.5%).
//' @param cpi The rate of CPI.
//' @param inArrears Are the payments paid in arrears (\emph{i.e.} before the balance has grown through \code{r_earnings}.)
//' @export

#include <Rcpp.h>
// #include "BalanceAfterRealLevelPayments.h"
using namespace Rcpp;

// [[Rcpp::export]]
int RealPensionLevel(int n, int balance, double r_earnings, double cpi, bool inArrears) {
  int p = 1;
  while (BalanceAfterRealLevelPayments(p, n, balance, r_earnings, cpi, inArrears) > 0) {
    p += p + 10000;
  }

  p -= 9999;
  while (BalanceAfterRealLevelPayments(p, n, balance, r_earnings, cpi, inArrears) > 0) {
    p += p + 100;
  }

  p -= 99;
  while (BalanceAfterRealLevelPayments(p, n, balance, r_earnings, cpi, inArrears) > 0) {
    p += p + 1;
  }

  return p;
}

