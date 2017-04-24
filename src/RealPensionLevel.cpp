//' @title Real level of pension payments giving starting retirement conditions
//' @name RealPensionLevel
//' @description The value of a cpi-adjusted pension in nominal terms.
//' @param n The number of periods.
//' @param balance The balance at retirement.
//' @param r_earnings The rate of earnings growth (\code{r_earnings = 0.065} means 6.5\%).
//' @param cpi The rate of CPI.
//' @param inArrears Are the payments paid in arrears (\emph{i.e.} before the balance has grown through \code{r_earnings}.)
//' @export

#include <Rcpp.h>
#include "BalanceAfterRealLevelPayments.h"
using namespace Rcpp;

// [[Rcpp::export]]
int RealPensionLevel(int n, int balance, double r_earnings, double cpi, bool inArrears) {
  int p = 1;
  int N = 13;
  int j = N;
  for (int k = 1; k <= N; ++k){
    j = N - k;
    while (BalanceAfterRealLevelPayments(p, n, balance, r_earnings, cpi, inArrears) > 0) {
      p += p + pow(2, k);
    }
    
    p -= pow(2, k);
  }
  
  while (BalanceAfterRealLevelPayments(p, n, balance, r_earnings, cpi, inArrears) > 0) {
    p += 1;
  }
  
  return p;
}

