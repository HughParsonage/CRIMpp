#include <Rcpp.h>
#include "BalanceAfterRealLevelPayments.h"
using namespace Rcpp;

//' @title Vectorized age pension
//' @description This function is the same \code{\link{AgePension}} but vectorized. Perversely it should be faster than linear.
//' @param Assets Vector of assets.
//' @param Income Vector of income.
//' @param HomeOwner Vector of home-owner.
//' @param Age Vector of ages.
//' @param Year Vector of years
//' @details \code{age_pension} is a vectorized version of \code{\link{AgePension}}.
// [[Rcpp::export]]
NumericVector age_pension(NumericVector Assets,
                          NumericVector Income,
                          LogicalVector HomeOwner,
                          IntegerVector Age,
                          IntegerVector Year) {
  int n = Assets.length();
  NumericVector out(n);
  
  for (int k = 0; k < n; ++k) {
    out[k] = AgePension(Assets[k], Income[k], HomeOwner[k], Age[k], Year[k]);
  }
  return out;
  
}
