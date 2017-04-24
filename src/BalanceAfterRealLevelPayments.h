#ifndef CRIMpp_BalanceAfterRealLevelPayments_H
#define CRIMpp_BalanceAfterRealLevelPayments_H

int BalanceAfterRealLevelPayments(int nominal_payment, int n, int balance, double r_earnings, double cpi, bool inArrears);
double IncomeTax (double income,
                  Rcpp::NumericVector thresholds,
                  Rcpp::NumericVector rates,
                  double litoThr = 37000,
                  double litoMax = 445,
                  double litoTaper = 0.015,
                  double saptoThr = 32279,
                  double saptoMax = 2230,
                  double saptoTaper = 0.1250,
                  double medLevyRate = 0.02,
                  double medLevyThr = 20542,
                  double medLevyShade = 0.10,
                  bool sapto_eligible = false);

#endif
