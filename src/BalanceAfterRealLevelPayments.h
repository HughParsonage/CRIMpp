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
double AgePension (double assets,
                   double income,
                   bool homeOwner,
                   int age,
                   int year,
                   double sinRaMax = 129.4,
                   double sinMaxRate = 788.4,
                   double sinAssHomeThr = 202000,
                   double sinAssNonHomeThr = 348500,
                   double pensAssTpr = 0.015,
                   double sinIncThr = 162,
                   double pensIncTpr = 0.50,
                   double sinSupTot = 14.1,
                   double sinSupMin = 14.1,
                   double sinCesMax = 14.1);
#endif
