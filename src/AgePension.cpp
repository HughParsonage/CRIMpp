//' @title Age pension of singles.
//' @name AgePension
//' @description Value of age pension (for singles only).
//' @param assets Assessable assets.
//' @param income Assessable income.
//' @param age Age (integer).
//' @param year Year of entitlement.
//' @param homeOwner Is the person a home-owner?
//' @param sinRaMax Maximum fortnightly entitlement to rent-assistance for singles.
//' @param sinMaxRate Pension maximum base rate, per fortnight, for singles.
//' @param sinAssHomeThr Assets threshold for single home-owners.
//' @param sinAssNonHomeThr Assets threshold for singles who don't own a home.
//' @param pensAssTpr Taper rate for assets test.
//' @param sinIncThr Income threshold for income test.
//' @param sinSupTot Pension supplement, total amount, per fortnight for a single.
//' @param sinSupMin Pension supplement, minimum amount, per fortnight for a single.
//' @export AgePension age_pension

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int pensionAge (int year) {
  // 2015-16 = 65
  // 2019-20 = 66
  int out = 70;
  if (year < 2036) {
    out = 65 + round((year - 2016) / 4);
  }
  return out;
}

// [[Rcpp::export]]
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
                   double sinCesMax = 14.1) {
  double out = 0;
  if (age > pensionAge(year)) {
    double maxRA = sinRaMax;
    if (homeOwner) {
      maxRA = 0;
    }
    
    double maxPension = 26.25 * (sinMaxRate + sinSupTot + sinCesMax + maxRA);
    double minPension = 26.25 * (sinSupMin + sinCesMax);
    
    double assTestAmt = 0;
    double assetsThr = sinAssNonHomeThr;
    if (homeOwner) {
      assetsThr = sinAssHomeThr;
    }
    if (assets > assetsThr) {
      assTestAmt = 26 * pensAssTpr * (assets - sinAssHomeThr);
    }
    
    double incTestAmt = pensIncTpr * income - 26 * sinIncThr;
    if (incTestAmt < 0) {
      incTestAmt = 0;
    }
    if (incTestAmt > maxPension) {
      incTestAmt = maxPension;
    }
    
    double bindingTest = incTestAmt;
    if (bindingTest < assTestAmt) {
      bindingTest = assTestAmt;
    }
    out = maxPension - bindingTest;
    
    if (out < minPension) {
      out = minPension;
    }
  }
  
  return out;
}














