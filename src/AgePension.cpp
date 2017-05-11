//' @title Age pension of singles.
//' @name AgePension
//' @description Value of age pension (for singles only).
//' @param assets Assessable assets.
//' @param income Assessable income.
//' @param age Age (integer).
//' @param year Year of entitlement.
//' @param homeOwner Is the person a home-owner?
//' @param sinRaMax Maximum entitlement to rent-assistance for singles.
//' @param sinAssHomeThr Assets threshold for single home-owners.
//' @param sinAssNonHomeThr Assets threshold for singles who don't own a home.
//' @param pensAssTpr Taper rate for assets test.
//' @param sinIncThr Income threshold for income test.
//' @param sinSupTot Pension supplement, total amount, per fortnight for a single.
//' @param sinSupMin Pension supplement, minimum amount, per fortnight for a single.
//' @export AgePension

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp:export]]
int pensionAge (int year) {
  // 2015-16 = 65
  // 2019-20 = 66
  int out = 70;
  if (year < 2036) {
    out = 65 + round((year - 2016) / 4);
  }
  return out;
}

// [[Rcpp:export]]
double AgePension (double assets, double income, bool homeOwner, int age, int year, double sinRaMax, double sinMaxRate, double sinAssHomeThr, double sinAssNonHomeThr, double pensAssTpr, double sinIncThr, double pensIncTpr, double sinSupTot, double sinSupMin, double sinCesMax) {
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
    if (assets > assetsThr){
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


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
