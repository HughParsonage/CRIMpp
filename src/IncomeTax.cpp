//' @title Income tax
//' @description Income tax for a single worker.
//' @param income The persons's income.
//' @param thresholds Income tax brackets
//' @param rates Marginal tax rates.
//' @name IncomeTax
//' @export IncomeTax

#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double IncomeTax (double income,
                  NumericVector thresholds = NumericVector::create(0, 18200, 37000, 87000, 180000),
                  NumericVector rates = NumericVector::create(0, 0.19, 0.325, 0.370, 0.450),
                  double litoThr = 37000,
                  double litoMax = 445,
                  double litoTaper = 0.015,
                  double saptoThr = 32279,
                  double saptoMax = 2230,
                  double saptoTaper = 0.1250,
                  double medLevyRate = 0.02,
                  double medLevyThr = 20542,
                  double medLevyShade = 0.10,
                  bool sapto_eligible = false) {
  double tax = 0.0;
  int tlen = thresholds.length();
  int rlen = thresholds.length();
  
  if (tlen != rlen){
    Rcpp::stop("Length of thresholds not equal to length of rates. Make sure they are the same length.");
  }
  
  // base tax
  for (int t = 1; t <= tlen - 1; ++t){
    if (income > thresholds[t]) {
      if (t < tlen - 1){
        if (income > thresholds[t + 1]){
          tax += rates[t] * (thresholds[t + 1] - thresholds[t]);
        } else {
          tax += rates[t] * (income - thresholds[t]);
        }
      } else {
        tax += rates[t] * (income - thresholds[t]);
      }
    }
    
  }
  
  // LITO
  double LITO = litoMax;
  if (income > litoThr){
    LITO -= litoTaper * (income - litoThr);
    if (LITO < 0){
      LITO = 0;
    }
  }
  
  tax -= LITO;
  
  // SAPTO
  double SAPTO = 0.0;
  if (sapto_eligible && tax > 0){
    SAPTO = saptoMax;
    if (income > saptoThr){
      SAPTO -= saptoTaper * income;
      if (SAPTO < 0){
        SAPTO = 0;
      }
    }
  }
  
  tax -= SAPTO;
  if (tax < 0) {
    tax = 0;
  }
    
  
  // Medicare levy
  double medicare_levy = 0.0;
  double medicare_levy_shade = 0.0;
  if (income > medLevyThr) {
    medicare_levy_shade = medLevyShade * (income - medLevyThr);
    medicare_levy = medLevyRate * income;
    
    if (medicare_levy_shade < medicare_levy) {
      medicare_levy = medicare_levy_shade;
    }
  }
  
  tax += medicare_levy;
  
  return tax;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
