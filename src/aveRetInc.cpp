//' @title Average income in retirement
//' @name aveRetInc
//' @description Average income in retirement.
//' @param AWOTE_prop Proportion of AWOTE.
//' @export aveRetInc

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
void showOutput(double x) {
  Rcout << "super balance" << x << std::endl;
}

// [[Rcpp::export]]
double aveRetInc(int AWOTE_prop = 1L,
                        double AWOTE_starting_year = 78200,
                        double start_balance = 0,
                        std::string deflator = "CPI",
                        int starting_age = 30L,
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
                        double taxable_earnings = 0.53) {
  int n = retirement_age - starting_age;
  int age = starting_age;
  double salary = AWOTE_starting_year;
  double wage_index = 0;
  double super_balance = start_balance;
  double SG_rate = 9.19 / 100;
  double contribution = 0.0;
  double net_contribution = 0.0;
  double earnings = 0.0;
  double net_earnings = 0.0;
  double cpi = 1;
  
  int year = starting_year;
  for (int i = 1; i <= n; ++i) {
    cpi *= (1 + long_term_CPI);
    if (i < 5) {
      wage_index = short_run_wage_index[i];
    } else {
      wage_index = long_run_wage_index;
    }
    
    // earnings
    if (age < retirement_age) {
      earnings = super_balance * asset_earnings_accumulation;
      net_earnings = (1 - earnings_tax_accumulation) * earnings;
    } else {
      earnings = super_balance * asset_earnings_pension;
      net_earnings = (1 - earnings_tax_pension) * earnings;
    }
    super_balance += net_earnings;
    
    // contribution (only SG for now)
    if (year >= 2021) {
      if (year < 2024){
        SG_rate = 0.1;
      } else {
        SG_rate = 0.11;
      }
    }
    salary = salary * (1 + wage_index);
    
    contribution = salary * SG_rate;
    net_contribution = (1 - contribution_tax) * contribution;
    super_balance += net_contribution;
    age += 1;
    year += 1;
  }
  return super_balance;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
