//' @title Average income in retirement
//' @name SuperBalanceAtRetirement
//' @description Average income in retirement.
//' @param AWOTE_prop Proportion of AWOTE.
//' @export SuperBalanceAtRetirement

#include <Rcpp.h>
#include "BalanceAfterRealLevelPayments.h"
using namespace Rcpp;

//
// [[Rcpp::export]]
void showOutput(double x) {
  Rcout << "super balance" << x << std::endl;
}

// [[Rcpp::export]]
double SuperBalanceAtRetirement(double AWOTE_starting_year = 78200,
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
                                double taxable_earnings = 0.53, 
                                double super_account_fee_2015 = 320.0) {
  int n = retirement_age - starting_age;
  int age = starting_age;
  double salary = AWOTE_starting_year;
  double income = salary;
  double wage_index = 0;
  double super_balance = start_balance;
  double underlying_super_cap = 30000;
  int super_cap = 30000;
  double SG_rate = 9.19 / 100;
  double SG_payment = 0.0;
  double contribution = 0.0;
  double net_contribution = 0.0;
  double earnings = 0.0;
  double net_earnings = 0.0;
  double cpi = 1;
  // grattan::cpi_inflator(from_fy = "2014-15", to_fy = "2016-17") = 1.032081
  double super_fee = super_account_fee_2015 * 1.032;
  double income_tax = 0.0;
  double total_employment_remuneration = 0.0;
  double working_life_income = 0.0;
  double income_deflated = income / (1 + wage_index);
  double working_life_income_deflated = 0.0;
  
  int year = starting_year;
  for (int i = 1; i <= n; ++i) {
    cpi *= (1 + long_term_CPI);
    
    if (i <= 5) {
      wage_index = short_run_wage_index[i - 1];
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
    
    salary *= (1 + wage_index);
    super_fee *= (1 + long_term_CPI);
    underlying_super_cap *= (1 + wage_index);
    super_cap = floor(underlying_super_cap / 2500) * 2500;
    
    // contribution (only SG for now)
    if (year < 2021) {
      if (year == 2015){
        underlying_super_cap = 30000;
      }
      if (year == 2016){
        underlying_super_cap = 25000;
      }
    } else {
      if (year < 2024){
        SG_rate = 0.10;
      } else {
        SG_rate = 0.11;
      }
    }
    
    SG_payment = salary * SG_rate;
    
    total_employment_remuneration = salary + SG_payment;
    
    if (contribution > super_cap) {
      salary += contribution - super_cap;
      contribution = super_cap;
    }
    net_contribution = (1 - contribution_tax) * contribution;
    super_balance += net_contribution;
    super_balance -= super_fee;
    double income_tax = 0;
    income_tax = IncomeTax(salary, 
                           NumericVector::create(0, 18200, 37000, 87000, 180000),
                           NumericVector::create(0, 0.19, 0.325, 0.370, 0.450),
                           37000,
                           445,
                           0.015,
                           32279,
                           2230,
                           0.1250,
                           0.02,
                           20542,
                           0.10,
                           false);
    income -= income_tax;
    
    if (age < retirement_age){
      income_deflated = income / (1 + wage_index);
      working_life_income += income;
      working_life_income_deflated += income_deflated;
    } 
    age += 1;
    year += 1;
  }
  return super_balance / (pow(1 + wage_index, n));
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
testthat::expect_equal(1, 1)
*/
