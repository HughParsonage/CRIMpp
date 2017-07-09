
#include <Rcpp.h>
#include "BalanceAfterRealLevelPayments.h"
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

//' @title Working Life Income
//' @name AverageWorkingLifeIncome
//' @param prop_of_AWOTE_start_year The proportion of AWOTE of the subject in the \code{starting_year}.
//' @param wage_method 'Constant prop AWOTE'.
//' @param starting_age The age at which the model begins. Set to 30 by default, rather than a younger age
//'   to account for time out of the workforce.
//' @param deflator What deflator should be used?
//' @param Starting age.
//' @param Retirement age.
//' @param AWOTE_in_starting_year The salary in starting year.
//' @param short_run_wage_growth A vector of wage indices.
//' @param long_run_wage_growth Rate of wage growth beyond short_run_wage_growth.
//' @param starting_year The year to begin. Used for determining superannuation guarantee.
//' @export AverageWorkingLifeIncome average_working_life_income
// [[Rcpp::export]]
double AverageWorkingLifeIncome (double prop_of_AWOTE_start_year = 1,
                                 std::string wage_method = "Constant prop AWOTE",
                                 std::string deflator = "wage",
                                 std::string scope = "entire",
                                 int starting_age = 30,
                                 int retirement_age = 70,
                                 double AWOTE_in_starting_year = 78200.00,
                                 NumericVector short_run_wage_growth = NumericVector::create(0.0225, 0.0250, 0.0275, 0.0325, 0.0350),
                                 double long_run_wage_growth = 0.0404,
                                 double long_run_cpi_growth = 0.025,
                                 int starting_year = 2016, 
                                 bool verbose = false) {
  double salary = prop_of_AWOTE_start_year * AWOTE_in_starting_year;
  double tax = 0.0;
  // int age = starting_age;
  int year = starting_year;
  int WN = retirement_age - starting_age;
  double out = salary;
  std::string SG_rate_method = "Legislated";
  double SG_rate = 0.09;
  double SG_payment = 0.0;
  double total_employment_remuneration = 0.0;
  double wage_index = 1;
  double cpi_index = 1;
  
  bool first = true;
  for (int i = 1; i <= WN; ++i) {
    if (SG_rate_method == "Legislated") {
      if (year >= 2026) {
        SG_rate = 0.12;
      } else {
        if (year < 2022) {
          SG_rate = 0.095;
        } else {
          SG_rate = 0.095 + 0.005 * (year - 2022);
        }
      }
    }
    
    SG_payment = salary * SG_rate;
    tax = IncomeTax(salary,
                    NumericVector::create(0, 18200, 37000, 87000, 180000),
                    NumericVector::create(0, 0.19, 0.325, 0.370, 0.450));
    total_employment_remuneration = salary - tax;
    
    
    if (first) {
      out = total_employment_remuneration;
    } else {
      out *= i;
      out += total_employment_remuneration / wage_index;
      out /= (i + 1);
    }
    
    // Inflate at end of year
    
    double wage_growth = 1 + long_run_wage_growth;
    if (i < short_run_wage_growth.length()) {
      wage_growth = 1 + short_run_wage_growth[i - 1];
    }
    wage_index *= wage_growth;
    cpi_index *= 1 + long_run_cpi_growth;
    salary *= wage_growth;
    
    if (verbose && first) {
      showRaw("Year\tSalary\tSGP\tTot\tOut\twageG\n");
    }
    
    if (verbose) {
      showRawDbl(year);
      showRaw("\t");
      showRawDbl(floor(salary));
      showRaw("\t");
      showRawDbl(floor(SG_payment));
      showRaw("\t");
      showRawDbl(floor(total_employment_remuneration));
      showRaw("\t");
      showRawDbl(floor(out));
      showRaw("\t");
      showRawDbl(floor(1000 * wage_index));
      showRaw("\t");
      showRawDbl(floor((salary / wage_index)));
      showRaw("\n");
    }
    
    // finally
    first = false;
    year += 1;
  }
  if (verbose) {
    showRaw("Year\tSalary\tSGP\tTot\tOut\twageG\n");
  }
  
  // if (deflator == "wage") {
  //   out /= wage_index;
  // }
  
  return out;
}

// [[Rcpp::export]]
NumericVector average_working_life_income(NumericVector prop_of_AWOTE_start_year = 1,
                                          std::string wage_method = "Constant prop AWOTE",
                                          std::string deflator = "wage",
                                          std::string scope = "entire",
                                          int starting_age = 30,
                                          int retirement_age = 70,
                                          double AWOTE_in_starting_year = 78200.00,
                                          NumericVector short_run_wage_growth = NumericVector::create(0.0225, 0.0250, 0.0275, 0.0325, 0.0350),
                                          double long_run_wage_growth = 0.0404,
                                          double long_run_cpi_growth = 0.025,
                                          int starting_year = 2016, 
                                          bool verbose = false) {
  int n = prop_of_AWOTE_start_year.length();
  NumericVector out(n);
  for (int k = 0; k < n; ++k) {
    out[k] = AverageWorkingLifeIncome(prop_of_AWOTE_start_year[k]);
  }
  return out;
}
