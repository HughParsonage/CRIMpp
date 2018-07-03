#include <Rcpp.h>
#include "BalanceAfterRealLevelPayments.h"
using namespace Rcpp;

// [[Rcpp::export]]
void showOutputt(const char* z, double x) {
  Rcout << z << x << std::endl;
}

// [[Rcpp:export]]
void showRaw(const char* z) {
  Rcout << z;
}

// [[Rcpp:export]]
void showRawDbl (double z) {
  Rcout << z;
}

// [[Rcpp::export]]
double Listo (double adjusted_income, double contributions, double threshold, double max_listo) {
  double out = 0;
  
  if (adjusted_income < threshold) {
    out = 0.15 * contributions;
    if (out > max_listo) {
      out = max_listo;
    }
  }
  return out;
}
//' @title Replacement rate
//' @name ReplacementRate
//' @description Replacement rate.
//' @param starting_age (Default: 30) The age in the first year of the model. Integer.
//' @param retirement_age (Default: 70) Retirement age for the model. Integer.
//' @param wage_method Constant proportion of AWOTE.
//' @param starting_salary The AWOTE for the starting year.
//' @param homeowner Is the person a home-owner?
//' @param house_value What is the value of the home owned by the person?
//' @param death_age Age of death.
//' @param start_fy_ending Year ending of the financial year at the start of the model.
//' @param SG_rate_method Rate of determining the super guarantee.
//' @param drawDownMethod Method of draw down.
//' @param minDrawDown6474,minDrawDown7579,minDrawDown8084,minDrawDown8589,minDrawDown9094,minDrawDown9599 Minimum draw-down rates for the given ages.
//' @param short_run_wage_growth Vector of rates of wage growth for the first five years.
//' @param long_run_wage_growth,long_run_cpi_growth Rate of wage/cpi growth beyond five years.
//' @param initial_super_balance Super balance in first year.
//' @param apply_pretax_cap Should the pre-tax cap be applied?
//' @param super_acct_fixed_fee The fixed fee for the person's super account.
//' @param contributions_tax_rate Rate of contributions tax.
//' @param asset_earnings_accumulation Rate of asset growth in the accumulation phase.
//' @param earnings_tax_accumulation Rate of tax on earnings in the accumulation phase.
//' @param asset_earnings_pension Rate of asset growth in the pension phase.
//' @param earnings_tax_pension Rate of tax on earnings in the pension phase.
//' @param div293_threshold Threshold for Division 293 phase.
//' @param verbose Report progress and internal calculations yearly.
//' @export ReplacementRate
// [[Rcpp::export]]
double ReplacementRate(int starting_age = 30,
                       int retirement_age = 70,
                       std::string wage_method = "Constant prop AWOTE",
                       double starting_salary = 78200,
                       bool homeowner = true,
                       double house_value = 250000,
                       int death_age = 92,
                       int start_fy_ending = 2016,
                       std::string SG_rate_method = "Legislated",
                       std::string drawDownMethod = "Min draw down",
                       NumericVector minDrawDown6474 = NumericVector::create(0.040, 0.040, 0.040, 0.040, 0.040, 0.040, 0.040, 0.040, 0.040, 0.040),
                       NumericVector minDrawDown7579 = NumericVector::create(0.050, 0.050, 0.050, 0.050, 0.050),
                       NumericVector minDrawDown8084 = NumericVector::create(0.060, 0.060, 0.060, 0.060, 0.060),
                       NumericVector minDrawDown8589 = NumericVector::create(0.070, 0.070, 0.070, 0.070, 0.070),
                       NumericVector minDrawDown9094 = NumericVector::create(0.080, 0.080, 0.080, 0.080, 0.080),
                       NumericVector minDrawDown9599 = NumericVector::create(0.110, 0.110, 0.110, 0.110, 0.110),
                       NumericVector short_run_wage_growth = NumericVector::create(0.0225, 0.0250, 0.0275, 0.0325, 0.0350),
                       double long_run_wage_growth = 0.0404,
                       double long_run_cpi_growth = 0.025,
                       double initial_super_balance = 0.0, 
                       bool apply_pretax_cap = false,
                       double super_acct_fixed_fee = 320.0, 
                       double contributions_tax_rate = 0.15,
                       double asset_earnings_accumulation = 0.0650,
                       double earnings_tax_accumulation = 0.15,
                       double asset_earnings_pension = 0.0550,
                       double earnings_tax_pension = 0.0,
                       double div293_threshold = 250000,
                       bool verbose = false) {
  // Preliminary
  NumericVector minDrawDown = NumericVector(minDrawDown6474.length() + minDrawDown7579.length() + minDrawDown8084.length() + minDrawDown8589.length() + minDrawDown9094.length() + minDrawDown9599.length());
  for (int m = 0; m < minDrawDown6474.length(); ++m) {
    minDrawDown[m] = minDrawDown6474[m];
  }
  for (int m = 0; m < minDrawDown7579.length(); ++m) {
    minDrawDown[m] = minDrawDown7579[m];
  }
  for (int m = 0; m < minDrawDown8084.length(); ++m) {
    minDrawDown[m] = minDrawDown8084[m];
  }
  for (int m = 0; m < minDrawDown8589.length(); ++m) {
    minDrawDown[m] = minDrawDown8589[m];
  }
  for (int m = 0; m < minDrawDown9094.length(); ++m) {
    minDrawDown[m] = minDrawDown9094[m];
  }
  for (int m = 0; m < minDrawDown9599.length(); ++m) {
    minDrawDown[m] = minDrawDown9599[m];
  }
  
  
  // Fundamental
  int year = start_fy_ending;
  int age = starting_age;
  
  // Economic parameters
  double wage_index = 1.0;
  double cpi_index = 1.0;
  
  
  // Income
  double salary = starting_salary;
  double SG_rate = 0.095;
  
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
  
  // Welfare calculation
  double PensionBenchmark = 1395.1 * (365 / 7) / 26;
  PensionBenchmark *= wage_index;
  double penMaxRateS = 788.40;
  double penSupTotS = 65.40;
  double penSupMinS = 162.0;
  // pension asset test for single homeowner
  double penAssHOThrS = 202000;
  double penAssNonHOThrS = 345000;
  double raMaxS = 129.40;
  double raThrS = 115.00;
  if (year > 2016) {
    if (penMaxRateS < 0.277 * PensionBenchmark) {
      penMaxRateS = 0.277 * PensionBenchmark;
    }
    penSupTotS *= cpi_index;
    penSupMinS *= cpi_index;
    penAssHOThrS *= cpi_index;
    penAssNonHOThrS *= cpi_index;
  }
  
  double SG_payment = salary * SG_rate;
  double total_employment_remuneration = salary + SG_payment;
  
  // Superannuation -- accumulation phase
  // starting_super_balance [at the start of the year]
  double starting_super_balance = initial_super_balance;
  
  // Super cap grows with wage inflation.  underlying_super_cap
  // is the cap so grown, which will be rounded when actually
  // applied to contributions
  double underlying_super_cap = 30000;
  if (year == 2018) {
    underlying_super_cap = 25000;
  }
  if (year > 2018) {
    underlying_super_cap = 25000 * wage_index;
  }
  
  int pre_tax_contributions_cap = floor(underlying_super_cap / 2500) * 2500;
  // Not used.
  int voluntary_concessional_contributions = 0;
  
  double total_concessional_contributions = SG_payment + voluntary_concessional_contributions;
  double excess_contributions = 0;
  if (apply_pretax_cap && total_concessional_contributions > pre_tax_contributions_cap) {
    excess_contributions = total_concessional_contributions - pre_tax_contributions_cap;
    total_concessional_contributions = pre_tax_contributions_cap;
  } 
  
  double total_nonconcessional_contributions = excess_contributions;
  double super_account_fee = super_acct_fixed_fee;
  
  double listoMax = 500;
  double listoThreshold = 37000;
  
  // ToDo: set LISTO to be changeable for different starting years
  double LISTO = Listo(salary + excess_contributions, 
                       total_concessional_contributions, 
                       listoThreshold, 
                       listoMax);
  double contributions_tax = contributions_tax_rate * total_concessional_contributions;
  double div293_tax = 0;
  if (salary > div293_threshold) {
    div293_tax = 0.15 * (salary - div293_threshold);
  }
  
  double earnings_rate = asset_earnings_accumulation;
  double accumulation_phase_earnings = earnings_rate * initial_super_balance;
  double balance_inflow = (total_concessional_contributions + total_nonconcessional_contributions + LISTO) - contributions_tax + div293_tax;
  accumulation_phase_earnings += (balance_inflow) * (pow(1 + accumulation_phase_earnings, 0.5) - 1);
  double accumulation_phase_earnings_tax = earnings_tax_accumulation * accumulation_phase_earnings;
  double closing_balance_eoy = initial_super_balance;
  closing_balance_eoy += balance_inflow;
  closing_balance_eoy -= accumulation_phase_earnings_tax;
  
  // Non-super assets
  double imputed_nonsuper_savings_prior_retire = 0;
  double opening_nonsuper_balance = 0;
  double nonsuper_earnings = 0;
  double nonsuper_drawdown = 0;
  double nonsuper_assets_eoy = imputed_nonsuper_savings_prior_retire;
  double assumed_house_value = house_value;
  
  
  // Social security
  double assessable_income = salary;
  double pension_entitlement = 0;
  
  // Income tax
  double Taxable_Income = salary + pension_entitlement;
  double Net_personal_income_tax = IncomeTax(Taxable_Income,
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
  
  // working duration
  // i = 0 already set
  int WN = retirement_age - starting_age;
  double working_age_income = total_employment_remuneration;
  double total_retirement_income_average = 0.0;
  double out = 1.0;
  
  if (wage_method == "Constant prop AWOTE") {
    // i = 1 already set
    for (int i = 1; i <= WN; ++i) {
      int j = i - 1;
      age += 1;
      year += 1;
      
      double wage_growth = 1 + long_run_wage_growth;
      if (i <= short_run_wage_growth.length()) {
        wage_growth = 1 + short_run_wage_growth[j];
      }
      wage_index *= wage_growth;
      cpi_index *= 1 + long_run_cpi_growth;
      salary *= wage_growth;
      
      // Pension inflation
      PensionBenchmark *= wage_growth;
      if (penMaxRateS < 0.277 * PensionBenchmark) {
        penMaxRateS = 0.277 * PensionBenchmark;
      }
      
      penSupTotS *= 1 + long_run_cpi_growth;
      penSupMinS *= 1 + long_run_cpi_growth;
      penAssHOThrS *= 1 + long_run_cpi_growth;
      penAssNonHOThrS *= 1 + long_run_cpi_growth;
      
      
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
      
      
      // Income tax
      Taxable_Income = salary + pension_entitlement;
      Net_personal_income_tax = IncomeTax(Taxable_Income,
                                          NumericVector::create(0, 18200, 37000, 87000, 180000) * wage_index,
                                          NumericVector::create(0, 0.19, 0.325, 0.370, 0.450),
                                          37000,
                                          445,
                                          0.015,
                                          32279 * cpi_index,
                                          2230,
                                          0.1250,
                                          0.02,
                                          20542 * cpi_index,
                                          0.10,
                                          false);
      total_employment_remuneration = salary + SG_payment - Net_personal_income_tax;
      total_employment_remuneration /= wage_index;
      
      working_age_income += (total_employment_remuneration - working_age_income) / i;
      
      // Superannuation
      starting_super_balance = closing_balance_eoy;
      
      if (year == 2018) {
        underlying_super_cap = 25000;
      }
      if (year > 2018) {
        underlying_super_cap = 25000 * wage_index;
      }
      
      pre_tax_contributions_cap = floor(underlying_super_cap / 2500) * 2500;
      // Not used.
      voluntary_concessional_contributions = 0;
      
      total_concessional_contributions = SG_payment + voluntary_concessional_contributions;
      excess_contributions = 0;
      if (apply_pretax_cap && total_concessional_contributions > pre_tax_contributions_cap) {
        excess_contributions = total_concessional_contributions - pre_tax_contributions_cap;
        total_concessional_contributions = pre_tax_contributions_cap;
      } 
      
      total_nonconcessional_contributions = excess_contributions;
      super_account_fee = super_acct_fixed_fee;
      
      listoThreshold *= wage_growth;
      listoMax *= wage_growth;
      
      // ToDo: set LISTO to be changeable for different starting years
      LISTO = Listo(salary + excess_contributions, 
                    total_concessional_contributions, 
                    listoThreshold, 
                    listoMax);
      contributions_tax = contributions_tax_rate * total_concessional_contributions;
      div293_tax = 0;
      if (salary > div293_threshold) {
        div293_tax = 0.15 * (salary - div293_threshold);
      }
      
      earnings_rate = asset_earnings_accumulation;
      accumulation_phase_earnings = earnings_rate * starting_super_balance;
      
      balance_inflow = (total_concessional_contributions + total_nonconcessional_contributions + LISTO) - contributions_tax + div293_tax;
      accumulation_phase_earnings += (balance_inflow) * (pow(1 + earnings_rate, 0.5) - 1);
      accumulation_phase_earnings_tax = earnings_tax_accumulation * accumulation_phase_earnings;
      
      closing_balance_eoy += accumulation_phase_earnings;
      closing_balance_eoy += balance_inflow;
      closing_balance_eoy -= accumulation_phase_earnings_tax;
      
      if (verbose) {
        showOutputt("Working-age income:\t", working_age_income);
        showOutputt("Inflow:\t", balance_inflow);
        showOutputt("Earnings:\t", accumulation_phase_earnings);
        showOutputt("Earnings tax:\t", accumulation_phase_earnings_tax);
        showOutputt("Closing balance:\t", closing_balance_eoy);
      }
      
    }
    
    bool alive = true;
    
    earnings_rate = asset_earnings_pension;
    double earnings_tax = earnings_tax_pension;
    double pension_phase_earnings_tax = 0.0;
    double pension_phase_earnings = 0.0;
    double pension_phase_draw_down = 0.0;
    double drawDownRate = minDrawDown[0];
    int i = 0;
    int j = 0;
    int lastDrawDown = minDrawDown.length();
    
    double non_super_assets = 0;
    
    double Assessable_assets = closing_balance_eoy + non_super_assets;
    double Assessable_income = 0.0;
    double age_pension = 0.0;
    
    double total_retirement_income_nominal = 0.0;
    double total_retirement_income_real = 0.0;
    double sinRaMax = 129.4;
    double sinMaxRate = 788.4;
    double sinAssHomeThr = 202000;
    double sinAssNonHomeThr = 348500;
    double pensAssTpr = 0.015;
    double sinIncThr = 162;
    double pensIncTpr = 0.50;
    double sinSupTot = 14.1;
    double sinSupMin = 14.1;
    double sinCesMax = 14.1;
    
    double total_lifetime_retirement_income = 0;
    
    while (alive && j < 6) {
      j += 1;
      age += 1;
      year += 1;
      cpi_index *= 1 + long_run_cpi_growth;
      
      starting_super_balance = closing_balance_eoy;
      pension_phase_draw_down = drawDownRate * starting_super_balance;
      pension_phase_earnings = pension_phase_earnings * starting_super_balance;
      pension_phase_earnings_tax = pension_phase_earnings * earnings_tax;
      
      // Non-super assets
      // imputed_nonsuper_savings_prior_retire = ;
      opening_nonsuper_balance = nonsuper_assets_eoy;
      nonsuper_earnings = earnings_rate * opening_nonsuper_balance;
      nonsuper_drawdown = drawDownRate * opening_nonsuper_balance;
      nonsuper_assets_eoy = opening_nonsuper_balance + nonsuper_earnings - nonsuper_drawdown;
      assumed_house_value = house_value * wage_index;
      
      Assessable_income = pension_phase_draw_down;
      Assessable_assets = closing_balance_eoy + non_super_assets;
      
      if (verbose) {
        showOutputt("Ass. Inc:\t", Assessable_income);
        showOutputt("Ass. Ass:\t", Assessable_assets);
      }
      
      // Age pension
      sinRaMax *= (1 + long_run_cpi_growth);
      penMaxRateS *= (1 + long_run_cpi_growth);
      PensionBenchmark *= (1 + long_run_wage_growth);
      if (penMaxRateS < 0.277 * PensionBenchmark) {
        penMaxRateS = 0.277 * PensionBenchmark;
      }
      
      // double sinRaMax = 129.4,
      //   double sinMaxRate = 788.4,
      //   double sinAssHomeThr = 202000,
      //   double sinAssNonHomeThr = 348500,
      //   double pensAssTpr = 0.015,
      //   double sinIncThr = 162,
      //   double pensIncTpr = 0.50,
      //   double sinSupTot = 14.1,
      //   double sinSupMin = 14.1,
      //   double sinCesMax = 14.1) {
      age_pension = AgePension(Assessable_assets, 
                               Assessable_income,
                               homeowner,
                               age,
                               year,
                               sinRaMax, 
                               penMaxRateS,
                               penAssHOThrS,
                               penAssNonHOThrS,
                               pensAssTpr,
                               pensIncTpr,
                               penSupTotS,
                               penSupMinS,
                               sinCesMax);
      age_pension = AgePension(Assessable_assets, Assessable_income, homeowner, age, year,
                               sinRaMax, penMaxRateS, penAssHOThrS,
                               penAssNonHOThrS, pensAssTpr,
                               pensIncTpr, penSupTotS);
      age_pension *= 26.25;
      
      if (verbose) {
        showOutputt("Age pension:\t", age_pension);
      }
      
      total_retirement_income_nominal = age_pension + Assessable_income;
      
      total_retirement_income_real = total_retirement_income_nominal / wage_index;
      total_retirement_income_average += (total_retirement_income_real - total_retirement_income_average) / j;
      total_lifetime_retirement_income += total_retirement_income_real;
      if (verbose) {
        showOutputt("Avg Ret. inc:\t", total_retirement_income_average);
      }
      
      closing_balance_eoy -= pension_phase_draw_down;
      closing_balance_eoy -= pension_phase_earnings_tax;
      closing_balance_eoy *= (1 + earnings_rate);
      
      // at end:
      if (age >= death_age) {
        alive = false;
      }
      
      if (i == lastDrawDown) {
        drawDownRate = minDrawDown[i];
      }
      
      i += 1;
    }
    
    out = total_retirement_income_real / working_age_income;
    
  }
  
  
  return out;
}



