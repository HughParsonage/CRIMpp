#' Super balance at retirement
#'
#' @description Determine the superannuation balance at the commencement of retirement of a single individual.
#'
#' @param starting_age An integer (thus \code{30L} not \code{30}) given the starting year of the individual.
#' @param retirement_age An integer. Youngest age of retirement age, not oldest age of work.
#' @param start_fy A financial year corresponding to \code{starting_age}.
#' @param wage.method One of \code{"Constant \% of AWOTE"} or \code{"Lifetime percentile"}.
#' @param wage.method.control A \code{list}: \describe{
#' \item{\code{"Constant \% of AWOTE"}}{\describe{
#'    \item{\code{AWOTE.start}}{the AWOTE at \code{starting_age};}
#'    \item{\code{AWOTE.multiple}}{the multiple of AWOTE to use.}
#'    }}
#' \item{\code{"Lifetime percentile"}}{\code{percentile} the percentile to use.}
#' }
#' Using one set of controls in the wrong \code{wage.method} is a warning.
#' @param starting_balance The balance of the super account when the person's age is \code{starting_age}.
#' @import data.table
#' @importFrom magrittr %>%


super_balance_at_retirement <- function(starting_age = 30L,
                                        retirement_age = 70L,
                                        start_fy = "2015-16",
                                        wage.method = "Constant % of AWOTE",
                                        wage.method.control = list(AWOTE = 1),
                                        starting_balance = 0){
  stopifnot(is.integer(starting_age),
            is.integer(retirement_age),
            grattan::is.fy(start_fy))

  working_ages <- seq(from = starting_age, to = retirement_age - 1, by = 1L)
  working_years_ending <- seq(from = grattan::fy2yr(start_fy), length.out = length(working_ages), by = 1L)

  data.table(age = working_ages,
             year_ending = working_years_ending) %>%
    .#[ total_concessional_contributions := ]



  balance <- 
    shift(balance) +
    total_concessional_contributions +
    total_nonconcessional_contributions +
    LISTO +
    -ordinary_contributions_tax +
    -div293_tax +
    -excess_contributions_tax +
    super_earnings +
    -super_earnings_tax

}
