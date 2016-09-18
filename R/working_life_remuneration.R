#' Working life remuneration
#' @param starting_age An integer (thus \code{30L} not \code{30}) given the starting year of the individual.
#' @param retirement_age An integer. Youngest age of retirement age, not oldest age of work.
#' @param start_fy The financial year corresponding to \code{starting_age}.
#' @param wage.method One of \code{"Constant \% of AWOTE"} or \code{"Lifetime percentile"}.
#' @param wage.method.control A \code{list}: \describe{
#' \item{\code{"Constant \% of AWOTE"}}{\describe{
#'    \item{\code{AWOTE.start}}{the AWOTE at \code{starting_age};}
#'    \item{\code{AWOTE.multiple}}{the multiple of AWOTE to use.}
#'    }}
#' \item{\code{"Lifetime percentile"}}{\code{percentile} the percentile to use.}
#' }
#' Using one set of controls in the wrong \code{wage.method} is a warning.
#' @param wage.inflator One of \code{"grattan::wage_inflator()"} or \code{"Treasury"}.
#' @param forecast.series See \code{"grattan::wage_inflator()"}.
#' @param forecast.level See above.
#' @return A \code{data.table} of information about the individual's remuneration from \code{starting_age} till the year before \code{retirement_age}.
#' @export

working_life_remuneration <- function(starting_age = 30L,
                                      retirement_age = 70L,
                                      start_fy = "2015-16",
                                      wage.method = c("Constant % of AWOTE", "Lifetime percentile"),
                                      wage.method.control = list(AWOTE.start = 78200, AWOTE.multiple = 1, percentile = NULL),
                                      wage.inflator = c("grattan::wage_inflator()", "Treasury"),
                                      forecast.series = "mean",
                                      forecast.level = 95) {


  stopifnot(is.integer(starting_age),
            is.integer(retirement_age),
            grattan::is.fy(start_fy))

  working_ages <- seq(from = starting_age, to = retirement_age - 1, by = 1L)
  working_years_ending <- seq(from = grattan::fy2yr(start_fy), length.out = length(working_ages), by = 1L)

  age <- year_ending <- NULL
  input <- data.table(age = working_ages,
                      year_ending = working_years_ending)

  fy_year <- NULL
  input[, fy_year := grattan::yr2fy(year_ending)]


  wage.method <- match.arg(wage.method)
  wage <- SG <- NULL
  switch(wage.method,
         "Constant % of AWOTE" = {
           if (!is.null(wage.method.control$percentile)){
             warning("wage.method.control has percentile set, but wage.method is", wage.method)
           }

           initial.wage <- wage.method.control$AWOTE.start * wage.method.control$AWOTE.multiple

           wage.inflator <- match.arg(wage.inflator)
           if (wage.inflator == "grattan::wage_inflator()"){
             the_wage_inflator <- function(...){
               # forecast struggles to identify the forecast.series if it is not passed like this:
               switch(forecast.series,
                      "mean"  = grattan::wage_inflator(..., forecast.series = "mean", forecast.level = forecast.level),
                      "upper" = grattan::wage_inflator(..., forecast.series = "upper", forecast.level = forecast.level),
                      "lower" = grattan::wage_inflator(..., forecast.series = "lower", forecast.level = forecast.level))
             }
           }

           output <-
             input %>%
             .[, wage := initial.wage * the_wage_inflator(wage = 1, from_fy = start_fy, to_fy = fy_year)] %>%
             # super guarantee
             .[, SG := super_guarantee(fy_year)] %>%
             .[, total_remuneration := wage + SG]


         },
         "Lifetime percentile" = {

         })

  output
}
