#' The rate of the superannuation guarantee
#' @param fy_year The financial years for which the super guarantee is desired.
#' @return The super guarantee for that year. See Details.
#' @details Currently returns 9.5\% for all years.
#' @export

super_guarantee <- function(fy_year){
  0.095
}
