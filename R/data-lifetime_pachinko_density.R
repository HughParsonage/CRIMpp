#' @title Lifetime 'Pachinko' Density
#' @description Data table reflecting lifetime densities of incoem
#' @format A \code{data.table} of 800,000 variables (number of age groups \eqn{{}\times 100}):
#' \describe{
#' \item{\code{Percentile}}{The initial percentile.}
#' \item{\code{id}}{ID representing a unique person.}
#' \item{\code{simulation}}{ID representing a unique simulation.}
#' \item{\code{AWOTE_average}}{Average lifetime income, expressed in AWOTE.}
#' }

"lifetime_pachinko_density"
