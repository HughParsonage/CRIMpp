#' @title Income Transition Table
#' @name income_transition_table
#' @description Probability of moving from one decile to another.
#' @format Data.table of 10,000 observations:
#' \describe{
#' \item{from,to}{Lifetime income percentiles from and to.}
#' \item{value}{Probability of moving from \code{from} to \code{to}.}
#' \item{cumvalue}{Cumulative probability, by \code{from}.}
#' }

"income_transition_table"