#' Artificial data for margins
#'
#' A fictitious dataset outcome, treatment, and demographic variables for 3000
#' observations.
#'
#' @format A data frame with 3000 rows and 11 variables:
#' \describe{
#'   \item{y}{numeric}
#'   \item{outcome}{integer, 0 or 1}
#'   \item{sex}{character: "female" or "male"}
#'   \item{group}{integer}
#'   \item{age}{integer}
#'   \item{distance}{numeric}
#'   \item{ycn}{numeric}
#'   \item{yc}{numeric, 0 or 1}
#'   \item{treatment}{integer}
#'   \item{agegroup}{character: "20-29", "30-39", or "40+"}
#'   \item{arm}{integer}
#' }
#' @source \url{http://www.stata-press.com/data/r14/margex.dta}
"margex"
