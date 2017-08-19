#' Robust variance-covariance matrices and T statistic d.o.f.
#'
#' Variance-covariance matrices with robust standard errors and
#' degrees-of-freedom for T statistics, for tests and examples specifying \code{vcov}.
#' Generated with \code{margex} data in this package.
#'
#' See data-raw/make_robust_vcov.R for details.
#'
#' @format A list of three lists, from an OLS model, logit model, and OLS with a polynomial
#' interaction with missing data, each containing
#' \describe{
#'   \item{clust}{3-by-3 variance-covariance matrix}
#'   \item{dof}{integer, degrees of freedom for the T statistic}
#' }
#'
