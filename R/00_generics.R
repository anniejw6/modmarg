#' Estimating predictive margins on a model
#'
#' This function estimates the predictive effects and levels for variables within
#' a model using the delta method.
#'
#' @param mod model object, currently only support those of class \code{\link[stats]{glm}}
#' @param var_interest name of the variable of interest, must correspond to a
#' covariate in the model
#' @param type either \code{'levels'} (predicted outcomes) or \code{'effects'} \eqn{dydx},
#' defaults to \code{'levels'}
#' @param vcov_mat the variance-covariance matrix, defaults to \code{NULL} in which
#' case \code{vcov(model)} is used.
#' @param at list, should be in the format of \code{list('var_name' = c(values))},
#' defaults to \code{NULL}. This calculates the margins of the variable at these
#' particular variables. If all values are needed, suggested syntax is
#' \code{at = list('var' = unique(df$var))}.
#' @param base_rn numeric, if \code{type == 'effects'}, the base level (taken as the
#' index of one of the ordered unique values in \code{var_interest}). if
#' \code{type == 'levels'}, this parameter is ignored. Defaults to 1.
#' @param at_var_interest vector, if type == 'levels', the values for the
#' variable of interest at which levels should be calculated.
#' If \code{NULL}, indicates all levels for a factor variable, defaults to \code{NULL}
#' @param dof integer, the degrees of freedom used for the T statistic in an
#' OLS model. Defaults to NULL in which case \code{mod$df.residual} is used.
#' @param data data.frame that margins should run over, defaults to
#' \code{mod$data}
#' @param cofint numeric, confidence interval (must be less than 1), defaults to 0.95
#' @param weights numeric, vector of weights used to generate predicted levels,
#' defaults to \code{mod$prior.weights}. Must be equal to the number of rows
#' in \code{data}, which means that if there are missing values, in \code{data},
#' then the default will not work because \code{prior.weights} are the weights
#' after subsetting.
#'
#' @return list of dataframes with predicted margins/effects, standard errors, p-values,
#' and confidence interval bounds
#'
#' @details
#' The variable for the predictive margin is specified by \code{var_interest}. If
#' margins are only needed at particular values of \code{var_interest},
#' \code{at_var_interest} should be used. If margins of \code{var_interest} are
#' needed at across the levels of a \emph{different} variable in the model,
#' \code{at} should be used.
#'
#' If higher-order polynomial terms (e.g. \eqn{y ~ x + x^2}) are added
#' using the R function \code{\link[stats]{poly}}, the \code{raw = TRUE}
#' argument should be used to include the basic polynomial terms
#' instead of orthogonal polynomial terms. If orthogonal polynomials are used,
#' \code{marg} will fail when the user specifies \code{at} for a small set
#' of values for the variable in question (e.g. \code{at = list(x = 10)}),
#' since \code{poly} needs more data to calculate orthogonal polynomials
#' (e.g. \code{poly(10, 2)} fails, but \code{poly(c(10, 8, 3), 2)} will run).
#'
#' P values are calculated with T tests for gaussian families, and Z tests
#' otherwise. If a new variance-covariance matrix is provided (e.g. for
#' clustering standard errors), the degrees of freedom for the T test / p-value
#' calculation may need to be specified using \code{dof}. To replicate Stata clustering
#' \code{vce(cluster var_name)}, \code{dof} should be set to \eqn{g - 1}, where g is
#' the number of unique levels of the clustering variable.
#'
#' This function currently only supports \code{\link[stats]{glm}} objects.
#' If you would like to use \code{lm} objects, consider running a \code{glm}
#' with family \code{gaussian}.
#'
#' When calculating predicted levels and effects for models built using weights,
#' \code{marg} returns weighted averages for levels and effects by default.
#' Users can remove this option by setting \code{weights = NULL}.
#'
#' @importFrom stats complete.cases terms vcov
#' @export
marg <- function(mod, var_interest,
                 type = 'levels',
                 vcov_mat = NULL,
                 dof = NULL,
                 at = NULL, base_rn = 1,
                 at_var_interest = NULL,
                 data = NULL,
                 weights = mod$prior.weights,
                 cofint = 0.95){

  stopifnot(type %in% c('levels', 'effects'))

  UseMethod("marg", mod)

}


#' Main wrapper function to calculate margins and standard errors
#'
#' For one set of transformed covariates (not including the variable of
#' interest), calculate the predicted level and standard error for the
#' variable of interest.
#'
#' @param df_levels data.frame, already transformed for variables not related
#'                  to the variable of interest
#' @param model model object
#' @param type either effects or levels
#' @param base_rn numeric, row number of the base level
#' @param vcov_mat matrix, variance-covariance matrix
#' @param weights vector of weights, or NULL
#' @importFrom stats coef predict model.matrix
pred_se <- function(df_levels, model, type, base_rn, vcov_mat, weights){

  UseMethod("pred_se", model)

}

