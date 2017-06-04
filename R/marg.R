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
#' This function currently only supports \code{\link[stats]{glm}} objects. If you would like to
#' use \code{lm} objects, consider running a \code{glm} with family
#' \code{gaussian}.
#'
#' @importFrom stats complete.cases terms vcov
#' @export
#' @examples
#'
#' data(mtcars)
#' mod <- glm(vs ~ as.factor(gear) + mpg, data = mtcars, family = 'binomial')
#'
#' # Get the level of the outcome variable at different values of `gear`
#' marg(mod, var_interest = 'gear', type = 'levels')
#'
#' # Get the effect of `gear` on the outcome value, holding values of `mpg`
#' # constant
#' marg(mod, var_interest = 'gear', type = 'effects',
#'      at = list(mpg = c(15, 21)))
#'
#'
#' data(margex)
#' mod <- glm(outcome ~ as.factor(treatment) + distance,
#'        data = margex, family = 'binomial')
#' # Get the level of the outcome variable at different values of `treatment`
#' marg(mod, var_interest = 'treatment', type = 'levels', at = NULL)
#' # Get the effect of `treatment` on the outcome variable
#' marg(mod, var_interest = 'treatment', type = 'effects', at = NULL)
#' # Get the level of the outcome variable at different values of `distance`
#' marg(mod, var_interest = 'distance', type = 'levels',
#'           at = NULL, at_var_interest = c(10, 20, 30))
#'
#' # Using a custom variance-covariance matrix for clustered standard errors
#' # (also requires custom degrees of freedom for T statistic with OLS model),
#' # clustering on the "arm" variable
#'
#' data(margex)
#' data(cvcov)
#' ?cvcov
#' v <- cvcov$ols$clust
#' d <- cvcov$ols$stata_dof
#' mod <- glm(outcome ~ treatment + distance,
#'            data = margex, family = 'binomial')
#' marg(mod, var_interest = 'treatment', type = 'levels',
#'           vcov_mat = v, dof = d)
#'
marg <- function(mod, var_interest,
                      type = 'levels',
                      vcov_mat = NULL,
                      dof = NULL,
                      at = NULL, base_rn = 1,
                      at_var_interest = NULL,
                      data = mod$data,
                 cofint = 0.95){

  stopifnot('glm' %in% class(mod))

  data <- data[, names(data) %in% all.vars(mod$formula)]
  data <- data[complete.cases(data), ]

  if(sum(grepl("poly\\(.*\\)", names(mod$model))) !=
     sum(grepl("raw = T", names(mod$model))))
    warning(paste("If you're using 'poly()' for higher-order terms,",
                  "use the raw = T option (see ?poly)"))

  stopifnot(
    var_interest %in% names(data),
    all(names(at) %in% names(data)),
    is.numeric(cofint),
    cofint < 1, cofint > 0
  )

  if(is.null(dof) & !is.null(vcov_mat) & mod$family$family == 'gaussian')
    warning(paste(
      "You provided a new variance-covariance matrix for an OLS model",
      "but no degrees of freedom for the T test. P-value calculations",
      "may be incorrect - see ?modmarg::marg for details."))

  if(is.null(vcov_mat))
    vcov_mat <- vcov(mod)

  if(is.null(dof))
    dof <- mod$df.residual

  # Check for extrapolated values
  for(i in seq_along(at)){
    if(is.numeric(data[[names(at)[i]]]) &
       ! all(at[[i]] <= max(data[[names(at)[i]]]) &
             at[[i]] >= min(data[[names(at)[i]]])))
      warning(sprintf("Not all values in 'at' are in the range of '%s'",
                      names(at)[i]))
  }

  # Warn if base_rn set but type != 'effects'
  if(base_rn != 1 & type != 'effects')
    warning(paste("Setting base_rn when type == 'levels' is ignored."))

  # Transform the ats ---
  if(!is.null(at)){

    data <- at_transforms(data, at)

  } else {

    data <- list(data)

  }


  # Calculate pred and se ---
  res <- lapply(data, function(x){
    pred_se(df_trans = x, var_interest = var_interest,
            model = mod, type = type, base_rn = base_rn,
            at_var_interest = at_var_interest,
            vcov_mat = vcov_mat)
  })

  lapply(res, function(x) {
    format_output(
      margin_labels = x$labels,
      pred_margins = x$pred_margins,
      se = x$se,
      family = mod$family$family,
      dof = dof,
      cofint = c( (1 - cofint)/2, 1 - (1 - cofint)/2 )
    )})

}


