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
                 data = mod$data[names(mod$prior.weights), ],
                 weights = mod$prior.weights,
                 cofint = 0.95){

  stopifnot(type %in% c('levels', 'effects'))


  # Set params and Run checks ----
  # Remove weights if no weights
  if(all(weights == 1)) weights <- NULL

  check_inputs(weights = weights, data = data, var_interest = var_interest,
               at = at, cofint = cofint, base_rn = base_rn, type = type,
               dof = dof, vcov_mat = vcov_mat)

  # Chekcs specific to glm-----
  # See if we're looking for continuous variables
  if(type == 'effects' & is.numeric(data[[var_interest]]) &
     ! all(unique(data[[var_interest]]) %in% c(0, 1)) &
     ! sprintf("as.character(%s)", var_interest) %in% names(mod$mod) &
     ! sprintf("as.factor(%s)", var_interest) %in% names(mod$mod))
    stop('We do not support effects for continuous variables at this time.')

  # Check if no weights when model was built was weights
  if(is.null(weights) & !all(mod$prior.weights == 1))
    warning('The model was built with weights, but you have not ',
            'provided weights. Your calculated margins may be odd. ',
            'See Details.')

  # Keep only complete variables ---
  data <- clean_glm_data(mod, data, weights)
  # Add weights back
  if(!is.null(data$`_weights`)) weights <- data$`_weights`

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

  # Transform the ats ---
  if(!is.null(at)){

    data <- at_transforms(data, at)

  } else {

    data <- list(data)

  }


  # Calculate pred and se ---
  res <- lapply(data, function(x){

    df_levels <- at_transforms(
      model_df = x,
      at_list = gen_at_list(df = x, var_interest = var_interest,
                            at_var_interest = at_var_interest))


    pred_se(df_levels = df_levels,
            model = mod, type = type, base_rn = base_rn,
            vcov_mat = vcov_mat, weights = weights,
            link_func = mod$family$linkinv,
            deriv_func = mod$family$mu.eta)
  })

  # Format Output ----
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

