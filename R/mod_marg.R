#' Estimating predictive margins on a model
#'
#' The variable for the predictive margin is specified by `var_interest`. If
#' margins are only needed at particular values of `var_interest`,
#' `at_var_interest` should be used. If margins of `var_interest` are needed at
#' across the levels of a *different* variable in the model, `at` should be
#' used.
#'
#'
#' @param mod model object, currently only support those of class glm
#' @param var_interest name of the variable of interest, must correspond to a covariate in the model
#' @param type either 'levels' (predicted outcomes) or 'effects' (dydx), defaults to 'levels'
#' @param vcov_mat the variance-covariance matrix, defaults to NULL in which case vcov(model) is used.
#' @param at list, should be in the format of list('var_name' = c(values)), defaults to NULL.
#' This calculates the margins of the variable at these particular variables.
#' If all values are needed, suggested syntax is `at = list(var = unique(df$var))`.
#' @param base_rn numeric, if type == 'effects', the base level (taken as the index of one of
#' the ordered unique values in var_interest). if type == 'levels', this param is ignored.
#' Defaults to 1.
#' @param at_var_interest vector, if type == 'levels', the values for the variable of interest at which levels should be calculated.
#' If NULL, indicates all levels for a factor variable, defaults to NULL
#' @param dof integer, the degrees of freedom used for the T statistic in an OLS model. Defaults to NULL in which case
#' mod$df.residual is used.
#' @param data data.frame that margins should run over, defaults to mod$data
#' @return list of dataframes with predicted margins/effects, se, p-values, and confidence interval bounds
#'
#' @details P values are calculated with T tests for OLS, and Z tests otherwise. If a new variance-covariance matrix is provided
#' (e.g. for clustering standard errors), the degrees of freedom for the T test / p-value calculation may need to be specified
#' using dof. To replicate Stata clustering vce(cluster var_name), dof should be set to g - 1, where g is the number of unique levels
#' of the clustering variable.
#' @importFrom stats complete.cases terms vcov
#' @export
#' @examples
#' data(mtcars)
#' mtcars$gear <- as.character(mtcars$gear)
#' mod <- glm(vs ~ gear + mpg * disp, data = mtcars, family = 'binomial')
#' mod_marg2(mod, var_interest = 'gear',
#'           type = 'levels', at = list(mpg = c(15, 21), disp = c(140, 180)))
#'
#' data(margex)
#' mod <- glm(outcome ~ as.factor(treatment) + distance,
#'        data = margex, family = 'binomial')
#' mod_marg2(mod, var_interest = 'treatment', type = 'levels', at = NULL)
#' mod_marg2(mod, var_interest = 'treatment', type = 'effects', at = NULL)
#' mod_marg2(mod, var_interest = 'distance', type = 'levels',
#'           at = NULL, at_var_interest = c(10, 20, 30))
#'
#' mod <- glm(outcome ~ distance + factor(sex),
#'            data = margex, family = 'binomial')
#' mod_marg2(mod, var_interest = 'sex', type = 'levels', at = NULL)
#'
#' # Using a custom variance-covariance matrix for clustered standard errors
#' # (also requires custom degrees of freedom for T statistic with OLS model)
#' data(margex)
#' data(cvcov)
#' v <- cvcov$ols$clust
#' d <- cvcov$ols$stata_dof
#' mod <- glm(outcome ~ treatment + distance,
#'            data = margex, family = 'binomial')
#' mod_marg2(mod, var_interest = 'treatment', type = 'levels',
#'           vcov_mat = v, dof = d)

mod_marg2 <- function(mod, var_interest,
                      type = 'levels',
                      vcov_mat = NULL,
                      dof = NULL,
                      at = NULL, base_rn = 1,
                      at_var_interest = NULL,
                      data = mod$data){

  stopifnot('glm' %in% class(mod))

  data <- data[, names(data) %in% all.vars(mod$formula)]
  data <- data[complete.cases(data), ]

  stopifnot(
    var_interest %in% names(data),
    all(names(at) %in% names(data))
  )

  if(is.null(dof) & !is.null(vcov_mat) & mod$family$family == 'gaussian')
    warning(paste(
      "You provided a new variance-covariance matrix for an OLS model",
      "but no degrees of freedom for the T test. P-value calculations",
      "may be incorrect - see ?modmarg::mod_marg2 for details."))

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
      dof = dof
    )})

}


