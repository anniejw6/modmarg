#' Estimating predictive margins on a model
#'
#' @param mod model object, currently only support those of class glm
#' @param var_interest variable of interest
#' @param type either 'levels' (predicted outcomes) or 'effects' (dydx), defaults to 'levels'
#' @param vcov_mat the variance-covariance matrix, defaults to vcov(model)
#' @param at list, should be in the format of list('var_name' = c(values)), defaults to NULL.
#' This calculates the margins of the variable at these particular variables.
#' @param base_rn numeric, if type == 'effects', the base level (taken as the index of one of
#' the ordered unique values in var_interest). if type == 'levels', this param is ignored.
#' Defaults to 1.
#' @param at_var_interest vector, if type == 'levels', the values for the variable of interest at which levels should be calculated. if NULL, indicates all levels for a
#' factor variable, defaults to NULL
#' @return list of dataframes with predicted margins/effects, se, p-values, and confidence interval bounds
#' @export
#' @examples
#' data(mtcars)
#' mtcars$gear <- factor(mtcars$gear)
#' mod <- glm(vs ~ gear + mpg * disp, data = mtcars, family = 'binomial')
#' mod_marg2(mod, var_interest = 'gear',
#'           type = 'levels', at = list(mpg = c(15, 21), disp = c(140, 180)))
#'
#' data(margex)
#' margex$treatment <- factor(margex$treatment)
#' mod <- glm(outcome ~ treatment + distance, data = margex, family = 'binomial')
#' mod_marg2(mod, var_interest = 'treatment', type = 'levels', at = NULL)
#' mod_marg2(mod, var_interest = 'treatment', type = 'effects', at = NULL)
#' mod_marg2(mod, var_interest = 'distance', type = 'levels',
#'           at = NULL, at_var_interest = c(10, 20, 30))
mod_marg2 <- function(mod, var_interest,
                      type = 'levels',
                      vcov_mat = vcov(mod),
                      at = NULL, base_rn = 1,
                      at_var_interest = NULL){

  df <- mod$data[, all.vars(mod$formula)]
  df <- df[complete.cases(df), ]
  
  stopifnot(
    'glm' %in% class(mod),
    var_interest %in% names(df),
    all(names(at) %in% names(df))
    # TODO: warning if at contains extrapolated values
    )
  if( any(grepl('factor', attr(terms(mod$formula), 'term.labels'))) )
    stop('Must create all factors OUTSIDE of the model formula (see example in documentation)')

  # Transform the ats ---
  if(!is.null(at)){

    df <- at_transforms(df, at)

  } else {

    df <- list(df)

  }


  # Calculate pred and se ---
  if(!is.numeric(df[[var_interest]])){

    return(
      lapply(df,
             function(x) discrete_wrap(df_trans = x, var_interest = var_interest,
                                       model = mod, type = type, base_rn = base_rn,
                                       at_var_interest = at_var_interest,
                                       vcov_mat = vcov_mat) )
    )
  } else {
    
    return(
      lapply(df,
             function(x) continuous_wrap(df_trans = x, var_interest = var_interest,
                                         model = mod, type = type,
                                         at_var_interest = at_var_interest,
                                         vcov_mat = vcov_mat) )
    )
    

  }

}


