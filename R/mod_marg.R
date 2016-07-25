#' Estimating predictive margins on a model
#'
#' @param mod model object, currently only support those of class glm
#' @param var_interest variable of interest
#' @param type either 'levels' (predicted outcomes) or 'effects' (dydx), defaults to 'levels'
#' @param at list, should be in the format of list('var_name' = c(values)), defaults to NULL.
#' This calculates the margins of the variable at these particular variables.
#' @param base_rn if 'effects', this is the base level (this is an index of the ordered unique values in var_interest)
#'
#' @return list of dataframes with predicted margins/effects, se, p-values, and confidence interval bounds
#' @export
#' @examples
#' data(mtcars)
#' mtcars$gear <- factor(mtcars$gear)
#' mod <- glm(vs ~ gear + mpg * disp, data = mtcars, family = 'binomial')
#' mod_marg2(mod, 'gear', 'levels', list(mpg = c(15, 21), disp = c(140, 180)))
#'
#' data(margex)
#' margex$treatment <- factor(margex$treatment)
#' mod <- glm(outcome ~ treatment + distance, data = margex, family = 'binomial')
#' mod_marg2(mod, 'treatment', 'levels', at = NULL)
#' mod_marg2(mod, 'treatment', 'effects', at = NULL)
mod_marg2 <- function(mod, var_interest,
                      type = 'levels',
                      at = NULL, base_rn = 1){

  stopifnot(
    'glm' %in% class(mod),
    var_interest %in% names(mod$model),
    all(names(at) %in% names(mod$model))
    # TODO: warning if at contains extrapolated values
    )

  # Transform the ats ---
  if(!is.null(at)){

    df <- at_transforms(mod$model, at)

  } else {

    df <- list(mod$model)

  }


  # Calculate pred and se ---

  lapply(df, function(x) calc_pred_se(x, var_interest, mod,
                                      type = type, base_rn = base_rn))
}


