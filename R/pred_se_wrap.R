#' Calculate margins and se
#'
#' For one set of transformed covariates (not including the variable of interest),
#' calculate the predicted level and se for the variable of interest.
#'
#' @param df_trans data.frame, should already be transformed for variables not related to
#' the variable of interest
#' @param var_interest the variable of interest
#' @param model model
#' @param type either effects or levels, defaults to levels
#' @param base_rn row number of the base level, defaults to 1
#' @param vcov_mat variance-covariance matrix, defaults to NULL
#' @param at_var_interest vector, if type == 'levels', the values for the variable of interest at which levels should be calculated. if NULL, indicates all levels for a
#' factor variable, defaults to NULL
#'
#' @importFrom stats coef
#'
#' @return dataframe of formatted output
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars$gear <- factor(mtcars$gear)
#' mm <- glm(vs ~ gear + mpg * disp, mtcars, family = 'binomial')
#' # apply at transformations
#' df <- at_transforms(mm$model, list("mpg" = c(15, 21)))
#' df <- df[[1]]
#' pred_se_wrap(df, var_interest = 'gear', model = mm, vcov_mat = vcov(mm))
#' pred_se_wrap(mm$model, var_interest = 'mpg',
#'                 at_var_interest = c(15, 21), model = mm,
#'                 vcov_mat = vcov(mm))
pred_se_wrap <- function(df_trans, var_interest, model,
                         type = 'levels', base_rn = 1,
                         at_var_interest = NULL,
                         vcov_mat = NULL){

  stopifnot(is.data.frame(df_trans),
            is.character(var_interest),
            var_interest %in% names(df_trans),
            type %in% c('effects', 'levels'))

  df_levels <- at_transforms(
    df_trans, gen_at_list(df_trans, var_interest, at_var_interest))

  # Get predicted values and covariates
  cov_preds <- lapply(df_levels, function(x)
    predict_modelmat(model = model, transformed_df = x))

  # calculate predictions
  preds <- sapply(cov_preds, function(x){ mean(x$pred_resp) })

  # if covariates are dropped from the model, remove those columns from cov_preds
  for(i in 1:length(cov_preds)){
    cov_preds[[i]]$covar <- cov_preds[[i]]$covar[, !is.na(coef(model))]
  }

  # calculate jacobian using first derivative
  jacobs <- do.call(rbind, lapply(cov_preds, function(x){
    calc_jacob(
      pred_values = x$pred_link, covar_matrix = x$covar,
      deriv_func = model$family$mu.eta)
  }))

  if(type == 'effects') {
    if(is.numeric(df_trans[[var_interest]]) &
       ! sprintf("as.character(%s)", var_interest) %in% names(model$model) &
       ! sprintf("as.factor(%s)", var_interest) %in% names(model$model)){
      stop('We do not support effects for continuous variables at this time.')
    } else {
      jacobs <- discrete_effect_jacob(jacobs, base_rn)
      preds <- discrete_effect_pred(preds, base_rn)
    }
  }

  format_output(
    margin_labels = names(cov_preds),
    pred_margins = preds,
    se = calc_pred_se(vcov_mat, jacobs),
    family = model$family$family,
    dof = model$df.residual
  )

}


