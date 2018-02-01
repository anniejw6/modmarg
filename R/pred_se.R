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
#' @param deriv_func function for the derivative of the predicted outcomes
#' @param link_func function to transform output of `predict` method into
#' response scale
#' @importFrom stats coef predict model.matrix
pred_se <- function(df_levels, model, type, base_rn, vcov_mat, weights,
                    deriv_func, link_func){

  UseMethod("pred_se", model)
}

.pred_se <- function(df_levels, model, type, base_rn, vcov_mat, weights,
                     deriv_func, link_func){


  res <- lapply(df_levels, function(x){
    # Predict function is expensive so just calling it once
    p <- predict(model, newdata = x)

    # Calculate mean values
    preds <- link_func(p)
    preds <- sum(preds * weights)/sum(weights)

    # Get covariate matrix
    covar_matrix <- get_covar(model, data = x)

    list(
      # Calculate Jacobian
      jacobs = calc_jacob(
        pred_values = p,
        covar_matrix = covar_matrix,
        deriv_func = deriv_func,
        weights = weights),
      # Calculate predicted values
      preds = preds
    )
  })

  jacobs <- do.call(rbind, lapply(res, function(x){x[['jacobs']]}))
  preds <- vapply(res, function(x){ x[['preds']]}, numeric(1))

  if(type == 'effects') {
    jacobs <- discrete_effect_jacob(jacobs, base_rn)
    preds <- discrete_effect_pred(preds, base_rn)
  }

  list(
    labels = names(df_levels),
    pred_margins = preds,
    se = calc_pred_se(vcov_mat, jacobs)
  )
}


get_covar <- function(model, data){

  UseMethod("get_covar", model)

}

