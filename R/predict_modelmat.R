#' Generate predictions and model matrix
#'
#' @param model model object
#' @param transformed_df df after applying whatever transform you want
#' @param formula model formula, defaults to model$formula
#'
#' @return list of covariates (covar) and predictions (pred)
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars$gear <- factor(mtcars$gear)
#' mm <- glm(vs ~ gear + mpg * disp, mtcars, family = 'binomial')
#'
#' df3 <- apply_transform(df = mtcars, var_name = 'gear', value = 3)
#' covar3 <- model.matrix(mm$formula, df3)
#' p3 <- predict(mm, newdata = df3)
#'
#' z <- predict_modelmat(model = mm, transformed_df = df3)
#' expect_identical(z$covar, covar3)
#' expect_identical(z$pred, p3)
predict_modelmat <- function(model, transformed_df,
                             formula = model$formula){

  list(
    covar =  model.matrix(formula, transformed_df),
    pred = predict(model, newdata = transformed_df)
  )

}
