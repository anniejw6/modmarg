#' @export
#' @rdname marg
#' @examples
#'
#' # --------------------------------------
#' # 2SLS using AER::ivreg (example adapted from ?AER::ivreg)
#'
#' data(margex)
#' margex$assign <- margex$treatment
#'
#' # Probability of getting treatment increases with age and distance
#' margex$pr_treat <- plogis(
#'   margex$age / sd(margex$age) +
#'   margex$distance / sd(margex$distance))
#'
#' # One-way non-compliance
#' margex$actual <- margex$assign
#' margex$actual[margex$assign == 1] <- rbinom(
#'   n = sum(margex$assign == 1), size = 1,
#'   prob = margex$pr_treat[margex$assign == 1]
#' )
#'
#' mod <- AER::ivreg(y ~ as.factor(actual) + age + distance |
#'                     as.factor(assign) + age + distance,
#'                   data = margex)
#'
#'
#' v <- function(object, ...){
#'   vcov(object) * object$df.residual / nobs(object)
#' }
#'
#' summary(mod, vcov = v)
#'
#' # Get the level of the outcome variable at different values of `actual`
#' # Note data is a *required* parameter here
#' marg(mod, var_interest = 'actual', data = margex, type = 'levels')
#' marg(mod, var_interest = 'actual', data = margex, type = 'effects')
#'
marg.ivreg <- function(mod, var_interest,
                       type = 'levels',
                       vcov_mat = NULL,
                       dof = NULL,
                       at = NULL, base_rn = 1,
                       at_var_interest = NULL,
                       data,
                       weights = mod$weights,
                       cofint = 0.95){
  # Set params and Run checks ----

  check_inputs(weights = weights, data = data, var_interest = var_interest,
               at = at, cofint = cofint, base_rn = base_rn, type = type,
               dof = dof, vcov_mat = vcov_mat)

  # Checks specific to ivreg ----
  # See if we're looking for continuous variables
  if(type == 'effects' & is.numeric(data[[var_interest]]) &
     ! all(unique(data[[var_interest]]) %in% c(0, 1)) &
     ! sprintf("as.character(%s)", var_interest) %in% names(data) &
     ! sprintf("as.factor(%s)", var_interest) %in% names(data))
    stop('We do not support effects for continuous variables at this time.')

  # Check if no weights when model was built was weights
  if(is.null(weights) & !is.null(mod$weights))
    warning('The model was built with weights, but you have not ',
            'provided weights. Your calculated margins may be odd. ',
            'See Details.')

  # Keep only complete variables ----
  data <- clean_glm_data(mod, data, weights)

  # Add weights back ----
  if(!is.null(data$`_weights`)) weights <- data$`_weights`

  if(is.null(vcov_mat))
    vcov_mat <- vcov.ivreg(mod)

  if(is.null(dof))
    dof <- Inf

  # Check for extrapolated values ----
  for(i in seq_along(at)){
    if(is.numeric(data[[names(at)[i]]]) &
       ! all(at[[i]] <= max(data[[names(at)[i]]]) &
             at[[i]] >= min(data[[names(at)[i]]])))
      warning(sprintf("Not all values in 'at' are in the range of '%s'",
                      names(at)[i]))
  }

  # Transform the ats ----
  if(!is.null(at)){
    data <- at_transforms(data, at)
  } else {
    data <- list(data)
  }

  # Calculate pred and se ----
  res <- lapply(data, function(x){

    df_levels <- at_transforms(
      model_df = x,
      at_list = gen_at_list(df = x, var_interest = var_interest,
                            at_var_interest = at_var_interest))


    pred_se(df_levels = df_levels,
            model = mod, type = type, base_rn = base_rn,
            vcov_mat = vcov_mat, weights = weights)
  })

  # Format Output ----
  lapply(res, function(x) {
    format_output(
      margin_labels = x$labels,
      pred_margins = x$pred_margins,
      se = x$se,
      family = 'gaussian',
      dof = dof,
      cofint = c( (1 - cofint)/2, 1 - (1 - cofint)/2 )
    )})

}

clean_glm_data <- function(mod, data, weights){

  # Store original number of rows
  nrow_orig <- nrow(data)

  # Grab only necessary variables
  data <- get_all_vars(mod, data)

  # Add weights
  if('_weights' %in% all.vars(mod$formula))
    stop("You cannot use the name '_weights' in the model formula. ",
         "Please rename to another variable.")
  data$`_weights` <- weights

  # Keep completes only
  data <- na.omit(data)

  # Remove any booleans
  if(all(data$`T` == TRUE))
    data$`T` <- NULL
  if(all(data$`F` == FALSE))
    data$`F` <- NULL

  # Throw warning if rows were dropped
  if(nrow(data) != nrow_orig)
    warning(sprintf('Dropping %s rows due to missing data',
                    nrow_orig - nrow(data)))

  data

}

vcov.ivreg <- function(mod){
  # Method taken from unexported AER::vcov.ivreg method
  # (https://github.com/cran/AER/blob/master/R/ivreg.R)
  # and modified to match Stata dof correction
  mod$sigma^2 * mod$cov.unscaled * mod$df.residual / mod$nobs
}
