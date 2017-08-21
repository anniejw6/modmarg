#' @export
#' @rdname marg
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
#' # Using weights
#'
#' data(margex)
#' mm <- glm(y ~ as.factor(treatment) + age, data = margex, family = 'gaussian',
#'           weights = distance)
#' z1 <- marg(mod = mm, var_interest = 'treatment', type = 'levels')[[1]]
#' z2 <- marg(mod = mm, var_interest = 'treatment', type = 'effects')[[1]]
marg.glm <- function(mod, var_interest,
                     type = 'levels',
                     vcov_mat = NULL,
                     dof = NULL,
                     at = NULL, base_rn = 1,
                     at_var_interest = NULL,
                     data = mod$data[names(mod$prior.weights), ],
                     weights = mod$prior.weights,
                     cofint = 0.95){

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

