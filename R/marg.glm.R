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
                     data = mod$data,
                     weights = mod$prior.weights,
                     cofint = 0.95){

  # Run checks ----
  check_inputs(weights = weights, data = data, var_interest = var_interest,
               at = at, cofint = cofint, base_rn = base_rn, type = type)

  # Chekcs specific to glm-----
  # Check for polynomials
  if(sum(grepl("poly\\(.*\\)", names(mod$model))) !=
     sum(grepl("raw = T", names(mod$model))))
    warning("If you're using 'poly()' for higher-order terms, ",
            "use the raw = T option (see ?poly)")

  if(is.null(dof) & !is.null(vcov_mat) & mod$family$family == 'gaussian')
    warning(
      "You provided a new variance-covariance matrix for an OLS model ",
      "but no degrees of freedom for the T test. P-value calculations ",
      "may be incorrect - see ?modmarg::marg for details.")

  # Check if no weights when model was built was weights
  if(is.null(weights) & !all(mod$prior.weights == 1))
    warning('The model was built with weights, but you have not ',
            'provided weights. Your calculated margins may be odd. ',
            'See Details.')

  # Set Params ----

  # Remove weights if no weights
  if(all(weights == 1)) weights <- NULL

  # Subset to covariate completes
  data <- data[, names(data) %in% all.vars(mod$formula)]
  complete_cases <- complete.cases(data)

  # Subset based on weights too
  if(!is.null(weights)) complete_cases <- complete_cases & !is.na(weights)

  if(sum(complete_cases) != nrow(data)){
    warning(sprintf('Dropping %s rows due to missing data',
                    nrow(data) - sum(complete_cases)))
    data <- data[complete_cases, ]
    weights <- weights[complete_cases]
  }

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
    pred_se(df_trans = x, var_interest = var_interest,
            model = mod, type = type, base_rn = base_rn,
            at_var_interest = at_var_interest,
            vcov_mat = vcov_mat, weights = weights)
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


