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
                     data = mod$data[names(mod$prior.weights), ],
                     weights = mod$prior.weights,
                     vcov_mat = NULL, dof = NULL,
                     ...){

  .marg(mod = mod, var_interest = var_interest,
        data = data,
        weights = weights,
        vcov_mat = vcov_mat, dof = dof,
        ...)
}

get_clean_data.glm <- function(mod, data, weights){

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


