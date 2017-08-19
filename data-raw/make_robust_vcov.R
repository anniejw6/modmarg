
# Create robust SE variance-covariance matrix for package tests
rm(list = ls())

robust_se <- function(model){
  if(model$family$family %in% c('binomial', 'quasibinomial')){
    sandwich::sandwich(model) * nobs(model) / (nobs(model) - 1)
  } else {
    if(model$family$family != 'gaussian')
      warning(paste('This function has only been tested using gaussian',
                    'and binomial models. Please use with caution!'))
    sandwich::vcovHC(model, "HC1")
  }
}

# Create sample vcov matrix for vcov test

# OLS
data(margex)
margex$treatment <- factor(margex$treatment)
mod <- glm(outcome ~ treatment + distance, data = margex, family = 'gaussian')
ols_rvcov <- robust_se(mod)

# Logit
data(margex)
margex$treatment <- factor(margex$treatment)
mod <- glm(outcome ~ treatment + distance, data = margex, family = 'binomial')
logit_rvcov <- robust_se(mod)

# Polynomial interaction, dropping rows
data(mtcars)
mtcars$am <- factor(mtcars$am)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$cyl[c(1, 10, 20, 31)] <- NA

mod <- glm(mpg ~ cyl * poly(disp, degree = 2, raw = TRUE) + hp,
           data = mtcars)
poly_rvcov <- robust_se(mod)

rvcov <- list(ols = ols_rvcov, logit = logit_rvcov, poly = poly_rvcov)
devtools::use_data(rvcov, overwrite = TRUE)
