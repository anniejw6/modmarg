# modmarg
predicted margins and marginals effects

# v1.0

* Can take the predictive levels of a continuous or discrete variable
* Can take the predictive effects (dydx) of a discrete variable
* Supports interactions
* Models: OLS, Logit
* Test suite

# v2.0

* Can take the predictive effects (dydx) of a continuous variable
* Inverse probability weights
* Clustered standard errors

# TODO

* More model types (e.g., that aren't already part of glm)

# Random notes on link family

```
binom_family <- make.link('logit')
# this is equivalent
x <- seq(0, 1, 0.01)
log_link <- function(x){ log(x/(1-x)) }
testthat::expect_equal(log_link(x), binom_family$linkfun(x))

# this is equivalent
x <- seq(-10, 10, 0.5)
inv_log_link <- function(x){ 1 / (1 + exp( -1 * x))}
testthat::expect_equal(inv_log_link(x), binom_family$linkinv(x))

# this is the derivative of the inverse link
x <- seq(0, 1, 0.01)
# p = 0.5 --> p * (1 - p) = 0.25
binom_family$mu.eta( 0 ) 
binom_family$mu.eta( log_link(0.5) )
```
