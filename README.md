# modmarg
[![Build Status](https://travis-ci.org/anniejw6/modmarg.svg?branch=master)](https://travis-ci.org/anniejw6/modmarg)

predicted margins and marginals effects


# v1.0

* Can take the predictive levels of a discrete variable [done]
* Can take the predictive levels of a continuous variable [not done?]
* Can take the predictive effects (dydx) of a discrete variable [done]
* Supports interactions [need to test]
* Models: OLS, Logit
* Test suite [done - could probably do more]
  * Add https://github.com/jimhester/covr?
  * Add Travis CI

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

# this is the derivative of the link (not the inverse link!)
x <- seq(0, 1, 0.01)
# p = 0.5 --> p * (1 - p) = 0.25
binom_family$mu.eta( 0 ) 
binom_family$mu.eta( log_link(0.5) )
```

# Resources

* [Taylor Approximation and the Delta Method](http://web.stanford.edu/class/cme308/OldWebsite/notes/TaylorAppDeltaMethod.pdf)

* [UCLA - Delta Method in R](http://www.ats.ucla.edu/stat/r/faq/deltamethod.htm)

* [EconBS](http://www.econometricsbysimulation.com/2012/12/the-delta-method-to-estimate-standard.html)

* [Stata Blog](http://www.stata.com/support/faqs/statistics/compute-standard-errors-with-margins/)

* https://en.wikipedia.org/wiki/Delta_method

* http://stats.stackexchange.com/questions/50778/sandwich-estimator-intuition

* https://www.math.ntnu.no/~hek/Optimering2010/LeastSquaresOptimization2010.pdf

* [Sandwich Estimators](http://thestatsgeek.com/2013/10/12/the-robust-sandwich-variance-estimator-for-linear-regression/)

* [Delta Method](http://www.phidot.org/software/mark/docs/book/pdf/app_2.pdf)

* [Stata and Margins](https://www3.nd.edu/~rwilliam/stats/Margins01.pdf)
