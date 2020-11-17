# modmarg
[![CRAN Version](https://www.r-pkg.org:443/badges/version/modmarg)](https://www.r-pkg.org:443/badges/version/modmarg)
[![Build Status](https://travis-ci.org/anniejw6/modmarg.svg?branch=master)](https://travis-ci.org/anniejw6/modmarg)
[![codecov](https://codecov.io/gh/anniejw6/modmarg/branch/master/graph/badge.svg)](https://codecov.io/gh/anniejw6/modmarg)

Calculate predicted levels and marginal effects 
using the delta method to calculate standard errors. This is an R-based 
version of Stata's 'margins' command.

Features:

* Calculate predictive levels and margins for `glm` and `ivreg` objects
(more models to be added - PRs welcome) using closed-form derivatives

* Add custom variance-covariance matrices to all calculations to add, e.g.,
clustered or robust standard errors (for more information on replicating
Stata analyses, see [here](https://github.com/anniejw6/modmarg/issues/107))

* Frequency weights are incorporated into margins and effects

# Usage

To install this package from CRAN, please run

```
install.packages('modmarg')
```

To install the development version of this package, please run

```
devtools::install_github('anniejw6/modmarg', build_vignettes = TRUE)
```

Here is an example of estimating predicted levels and effects
using the `iris` dataset:

```
data(iris)

mod <- glm(Sepal.Length ~ Sepal.Width + Species, 
           data = iris, family = 'gaussian')
           
# Predicted Levels
modmarg::marg(mod, var_interest = 'Species', type = 'levels')

# Predicted Effects
modmarg::marg(mod, var_interest = 'Species', type = 'effects')
```

There are two vignettes included:

```
vignette('usage', package = 'modmarg')
vignette('delta-method', package = 'modmarg')
```

# More Reading on the Delta Method

* [Delta Method](http://www.phidot.org/software/mark/docs/book/pdf/app_2.pdf): This is from the appendix the book guide to the [MARK program, developed by Gary White](http://www.phidot.org/software/mark/index.html).

* [How can I estimate the standard error of transformed regression parameters in R using the delta method?](https://stats.idre.ucla.edu/r/faq/how-can-i-estimate-the-standard-error-of-transformed-regression-parameters-in-r-using-the-delta-method/) from UCLA.

* [The Delta method to estimate standard errors from a non-linear transformation](http://www.econometricsbysimulation.com/2012/12/the-delta-method-to-estimate-standard.html) from Econometrics by Simulation.

* [A FAQ on Margins from the Stata Blog](https://www.stata.com/support/faqs/statistics/compute-standard-errors-with-margins/)

* [Delta Method on Wikipedia](https://en.wikipedia.org/wiki/Delta_method)

* [What is the intuition behind the sandwich estimator?](https://stats.stackexchange.com/questions/50778/sandwich-estimator-intuition) from StackExchange

* [Least Squares Optimization](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.207.3178) by Harald E. Krogstad

* [The robust sandwich variance estimator for linear regression (theory)](http://thestatsgeek.com/2013/10/12/the-robust-sandwich-variance-estimator-for-linear-regression/) by Jonathan Bartlett

* [Using *Stata’s* Margins Command to Estimate and Interpret Adjusted Predictions and Marginal Effects](https://www3.nd.edu/~rwilliam/stats/Margins01.pdf) by [Richard Williams](http://www3.nd.edu/~rwilliam/)
