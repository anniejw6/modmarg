data(margex)
set.seed(100)
margex$assign <- margex$treatment

# Probability of getting treatment increases with age
margex$pr_treat <- plogis(
  scale(margex$age) + scale(margex$distance))

# One-way non-compliance
margex$actual <- margex$assign
margex$actual[margex$assign == 1] <- rbinom(
  n = sum(margex$assign == 1), size = 1,
  prob = margex$pr_treat[margex$assign == 1]
)

# Weights
margex$wgt <- runif(nrow(margex))

# vcov function for summary
vcov.ivreg <- function(model){
  model$sigma^2 * model$cov.unscaled * model$df.residual / model$nobs
}

# ---------------------
context('2SLS model margins')

# Stata

test_that('2sls coefficients and standard errors are correct', {

  mod <- AER::ivreg(
    y ~ as.factor(actual) + age + distance |
      as.factor(assign) + age + distance,
    data = margex)

  mm <- summary(mod, vcov.ivreg)

  # .ivregress 2sls y c.age c.distance (i.actual = i.assign)

  # Instrumental variables (2SLS) regression               Number of obs =    3000
  #                                                        Wald chi2(3)  =  340.53
  #                                                        Prob > chi2   =  0.0000
  #                                                        R-squared     =       .
  #                                                        Root MSE      =  22.642

  # ------------------------------------------------------------------------------
  #            y |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #     1.actual |   29.61091    1.84763    16.03   0.000     25.98962     33.2322
  #          age |  -.7501718   .0451406   -16.62   0.000    -.8386457   -.6616978
  #     distance |  -.0140998   .0023245    -6.07   0.000    -.0186558   -.0095438
  #        _cons |   92.52933    1.61735    57.21   0.000     89.35938    95.69928
  # ------------------------------------------------------------------------------
  # Instrumented:  1.actual
  # Instruments:   age distance 1.assign

  expect_equal(mm$coefficients[, 'Estimate'], c(
    '(Intercept)' = 92.52933, 'as.factor(actual)1' = 29.61091,
    'age' = -0.7501718, 'distance' = -0.0140998))

  expect_equal(mm$coefficients[, 'Std. Error'], c(
    '(Intercept)' = 1.61735, 'as.factor(actual)1' = 1.84763,
    'age' = 0.0451406, 'distance' = 0.0023245),
    tolerance = 0.000001)

})

test_that('2SLS margins are correct', {

  mod <- AER::ivreg(
    y ~ as.factor(actual) + age + distance | as.factor(assign) + age + distance,
    data = margex)

  z <- marg(mod, var_interest = 'actual', data = margex)[[1]]

  # . margins i.actual

  # Predictive margins                                Number of obs   =       3000
  # Model VCE    : Unadjusted

  # Expression   : Linear prediction, predict()

  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #       actual |
  #           0  |   61.84719   .6426773    96.23   0.000     60.58757    63.10682
  #           1  |    91.4581   1.417176    64.54   0.000     88.68049    94.23572
  # ------------------------------------------------------------------------------

  expect_equal(z$Label, paste('actual =', c(0, 1)))
  expect_equal(z$Margin, c(61.84719, 91.4581), tolerance = 0.000001)
  expect_equal(z$Standard.Error, c(.6426773, 1.417176), tolerance = 0.0000001)
  # TODO: other parameters?
  expect_equal(z$Test.Stat, c(96.23, 64.54), tolerance = 0.001)
  expect_equal(z$P.Value, c(0, 0), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(60.58757, 88.68049), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(63.10682, 94.23572), tolerance = 0.0001)

  # . margins, dydx(actual)

  # Average marginal effects                          Number of obs   =       3000
  # Model VCE    : Unadjusted

  # Expression   : Linear prediction, predict()
  # dy/dx w.r.t. : 1.actual

  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #     1.actual |   29.61091    1.84763    16.03   0.000     25.98962     33.2322
  # ------------------------------------------------------------------------------
  # Note: dy/dx for factor levels is the discrete change from the base level.

  z <- marg(mod, data = margex, var_interest = 'actual',
            type = 'effects')[[1]]

  expect_equal(z$Label, paste('actual =', c(0, 1)))
  expect_equal(z$Margin, c(0, 29.61091), tolerance = 0.000001)
  expect_equal(z$Standard.Error, c(0, 1.84763), tolerance = 0.000001)
  expect_equal(z$Test.Stat, c(NaN, 16.03), tolerance = 0.001)
  expect_equal(z$P.Value, c(NaN, 0.000), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(0, 25.98962), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(0, 33.2322), tolerance = 0.0001)

})


test_that('2SLS margins handle weights', {

  mod <- AER::ivreg(
    y ~ as.factor(actual) + age + distance | as.factor(assign) + age + distance,
    weights = wgt, data = margex)

  # .ivregress 2sls y c.age c.distance (i.actual = i.assign)
  # . margins i.actual
  #
  # Predictive margins                                Number of obs   =       3000
  # Model VCE    : Unadjusted
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #       actual |
  #           0  |   61.81683   .6938917    89.09   0.000     60.45683    63.17684
  #           1  |   91.58006    1.73586    52.76   0.000     88.17783    94.98228
  # ------------------------------------------------------------------------------

  z <- marg(mod, var_interest = 'actual',
            vcov_mat = sandwich::vcovHC(mod, type = 'HC0'),
            # dof = mod$df.residual,
            weights = margex$wgt, data = margex)[[1]]

  expect_equal(z$Label, paste('actual =', c(0, 1)))
  expect_equal(z$Margin, c(61.81683, 91.58006), tolerance = 0.000001)
  expect_equal(z$Standard.Error, c(.6938917, 1.73586), tolerance = 0.0000001)
  expect_equal(z$Test.Stat, c(89.09, 52.76), tolerance = 0.01)
  expect_equal(z$P.Value, c(0.000, 0.000), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(60.45683, 88.17783), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(63.17684, 94.98228), tolerance = 0.0001)

  # . margins, dydx(actual)

  # Average marginal effects                          Number of obs   =       3000
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  # dy/dx w.r.t. : 1.actual
  #
  # ------------------------------------------------------------------------------
  #            |            Delta-method
  #            |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #   1.actual |   29.76322    2.17495    13.68   0.000      25.5004    34.02605
  # ------------------------------------------------------------------------------

  z <- marg(mod, var_interest = 'actual',
            vcov_mat = sandwich::vcovHC(mod, type = 'HC0'),
            weights = margex$wgt, data = margex,
            type = 'effects')[[1]]

  expect_equal(z$Label, paste('actual =', c(0, 1)))
  expect_equal(z$Margin, c(0, 29.76322), tolerance = 0.000001)
  expect_equal(z$Standard.Error, c(0, 2.17495), tolerance = 0.000001)
  expect_equal(z$Test.Stat, c(NaN, 13.68), tolerance = 0.001)
  expect_equal(z$P.Value, c(NaN, 0.000), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(0, 25.5004), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(0, 34.02605), tolerance = 0.0001)

})

test_that('2SLS margins warn if weights are absent', {

  mod <- AER::ivreg(
    y ~ as.factor(actual) + age + distance | as.factor(assign) + age + distance,
    weights = wgt, data = margex)

  expect_warning(
    z <- marg(mod, var_interest = 'actual', data = margex),
    paste("The model was built with weights, but you have not",
          "provided weights. Your calculated margins may be odd.",
          "See Details."),
    fixed = TRUE
  )

})

# ---------------------
context('2SLS models handle missingness')

test_that('2SLS margins handle missing covariates', {

  margex_na <- margex
  margex_na$distance[c(1, 3, 5, 7, 9)] <- NA

  mod <- AER::ivreg(
    y ~ as.factor(actual) + age + distance | as.factor(assign) + age + distance,
    data = margex_na)

  # .ivregress 2sls y c.age c.distance (i.actual = i.assign)
  # . margins i.actual

  # Predictive margins                                Number of obs   =       2995
  # Model VCE    : Unadjusted
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #       actual |
  #           0  |   61.84185   .6423856    96.27   0.000      60.5828     63.1009
  #           1  |   91.44042   1.416974    64.53   0.000     88.66321    94.21764
  # ------------------------------------------------------------------------------

  z <- marg(mod, var_interest = 'actual', data = margex_na)[[1]]

  expect_equal(z$Label, paste('actual =', c(0, 1)))
  expect_equal(z$Margin, c(61.84185, 91.44042), tolerance = 0.000001)
  expect_equal(z$Standard.Error, c(.6423856, 1.416974), tolerance = 0.000001)
  expect_equal(z$Test.Stat, c(96.27, 64.54), tolerance = 0.001)
  expect_equal(z$P.Value, c(0.000, 0.000), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(60.5828, 88.66321), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(63.1009, 94.21764), tolerance = 0.0001)

  # . margins, dydx(actual)

  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #     1.actual |   29.59858   1.846635    16.03   0.000     25.97924    33.21791
  # ------------------------------------------------------------------------------

  z <- marg(mod, var_interest = 'actual', data = margex_na,
            type = 'effects')[[1]]

  expect_equal(z$Label, paste('actual =', c(0, 1)))
  expect_equal(z$Margin, c(0, 29.59858), tolerance = 0.000001)
  expect_equal(z$Standard.Error, c(0, 1.846635), tolerance = 0.000001)
  expect_equal(z$Test.Stat, c(NaN, 16.03), tolerance = 0.001)
  expect_equal(z$P.Value, c(NaN, 0.000), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(0, 25.97924), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(0, 33.21791), tolerance = 0.0001)

})

test_that('2SLS margins handle missing weights', {

  margex_na <- margex
  margex_na$wgt[c(1, 2, 5, 8, 9)] <- NA

  mod <- AER::ivreg(
    y ~ as.factor(actual) + age + distance | as.factor(assign) + age + distance,
    weights = wgt, data = margex_na)

  # .ivregress 2sls y c.age c.distance (i.actual = i.assign) [pweight = wgt]
  # . margins i.actual

  # Predictive margins                                Number of obs   =       2995
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #       actual |
  #           0  |   61.82222   .6935352    89.14   0.000     60.46291    63.18152
  #           1  |   91.55232   1.737943    52.68   0.000     88.14601    94.95862
  # ------------------------------------------------------------------------------

  z <- marg(mod, var_interest = 'actual',
            vcov_mat = sandwich::vcovHC(mod, type = 'HC0'),
            data = margex_na, weights = margex_na$wgt)[[1]]

  expect_equal(z$Label, paste('actual =', c(0, 1)))
  expect_equal(z$Margin, c(61.82222, 91.55232), tolerance = 0.000001)
  expect_equal(z$Standard.Error, c(.6935352, 1.737943), tolerance = 0.000001)
  expect_equal(z$Test.Stat, c(89.14, 52.68), tolerance = 0.001)
  expect_equal(z$P.Value, c(0.000, 0.000), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(60.46291, 88.14601), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(63.18152, 94.95862), tolerance = 0.0001)

  # . margins, dydx(actual)

  # Average marginal effects                          Number of obs   =       2995
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  # dy/dx w.r.t. : 1.actual
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #     1.actual |    29.7301   2.176272    13.66   0.000     25.46469    33.99552

  z <- marg(mod, var_interest = 'actual',
            vcov_mat = sandwich::vcovHC(mod, type = 'HC0'),
            data = margex_na, weights = margex_na$wgt,
            type = 'effects')[[1]]

  expect_equal(z$Label, paste('actual =', c(0, 1)))
  expect_equal(z$Margin, c(0, 29.7301), tolerance = 0.000001)
  expect_equal(z$Standard.Error, c(0, 2.176272), tolerance = 0.000001)
  expect_equal(z$Test.Stat, c(NaN, 13.66), tolerance = 0.001)
  expect_equal(z$P.Value, c(NaN, 0.000), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(0, 25.46469), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(0, 33.99552), tolerance = 0.0001)

})

test_that('2SLS margins handle missing weights and covariates', {

  margex_na <- margex
  margex_na$distance[c(1, 3, 5, 7, 9)] <- NA
  margex_na$wgt[c(1, 4, 5, 6, 9, 10)] <- NA

  mod <- AER::ivreg(
    y ~ as.factor(actual) + age + distance | as.factor(assign) + age + distance,
    weights = wgt, data = margex_na)

  # .ivregress 2sls y c.age c.distance (i.actual = i.assign) [pweight = wgt]
  # . margins i.actual

  # Predictive margins                                Number of obs   =       2992
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #       actual |
  #           0  |   61.82219   .6934071    89.16   0.000     60.46314    63.18125
  #           1  |   91.51205   1.736853    52.69   0.000     88.10789    94.91622
  # ------------------------------------------------------------------------------

  z <- marg(mod, var_interest = 'actual',
            vcov_mat = sandwich::vcovHC(mod, type = 'HC0'),
            data = margex_na, weights = margex_na$wgt)[[1]]

  expect_equal(z$Label, paste('actual =', c(0, 1)))
  expect_equal(z$Margin, c(61.82219, 91.51205), tolerance = 0.000001)
  expect_equal(z$Standard.Error, c(.6934071, 1.736853), tolerance = 0.000001)
  expect_equal(z$Test.Stat, c(89.16, 52.69), tolerance = 0.001)
  expect_equal(z$P.Value, c(0.000, 0.000), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(60.46314, 88.10789), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(63.18125, 94.91622), tolerance = 0.0001)

  # . margins, dydx(actual)

  # Average marginal effects                          Number of obs   =       2992
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  # dy/dx w.r.t. : 1.actual
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #     1.actual |   29.68986   2.174845    13.65   0.000     25.42724    33.95248
  # ------------------------------------------------------------------------------

  z <- marg(mod, var_interest = 'actual', type = 'effects',
            vcov_mat = sandwich::vcovHC(mod, type = 'HC0'),
            data = margex_na, weights = margex_na$wgt)[[1]]

  expect_equal(z$Label, paste('actual =', c(0, 1)))
  expect_equal(z$Margin, c(0, 29.68986), tolerance = 0.000001)
  expect_equal(z$Standard.Error, c(0, 2.174845), tolerance = 0.000001)
  expect_equal(z$Test.Stat, c(NaN, 13.65), tolerance = 0.001)
  expect_equal(z$P.Value, c(NaN, 0.000), tolerance = 0.0001)
  expect_equal(z$`Lower CI (95%)`, c(0, 25.42724), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(0, 33.95248), tolerance = 0.0001)

})

test_that('P values are calculated correctly', {
  data(margex)
  set.seed(100)
  margex$noise <- rbinom(nrow(margex), 1, 0.5)

  # . ivregress 2sls y c.age c.distance (i.noise = i.treatment)

  mod <- AER::ivreg(
    y ~ as.factor(noise) + age + distance |
      as.factor(treatment) + age + distance,
    data = margex)

  # . margins i.noise
  #
  # Predictive margins                                Number of obs   =       3000
  # Model VCE    : Unadjusted
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #        noise |
  #           0  |  -609.8165   1255.984    -0.49   0.627    -3071.499    1851.866
  #           1  |   731.4008   1222.935     0.60   0.550    -1665.507    3128.308
  # ------------------------------------------------------------------------------

  z <- marg(mod, var_interest = 'noise', type = 'levels', data = margex)[[1]]

  expect_equal(z$Margin, c(-609.8165, 731.4008), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(1255.984, 1222.935), tolerance = 0.001)
  expect_equal(z$Test.Stat, c(-0.49, 0.60), tolerance = 0.01)
  expect_equal(z$P.Value, c(0.627, 0.550), tolerance = 0.001)
  expect_equal(z$`Lower CI (95%)`, c(-3071.499, -1665.507), tolerance = 0.001)
  expect_equal(z$`Upper CI (95%)`, c(1851.866, 3128.308), tolerance = 0.001)

  # . margins, dydx(noise)
  #
  # Average marginal effects                          Number of obs   =       3000
  # Model VCE    : Unadjusted
  #
  # Expression   : Linear prediction, predict()
  # dy/dx w.r.t. : 1.noise
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #      1.noise |   1341.217   2478.797     0.54   0.588    -3517.136     6199.57
  # ------------------------------------------------------------------------------

  z <- marg(mod, var_interest = 'noise', type = 'effects', data = margex)[[1]]
  z <- z[2, ]

  expect_equal(z$Margin, 1341.217, tolerance = 0.0001)
  expect_equal(z$Standard.Error, 2478.797, tolerance = 0.0001)
  expect_equal(z$Test.Stat, 0.54, tolerance = 0.01)
  expect_equal(z$P.Value, 0.588, tolerance = 0.001)
  expect_equal(z$`Lower CI (95%)`, -3517.136, tolerance = 0.001)
  expect_equal(z$`Upper CI (95%)`, 6199.57, tolerance = 0.0001)
})
