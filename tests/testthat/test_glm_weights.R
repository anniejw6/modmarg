library(modmarg)
context("Calculate things with weights works")

test_that("Complete dataset: Analytic Weights", {

  data(margex)
  mm <- glm(y ~ as.factor(treatment) + age, data = margex, family = 'gaussian',
            weights = distance)

  # . reg y i.treatment c.age [aweight = distance]
  # (sum of wgt is   1.7576e+05)
  #
  #       Source |       SS       df       MS              Number of obs =    3000
  # -------------+------------------------------           F(  2,  2997) =  302.37
  #        Model |  234301.395     2  117150.697           Prob > F      =  0.0000
  #     Residual |  1161174.13  2997   387.44549           R-squared     =  0.1679
  # -------------+------------------------------           Adj R-squared =  0.1673
  #        Total |  1395475.53  2999  465.313614           Root MSE      =  19.684
  #
  # ------------------------------------------------------------------------------
  #            y |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   15.21605   .7505776    20.27   0.000     13.74435    16.68774
  #          age |  -.6131126   .0321209   -19.09   0.000    -.6760937   -.5501314
  #        _cons |   83.13915   1.276775    65.12   0.000      80.6357    85.64259
  # ------------------------------------------------------------------------------

  # . margins i.treatment
  #
  # Predictive margins                                Number of obs   =       3000
  # Model VCE    : OLS
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #    treatment |
  #           0  |   58.75757   .5044906   116.47   0.000     57.76839    59.74676
  #           1  |   73.97362    .535136   138.23   0.000     72.92435    75.02289
  # ------------------------------------------------------------------------------

  z1 <- marg(mod = mm, var_interest = 'treatment', type = 'levels')[[1]]
  expect_equal(z1$Margin, c(58.75757, 73.97362), tolerance = 0.00001)
  expect_equal(z1$Standard.Error, c(.5044906, .535136), tolerance = 0.00001)

  # . margins, dydx(treatment)
  #
  # Average marginal effects                          Number of obs   =       3000
  # Model VCE    : OLS
  #
  # Expression   : Linear prediction, predict()
  # dy/dx w.r.t. : 1.treatment
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   15.21605   .7505776    20.27   0.000     13.74435    16.68774
  # ------------------------------------------------------------------------------
  # Note: dy/dx for factor levels is the discrete change from the base level.

  z2 <- marg(mod = mm, var_interest = 'treatment', type = 'effects')[[1]]
  expect_equal(z2$Margin[2], 15.21605, tolerance = 0.00001)
  expect_equal(z2$Standard.Error[2], .7505776, tolerance = 0.00001)

  # Now with a binomial!
  data(margex)
  mm <- glm(outcome ~ as.factor(treatment) + age, data = margex,
            family = 'binomial', weights = distance)
  # summary(mm)

  # . glm outcome i.treatment c.age [aweight = distance], link(logit)
  # (sum of wgt is   1.7576e+05)
  #
  # Iteration 0:   log likelihood = -2153.9105  (not concave)
  # Iteration 1:   log likelihood = -864.74513  (not concave)
  # Iteration 2:   log likelihood =  148.40447  (backed up)
  # Iteration 3:   log likelihood =  797.79906
  # Iteration 4:   log likelihood =  938.77072
  # Iteration 5:   log likelihood =  941.78157
  # Iteration 6:   log likelihood =  941.81143
  # Iteration 7:   log likelihood =  941.81148
  #
  # Generalized linear models                          No. of obs      =      3000
  # Optimization     : ML                              Residual df     =      2997
  #                                                    Scale parameter =  .0312808
  # Deviance         =  93.74854825                    (1/df) Deviance =  .0312808
  # Pearson          =  93.74854825                    (1/df) Pearson  =  .0312808
  #
  # Variance function: V(u) = 1                        [Gaussian]
  # Link function    : g(u) = ln(u/(1-u))              [Logit]
  #
  #                                                    AIC             = -.6258743
  # Log likelihood   =  941.8114828                    BIC             = -23901.34
  #
  # ------------------------------------------------------------------------------
  #              |                 OIM
  #      outcome |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   1.160055   .2804038     4.14   0.000     .6104737    1.709636
  #          age |   .0982091   .0123429     7.96   0.000     .0740174    .1224008
  #        _cons |  -8.579118   .6827328   -12.57   0.000     -9.91725   -7.240986
  # ------------------------------------------------------------------------------

  # . margins i.treatment
  #
  # Predictive margins                                Number of obs   =       3000
  # Model VCE    : OIM
  #
  # Expression   : Predicted mean outcome, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #    treatment |
  #           0  |   .0162469   .0042079     3.86   0.000     .0079995    .0244943
  #           1  |   .0484142   .0040881    11.84   0.000     .0404018    .0564267
  # ------------------------------------------------------------------------------

  # Please note that a binomial glm with weights DOES NOT REPLICATE across
  # Stata and R

})

test_that("Complete dataset: Probability Weights", {

  data(margex)
  mm <- glm(y ~ as.factor(treatment) + age, data = margex, family = 'gaussian',
            weights = distance)
  # lmtest::coeftest(mm, sandwich::vcovHC(mm, 'HC1'))

  # . reg y i.treatment c.age [pweight = distance]
  # (sum of wgt is   1.7576e+05)
  #
  # Linear regression                                      Number of obs =    3000
  #                                                        F(  2,  2997) =   29.91
  #                                                        Prob > F      =  0.0000
  #                                                        R-squared     =  0.1679
  #                                                        Root MSE      =  19.684
  #
  # ------------------------------------------------------------------------------
  #              |               Robust
  #            y |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   15.21605    2.43451     6.25   0.000     10.44257    19.98953
  #          age |  -.6131126    .099945    -6.13   0.000    -.8090804   -.4171448
  #        _cons |   83.13915   3.998522    20.79   0.000     75.29902    90.97927
  # ------------------------------------------------------------------------------

  # stata
  # . margins i.treatment
  #
  # Predictive margins                                Number of obs   =       3000
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #             |            Delta-method
  #             |     Margin   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #   treatment |
  #          0  |   58.75757   1.618851    36.30   0.000      55.5834    61.93174
  #          1  |   73.97362   1.752939    42.20   0.000     70.53653     77.4107
  # ------------------------------------------------------------------------------

  z1 <- marg(mod = mm, var_interest = 'treatment', type = 'levels',
             vcov_mat = sandwich::vcovHC(mm, 'HC1'))[[1]]
  expect_equal(z1$Margin, c(58.75757, 73.97362), tolerance = 0.00001)
  expect_equal(z1$Standard.Error, c(1.618851, 1.752939), tolerance = 0.00001)

  # Average marginal effects                          Number of obs   =       3000
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  # dy/dx w.r.t. : 1.treatment
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   15.21605    2.43451     6.25   0.000     10.44257    19.98953
  # ------------------------------------------------------------------------------

  z2 <- marg(mod = mm, var_interest = 'treatment', type = 'effects',
             vcov_mat = sandwich::vcovHC(mm, 'HC1'))[[1]]
  expect_equal(z2$Margin[2], 15.21605, tolerance = 0.00001)
  expect_equal(z2$Standard.Error[2], 2.43451, tolerance = 0.00001)

  # Binomial ----
  # See also https://stackoverflow.com/questions/27367974/different-robust-standard-errors-of-logit-regression-in-stata-and-r
  sandwich1 <- function(object, ...){
    sandwich::sandwich(object) * nobs(object) / (nobs(object) - 1)
  }
  data(margex)
  mm1 <- glm(outcome ~ as.factor(treatment) + age, data = margex,
            family = 'quasibinomial', weights = distance)
  mm2 <- glm(outcome ~ as.factor(treatment) + age, data = margex,
            family = 'binomial', weights = distance)

  # lmtest::coeftest(mm, sandwich1)

  # . logit outcome i.treatment c.age [pweight = distance]
  #
  # Iteration 0:   log pseudolikelihood = -26427.316
  # Iteration 1:   log pseudolikelihood = -23090.032
  # Iteration 2:   log pseudolikelihood = -21521.172
  # Iteration 3:   log pseudolikelihood = -21485.095
  # Iteration 4:   log pseudolikelihood = -21484.924
  # Iteration 5:   log pseudolikelihood = -21484.924
  #
  # Logistic regression                               Number of obs   =       3000
  #                                                   Wald chi2(2)    =     130.53
  #                                                   Prob > chi2     =     0.0000
  # Log pseudolikelihood = -21484.924                 Pseudo R2       =     0.1870
  #
  # ------------------------------------------------------------------------------
  #              |               Robust
  #      outcome |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   1.309707   .2217218     5.91   0.000     .8751401    1.744274
  #          age |   .1131297    .012399     9.12   0.000     .0888282    .1374313
  #        _cons |   -9.51608   .5902697   -16.12   0.000    -10.67299   -8.359173
  # ------------------------------------------------------------------------------

  # . margins i.treatment
  #
  # Predictive margins                                Number of obs   =       3000
  # Model VCE    : Robust
  #
  # Expression   : Pr(outcome), predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #    treatment |
  #           0  |   .0136666   .0024822     5.51   0.000     .0088016    .0185316
  #           1  |   .0467673   .0053306     8.77   0.000     .0363195     .057215
  # ------------------------------------------------------------------------------
  z1 <- marg(mod = mm1, var_interest = 'treatment', type = 'levels',
             vcov_mat = sandwich1(mm1))[[1]]
  z2 <- marg(mod = mm2, var_interest = 'treatment', type = 'levels',
             vcov_mat = sandwich1(mm2))[[1]]
  expect_equal(z1$Margin, z2$Margin, tolerance = 0.00001)
  expect_equal(z1$Margin, c(.0136666, .0467673), tolerance = 0.00001)
  expect_equal(z1$Standard.Error, z2$Standard.Error)
  expect_equal(z1$Standard.Error, c(.0024822, .0053306), tolerance = 0.00001)

  # . margins, dydx(treatment)
  #
  # Average marginal effects                          Number of obs   =       3000
  # Model VCE    : Robust
  #
  # Expression   : Pr(outcome), predict()
  # dy/dx w.r.t. : 1.treatment
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   .0331007      .0058     5.71   0.000     .0217328    .0444685
  # ------------------------------------------------------------------------------
  # Note: dy/dx for factor levels is the discrete change from the base level.

  z1 <- marg(mod = mm1, var_interest = 'treatment', type = 'effects',
             vcov_mat = sandwich1(mm1))[[1]]
  z2 <- marg(mod = mm2, var_interest = 'treatment', type = 'effects',
             vcov_mat = sandwich1(mm2))[[1]]
  expect_equal(z1$Margin, z2$Margin, tolerance = 0.00001)
  expect_equal(z1$Margin[2], c(.0331007), tolerance = 0.00001)
  expect_equal(z1$Standard.Error, z2$Standard.Error)
  expect_equal(z1$Standard.Error[2], c(.0058), tolerance = 0.00001)

})

test_that("Partial Dataset: Missing covariate data, full weight data", {

  data(margex)
  margex$age[c(18, 28, 190, 2830)] <- NA
  margex$treatment[c(5, 29, 2)] <- NA

  mm <- glm(y ~ as.factor(treatment) + age, data = margex, family = 'gaussian',
            weights = distance)

  # . reg y i.treatment c.age [aweight = distance]
  # (sum of wgt is   1.7496e+05)
  #
  #       Source |       SS       df       MS              Number of obs =    2993
  # -------------+------------------------------           F(  2,  2990) =  300.29
  #        Model |  233633.182     2  116816.591           Prob > F      =  0.0000
  #     Residual |  1163156.82  2990   389.01566           R-squared     =  0.1673
  # -------------+------------------------------           Adj R-squared =  0.1667
  #        Total |  1396790.01  2992   466.84158           Root MSE      =  19.723
  #
  # ------------------------------------------------------------------------------
  #            y |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |    15.1963   .7533367    20.17   0.000     13.71918    16.67341
  #          age |   -.613312   .0321677   -19.07   0.000    -.6763851   -.5502389
  #        _cons |   83.14645   1.278535    65.03   0.000     80.63955    85.65335
  # ------------------------------------------------------------------------------

  # . margins i.treatment
  #
  # Predictive margins                                Number of obs   =       2993
  # Model VCE    : OLS
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #    treatment |
  #           0  |   58.75824   .5049496   116.36   0.000     57.76815    59.74832
  #           1  |   73.95453   .5383336   137.38   0.000     72.89899    75.01007
  # ------------------------------------------------------------------------------

  z1 <- marg(mod = mm, var_interest = 'treatment', type = 'levels',
             data = mm$data, weights = mm$data$distance)[[1]]
  expect_equal(z1$Margin, c(58.75824, 73.95453), tolerance = 0.00001)
  expect_equal(z1$Standard.Error, c(.5049496, .5383336), tolerance = 0.00001)

  # . margins, dydx(treatment)
  #
  # Average marginal effects                          Number of obs   =       2993
  # Model VCE    : OLS
  #
  # Expression   : Linear prediction, predict()
  # dy/dx w.r.t. : 1.treatment
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |    15.1963   .7533367    20.17   0.000     13.71918    16.67341

  z1 <- marg(mod = mm, var_interest = 'treatment', type = 'effects',
             data = mm$data, weights = mm$data$distance)[[1]]
  expect_equal(z1$Margin[2], c(15.1963), tolerance = 0.00001)
  expect_equal(z1$Standard.Error[2], c(.7533367), tolerance = 0.00001)

})

test_that("Partial Dataset: Full covariate data, missing weight data", {

  data(margex)
  margex$distance[c(5, 29, 2, 14, 920)] <- NA

  mm <- glm(y ~ as.factor(treatment) + age, data = margex, family = 'gaussian',
            weights = distance)

  # . reg y i.treatment c.age [aweight = distance]
  # (sum of wgt is   1.7569e+05)
  #
  #       Source |       SS       df       MS              Number of obs =    2995
  # -------------+------------------------------           F(  2,  2992) =  301.96
  #        Model |  234038.663     2  117019.331           Prob > F      =  0.0000
  #     Residual |  1159496.29  2992  387.532181           R-squared     =  0.1679
  # -------------+------------------------------           Adj R-squared =  0.1674
  #        Total |  1393534.95  2994  465.442535           Root MSE      =  19.686
  #
  # ------------------------------------------------------------------------------
  #            y |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   15.20872   .7512209    20.25   0.000     13.73576    16.68168
  #          age |  -.6136003   .0321528   -19.08   0.000    -.6766441   -.5505564
  #        _cons |   83.15702   1.277997    65.07   0.000     80.65118    85.66286
  # ------------------------------------------------------------------------------

  # . margins i.treatment
  #
  # Predictive margins                                Number of obs   =       2995
  # Model VCE    : OLS
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #    treatment |
  #           0  |   58.75968   .5048333   116.39   0.000     57.76982    59.74953
  #           1  |    73.9684    .535735   138.07   0.000     72.91795    75.01884
  # ------------------------------------------------------------------------------

  z1 <- marg(mod = mm, var_interest = 'treatment', type = 'levels',
             data = mm$data, weights = mm$data$distance)[[1]]
  expect_equal(z1$Margin, c(58.75968, 73.9684), tolerance = 0.00001)
  expect_equal(z1$Standard.Error, c(.5048333, .535735), tolerance = 0.00001)

  # . margins, dydx(treatment)
  #
  # Average marginal effects                          Number of obs   =       2995
  # Model VCE    : OLS
  #
  # Expression   : Linear prediction, predict()
  # dy/dx w.r.t. : 1.treatment
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   15.20872   .7512209    20.25   0.000     13.73576    16.68168
  # ------------------------------------------------------------------------------
  # Note: dy/dx for factor levels is the discrete change from the base level.

  z1 <- marg(mod = mm, var_interest = 'treatment', type = 'effects',
             data = mm$data, weights = mm$data$distance)[[1]]
  expect_equal(z1$Margin[2], c(15.20872), tolerance = 0.00001)
  expect_equal(z1$Standard.Error[2], c(.7512209), tolerance = 0.00001)

})

test_that("Partial Dataset: Missing covariate data, missing weight data", {

  data(margex)
  margex$age[c(18, 28, 190, 2830)] <- NA
  margex$treatment[c(5, 29, 2)] <- NA
  margex$y[c(1:20)] <- NA
  margex$distance[c(10, 38, 28, 18, 123, 209, 840)] <- NA

  mm <- glm(y ~ as.factor(treatment) + age, data = margex, family = 'gaussian',
            weights = distance)

  # . reg y i.treatment c.age [aweight = distance]
  # (sum of wgt is   1.7472e+05)
  #
  #       Source |       SS       df       MS              Number of obs =    2972
  # -------------+------------------------------           F(  2,  2969) =  298.17
  #        Model |  232038.857     2  116019.429           Prob > F      =  0.0000
  #     Residual |  1155254.79  2969   389.10569           R-squared     =  0.1673
  # -------------+------------------------------           Adj R-squared =  0.1667
  #        Total |  1387293.65  2971  466.945019           Root MSE      =  19.726
  #
  # ------------------------------------------------------------------------------
  #            y |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   15.16541   .7559662    20.06   0.000     13.68314    16.64768
  #          age |   -.614479   .0322896   -19.03   0.000    -.6777912   -.5511667
  #        _cons |   83.19123   1.283191    64.83   0.000     80.67519    85.70726
  # ------------------------------------------------------------------------------

  # . margins i.treatment
  #
  # Predictive margins                                Number of obs   =       2972
  # Model VCE    : OLS
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #    treatment |
  #           0  |    58.7659     .50639   116.05   0.000     57.77299    59.75881
  #           1  |    73.9313    .540633   136.75   0.000     72.87125    74.99136
  # ------------------------------------------------------------------------------

  z1 <- marg(mod = mm, var_interest = 'treatment', type = 'levels',
             data = mm$data, weights = mm$data$distance)[[1]]
  expect_equal(z1$Margin, c(58.7659, 73.9313), tolerance = 0.00001)
  expect_equal(z1$Standard.Error, c(.50639, .540633), tolerance = 0.00001)

  # . margins, dydx(treatment)
  #
  # Average marginal effects                          Number of obs   =       2972
  # Model VCE    : OLS
  #
  # Expression   : Linear prediction, predict()
  # dy/dx w.r.t. : 1.treatment
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   15.16541   .7559662    20.06   0.000     13.68314    16.64768
  # ------------------------------------------------------------------------------
  # Note: dy/dx for factor levels is the discrete change from the base level.

  z1 <- marg(mod = mm, var_interest = 'treatment', type = 'effects',
             data = mm$data, weights = mm$data$distance)[[1]]
  expect_equal(z1$Margin[2], c(15.16541), tolerance = 0.00001)
  expect_equal(z1$Standard.Error[2], c(.7559662), tolerance = 0.00001)

})

test_that("Missing covariate, outcome, and weight data", {

  data(margex)

  margex$distance[c(1,3,5)] <- NA
  margex$age[c(10, 24, 390)] <- NA
  margex$y[c(12, 220)] <- NA

  mm <- glm(y ~ as.factor(treatment) + age, data = margex, family = 'gaussian',
            weights = distance)

  # . reg y i.treatment c.age [aw = distance]
  # (sum of wgt is   1.7498e+05)
  #
  #       Source |       SS       df       MS              Number of obs =    2992
  # -------------+------------------------------           F(  2,  2989) =  302.48
  #        Model |  235155.511     2  117577.756           Prob > F      =  0.0000
  #     Residual |  1161876.02  2989  388.717305           R-squared     =  0.1683
  # -------------+------------------------------           Adj R-squared =  0.1678
  #        Total |  1397031.54  2991  467.078414           Root MSE      =  19.716

  # ------------------------------------------------------------------------------
  #            y |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   15.28986   .7537398    20.29   0.000     13.81196    16.76776
  #          age |  -.6161595   .0322077   -19.13   0.000    -.6793111   -.5530079
  #        _cons |   83.25147   1.279858    65.05   0.000     80.74198    85.76097
  # ------------------------------------------------------------------------------
  #
  # . margins i.treatment
  #
  # Predictive margins                                Number of obs   =       2992
  # Model VCE    : OLS
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #    treatment |
  #           0  |   58.73447    .505075   116.29   0.000     57.74414     59.7248
  #           1  |   74.02433   .5383886   137.49   0.000     72.96868    75.07998
  # ------------------------------------------------------------------------------

  z1 <- modmarg::marg(mod = mm, var_interest = 'treatment', type = 'levels')[[1]]

  expect_equal(z1$Label, as.factor(c('treatment = 0', 'treatment = 1')))
  expect_equal(z1$Margin, c(58.73447, 74.02433), tolerance = 0.0000001)
  expect_equal(z1$Standard.Error, c(0.505075, 0.5383886), tolerance = 0.0000001)
  expect_equal(z1$Test.Stat, c(116.29, 137.49), tolerance = 0.0001)
  expect_equal(z1$P.Value, c(0.000, 0.000), tolerance = 0.0001)
  expect_equal(z1$`Lower CI (95%)`, c(57.74414, 72.96868), tolerance = 0.0000001)
  expect_equal(z1$`Upper CI (95%)`, c(59.7248, 75.07998), tolerance = 0.0000001)

})

test_that("Errors and warnings work", {

  data(margex)
  set.seed(12345)
  margex$y[sample(nrow(margex), 100)] <- NA
  mm <- glm(y ~ as.factor(treatment) + age + factor(outcome), data = margex,
            family = 'gaussian', weights = distance)

  expect_error(marg(mod = mm, var_interest = 'treatment', type = 'levels',
                    data = subset(margex, outcome == 1)),
               '`weights` and `data` must be the same length.')

  expect_warning(marg(mod = mm, var_interest = 'treatment', type = 'levels',
                    data = subset(margex, outcome == 1),
                    weights = NULL),
               paste('The model was built with weights, but you have not',
                     'provided weights. Your calculated margins may be odd.',
                     'See Details.'))

  expect_warning(marg(mod = mm, var_interest = 'treatment', type = 'levels',
                      data = subset(margex, outcome == 1),
                      weights = rep(1, sum(margex$outcome == 1))),
                 paste('The model was built with weights, but you have not',
                       'provided weights. Your calculated margins may be odd.',
                       'See Details.'))

})
