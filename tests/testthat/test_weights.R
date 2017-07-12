library(modmarg)
context("Calculate things with weights works")

test_that("Using weights with full dataset works", {

  data(margex)
  mm <- glm(y ~ as.factor(treatment) + age, data = margex, family = 'gaussian',
            weights = distance)

  # Actual check
  z1 <- marg(mod = mm, var_interest = 'treatment', type = 'levels',
             vcov_mat = sandwich::vcovHC(mm, 'HC1'))[[1]]
  z2 <- marg(mod = mm, var_interest = 'treatment', type = 'effects',
             vcov_mat = sandwich::vcovHC(mm, 'HC1'))[[1]]

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

  # Make sure levels calculated correctly
  expect_equal(z1$Margin, c(58.75757, 73.97362), tolerance = 0.0001)
  expect_equal(z1$Standard.Error, c(1.618851, 1.752939), tolerance = 0.0001)

  #
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

  expect_equal(z2$Margin[2], 15.21605, tolerance = 0.0001)
  expect_equal(z2$Standard.Error[2], 2.43451, tolerance = 0.0001)

})


test_that("Using weights on a subset of data works", {

  data(margex)
  set.seed(12345)
  margex$y[sample(nrow(margex), 10)] <- NA
  mm <- glm(y ~ as.factor(treatment) + age, data = margex,
            family = 'gaussian', weights = distance)

  # Actual check
  z1 <- marg(mod = mm, var_interest = 'treatment', type = 'levels',
             weights = margex$distance,
             vcov_mat = sandwich::vcovHC(mm, 'HC1'))[[1]]
  z2 <- marg(mod = mm, var_interest = 'treatment', type = 'effects',
             weights = margex$distance,
             vcov_mat = sandwich::vcovHC(mm, 'HC1'))[[1]]

  # . reg y i.treatment age [pweight = distance]
  # (sum of wgt is   1.7571e+05)
  #
  # Linear regression                                      Number of obs =    2990
  #                                                        F(  2,  2987) =   29.91
  #                                                        Prob > F      =  0.0000
  #                                                        R-squared     =  0.1679
  #                                                        Root MSE      =  19.685
  #
  # ------------------------------------------------------------------------------
  #              |               Robust
  #            y |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   15.21381   2.434979     6.25   0.000      10.4394    19.98821
  #          age |  -.6132242   .0999579    -6.13   0.000    -.8092174    -.417231
  #        _cons |   83.14599   3.999319    20.79   0.000     75.30429    90.98768
  # ------------------------------------------------------------------------------
  #
  # . estout . using mod1.txt, cells("b se t p") stats(N) replace
  # (note: file mod1.txt not found)
  # (output written to mod1.txt)
  #
  # . estimates store t1
  #
  # . margins i.treatment
  #
  # Predictive margins                                Number of obs   =       2990
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #    treatment |
  #           0  |   58.76027   1.619279    36.29   0.000     55.58525    61.93529
  #           1  |   73.97407   1.753296    42.19   0.000     70.53628    77.41187
  # ------------------------------------------------------------------------------
  #
  # . quietly estadd margins i.treatment, replace
  #
  # . estout e(margins_table) using m1.txt, replace
  # (note: file m1.txt not found)
  # (output written to m1.txt)
  #
  # . estimates restore t1
  # (results t1 are active now)
  #
  # . margins, dydx(treatment)
  #
  # Average marginal effects                          Number of obs   =       2990
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  # dy/dx w.r.t. : 1.treatment
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   15.21381   2.434979     6.25   0.000      10.4394    19.98821
  # ------------------------------------------------------------------------------
  # Note: dy/dx for factor levels is the discrete change from the base level.

  expect_equal(z1$Margin, c(58.76027, 73.97407), tolerance = 0.0001)
  expect_equal(z1$Standard.Error, c(1.619279, 1.753296), tolerance = 0.0001)
  expect_equal(z2$Margin[2], 15.21381, tolerance = 0.0001)
  expect_equal(z2$Standard.Error[2], 2.434979, tolerance = 0.0001)

  data(margex)
  set.seed(12345)
  margex$y[sample(nrow(margex), 100)] <- NA
  mm <- glm(y ~ as.factor(treatment) + age + factor(outcome), data = margex,
            family = 'gaussian', weights = distance)

  # . reg y i.treatment age i.outcome [pweight = distance]
  # (sum of wgt is   1.7084e+05)
  #
  # Linear regression                                      Number of obs =    2900
  #                                                        F(  3,  2896) =   19.61
  #                                                        Prob > F      =  0.0000
  #                                                        R-squared     =  0.1699
  #                                                        Root MSE      =  19.827
  #
  # ------------------------------------------------------------------------------
  #              |               Robust
  #            y |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #  1.treatment |   15.28873   2.525845     6.05   0.000      10.3361    20.24137
  #          age |  -.6378861   .1061343    -6.01   0.000    -.8459925   -.4297797
  #    1.outcome |   2.314752   2.255863     1.03   0.305    -2.108507    6.738011
  #        _cons |   84.05644   4.205926    19.99   0.000     75.80953    92.30335
  # ------------------------------------------------------------------------------
  #
  # . estout . using mod1.txt, cells("b se t p") stats(N) replace
  # (note: file mod1.txt not found)
  # (output written to mod1.txt)
  #
  # . estimates store t1
  #
  # . margins i.treatment, by(outcome)
  #
  # Predictive margins                                Number of obs   =       2900
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  # over         : outcome
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |     Margin   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #      outcome#|
  #    treatment |
  #         0 0  |    58.9547   1.634249    36.07   0.000     55.75029     62.1591
  #         0 1  |   74.24343   1.905337    38.97   0.000     70.50747    77.97938
  #         1 0  |    53.0616   2.417434    21.95   0.000     48.32154    57.80167
  #         1 1  |   68.35033   1.121245    60.96   0.000     66.15181    70.54885
  # ------------------------------------------------------------------------------
  #
  # . quietly estadd margins i.treatment, by(outcome) replace
  #
  # . estout e(margins_table) using m1.txt, replace
  # (note: file m1.txt not found)
  # (output written to m1.txt)
  #
  # . estimates restore t1
  # (results t1 are active now)
  #
  # . margins, dydx(treatment) by(outcome)
  #
  # Average marginal effects                          Number of obs   =       2900
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  # dy/dx w.r.t. : 1.treatment
  # over         : outcome
  #
  # ------------------------------------------------------------------------------
  #              |            Delta-method
  #              |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  # 1.treatment  |
  #      outcome |
  #           0  |   15.28873   2.525845     6.05   0.000      10.3361    20.24137
  #           1  |   15.28873   2.525845     6.05   0.000      10.3361    20.24137
  # ------------------------------------------------------------------------------
  # Note: dy/dx for factor levels is the discrete change from the base level.

  z1 <- marg(mod = mm, var_interest = 'treatment', type = 'levels',
             data = subset(margex, outcome == 1),
             weights = margex$distance[margex$outcome == 1],
             vcov_mat = sandwich::vcovHC(mm, 'HC1'))[[1]]
  z2 <- marg(mod = mm, var_interest = 'treatment', type = 'levels',
             data = subset(margex, outcome == 0),
             weights = margex$distance[margex$outcome == 0],
             vcov_mat = sandwich::vcovHC(mm, 'HC1'))[[1]]

  expect_equal(z1$Margin, c(53.0616, 68.35033), tolerance = 0.0001)
  expect_equal(z1$Standard.Error, c(2.417434, 1.121245), tolerance = 0.0001)
  expect_equal(z2$Margin, c(58.9547, 74.24343), tolerance = 0.0001)
  expect_equal(z2$Standard.Error, c(1.634249, 1.905337), tolerance = 0.0001)



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
