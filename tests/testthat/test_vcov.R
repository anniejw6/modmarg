library(modmarg)
context("Use different variance-covariance matrices")

test_that("clustered standard errors are correct", {

  # Gaussian model

  data(margex)
  mod <- glm(outcome ~ treatment + distance, data = margex, family = 'gaussian')

  data(cvcov)
  v <- cvcov$ols$clust
  d <- cvcov$ols$stata_dof

  z <- mod_marg2(mod, var_interest = 'treatment',
                 type = 'levels', vcov_mat = v, dof = d)[[1]]

  # stata
  # reg outcome i.treatment distance, vce(cluster arm)
  # margins i.treatment

  # Predictive margins                                Number of obs   =       3000
  # Model VCE    : Robust
  #
  # Expression   : Linear prediction, predict()
  #
  # ------------------------------------------------------------------------------
  #           |            Delta-method
  #           |     Margin   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  # treatment |
  #        0  |   .0802249   .0481047     1.67   0.237     -.126753    .2872028
  #        1  |   .2588702   .0467188     5.54   0.031     .0578554    .4598851
  # ------------------------------------------------------------------------------

  expect_equal(z$Margin, c(0.0802249, 0.2588702), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(0.0481047, 0.0467188),
               tolerance = 0.0001)
  expect_equal(z$P.Value, c(0.2373067, 0.0310606), tolerance = 0.0001)

  expect_warning(mod_marg2(mod, var_interest = 'treatment',
                           type = 'levels', vcov_mat = v))

  # Binary model

  data(margex)
  mod <- glm(outcome ~ treatment + distance, data = margex, family = 'binomial')
  data(cvcov)
  v <- cvcov$logit$clust
  d <- cvcov$logit$stata_dof
  z <- mod_marg2(mod, var_interest = 'treatment',
                 type = 'levels', vcov_mat = v)[[1]]

  # stata
  # logit outcome i.treatment distance, vce(cluster arm)
  # margins i.treatment
  #
  # Predictive margins                                Number of obs   =       3000
  # Model VCE    : Robust
  #
  # Expression   : Pr(outcome), predict()
  #
  # ------------------------------------------------------------------------------
  #             |            Delta-method
  #             |     Margin   Std. Err.      z    P>|z|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #   treatment |
  #          0  |   .0791146   .0473008     1.67   0.094    -.0135933    .1718226
  #          1  |   .2600204   .0472214     5.51   0.000      .167468    .3525727
  # ------------------------------------------------------------------------------

  expect_equal(z$Margin, c(0.0791146, 0.2600204), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(0.0473008, 0.0472214),
               tolerance = 0.0001)
  expect_equal(z$P.Value, c(9.4409e-02, 3.6600e-08), tolerance = 0.0001)

})

