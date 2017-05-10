library(modmarg)
context("Use different variance-covariance matrices")

test_that("clustered standard errors are correct", {

  data(margex)
  mod <- glm(outcome ~ treatment + distance, data = margex, family = 'gaussian')

  data(ols_cvcov)
  v <- ols_cvcov$clust
  d <- ols_cvcov$stata_dof

  z <- mod_marg2(mod, var_interest = 'treatment',
                 type = 'levels', vcov_mat = v, dof = d)[[1]]

  stata <- aiEstimation::mod_marg(
    model = "reg outcome i.treatment distance, vce(cluster arm)",
    margs = list(levels = "margins i.treatment"),
    df = margex
  )

  expect_equal(z$Margin, c(0.0802249, 0.2588702), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(0.0481047, 0.0467188),
               tolerance = 0.0001)
  expect_equal(z$P.Value, c(0.2373067, 0.0310606), tolerance = 0.0001)

  data(margex)
  mod <- glm(outcome ~ treatment + distance, data = margex, family = 'binomial')
  data(logit_cvcov)
  v <- logit_cvcov$clust
  d <- logit_cvcov$stata_dof
  z <- mod_marg2(mod, var_interest = 'treatment',
                 type = 'levels', vcov_mat = v)[[1]]

  # stata
  # logit outcome i.treatment distance, vce(cluster arm)
  # margins i.treatment"]

  expect_equal(z$Margin, c(0.0791146, 0.2600204), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(0.0473008, 0.0472214),
               tolerance = 0.0001)
  expect_equal(z$P.Value, c(9.4409e-02, 3.6600e-08), tolerance = 0.0001)

})

