library(modmarg)
context("Calculate everything correctly")

test_that("output is calculated correctly", {

  # http://www.stata.com/support/faqs/statistics/compute-standard-errors-with-margins/
  data(margex)
  margex$treatment <- factor(margex$treatment)
  mod <- glm(outcome ~ treatment + distance, data = margex, family = 'binomial')
  z <- mod_marg2(mod, 'treatment', 'levels', at = NULL)[[1]]

  expect_equal(z$Margin, c(.0791146, .2600204), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(.0069456, .0111772), tolerance = 0.0001)
  expect_equal(z$Z.Value, c(11.39, 23.26), tolerance = 0.001)
  expect_equal(z$P.Value, c(0, 0), tolerance = 0.001)
  expect_equal(z$`Lower CI (95%)`, c(.0655016, .2381135), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(.0927277, .2819272), tolerance = 0.0001)

  z <- mod_marg2(mod, 'treatment', 'effects', at = NULL)[[1]]
  z <- z[2, ]
  expect_equal(z$Margin, c(.1809057), tolerance = 0.0001)
  expect_equal(z$Standard.Error, c(.0131684), tolerance = 0.0001)
  expect_equal(z$Z.Value, c(13.74), tolerance = 0.001)
  expect_equal(z$P.Value, c(0), tolerance = 0.001)
  expect_equal(z$`Lower CI (95%)`, c(.1550961), tolerance = 0.0001)
  expect_equal(z$`Upper CI (95%)`, c(.2067153), tolerance = 0.0001)
})
