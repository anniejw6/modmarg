# Format margins output
#
# @param se vector, standard errors
# @param family character, type of model (e.g. gaussian, binomial)
# @param dof integer, residual degrees of freedom
# @param cofint vector, confidence intervals defaults to c(0.025, 0.975)
# @param margin_labels vector, label of what the margins mean
# @param pred_margins vector, predictive margins
#
#' @importFrom stats pt qnorm pnorm qt
# @return dataframe of label, margin, se, z-value, p-value, and confidence interval bounds
#
# @examples
# format_output(margin_labels = c('hello', 'goodbye', 'whatever'),
# pred_margins = c(1, 0.25, 3.1),
# se = c(0.25, 0.75, 0.5), family = 'binomial')
format_output <- function(margin_labels, pred_margins, se, family, dof,
                          cofint = c(0.025, 0.975)){

  stopifnot(is.numeric(pred_margins), is.numeric(se),
            is.numeric(cofint),
            cofint[1] > 0, cofint[2] < 1,
            cofint[2] > cofint[1], length(cofint) == 2,
            length(margin_labels) == length(pred_margins),
            length(se) == length(pred_margins)
            )

  test_stat <- pred_margins / se
  if(family == "gaussian"){
    pval <- 2 * pt(-1 * abs(test_stat), df = dof)
    ci <- c(qt(min(cofint), df = dof), qt(max(cofint), df = dof))
  } else {
    pval <- 2 * pnorm(-1 * abs(test_stat))
    ci <- c(qnorm(min(cofint)), qnorm(max(cofint)))
  }

  res <- data.frame(
    Label = margin_labels,
    Margin = pred_margins,
    `Standard Error` = se,
    `Test Stat` = test_stat,
    `P Value` = pval,
    lower_ci = pred_margins + min(ci) * se,
    upper_ci = pred_margins + max(ci) * se
  )

  names(res)[names(res) %in% c('lower_ci', 'upper_ci')] <-
    sprintf("%s (%s)", c('Lower CI', 'Upper CI'),
            paste0(round(100 * (cofint[2] - cofint[1])), '%'))

  row.names(res) <- NULL
  res
}
