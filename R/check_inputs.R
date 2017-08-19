# Wrapper function to check inputs
check_inputs <- function(weights, data, var_interest,
                         at, cofint, base_rn, type, dof, vcov_mat){

  # Warn if base_rn set but type != 'effects'
  if(base_rn != 1 & type != 'effects')
    warning("Setting base_rn when type == 'levels' is ignored.")

  # Weights should be same length as data
  if(!is.null(weights) & length(weights) != nrow(data))
    stop('`weights` and `data` must be the same length.')

  # DOF Check
  if(is.null(dof) & !is.null(vcov_mat))
    warning(
      "You provided a new variance-covariance matrix ",
      "but no degrees of freedom for the T test. P-value calculations ",
      "may be incorrect if the model is gaussian - ",
      "see ?modmarg::marg for details.")

  stopifnot(
    var_interest %in% names(data),
    all(names(at) %in% names(data)),
    is.numeric(cofint),
    cofint < 1, cofint > 0
  )

}
