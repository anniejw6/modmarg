# Wrapper function to check inputs
check_inputs <- function(weights, data, var_interest,
                         at, cofint, base_rn, type){

  # Warn if base_rn set but type != 'effects'
  if(base_rn != 1 & type != 'effects')
    warning("Setting base_rn when type == 'levels' is ignored.")

  # Check for polynomials
  # Should be a general thing, but may have to be specific

  # Weights should be same length as data
  if(!is.null(weights) & length(weights) != nrow(data))
    stop('`weights` and `data` must be the same length.')

  stopifnot(
    var_interest %in% names(data),
    all(names(at) %in% names(data)),
    is.numeric(cofint),
    cofint < 1, cofint > 0
  )

}
