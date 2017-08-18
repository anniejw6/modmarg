clean_glm_data <- function(mod, data, weights){

  # Grab only necessary variables
  data <- get_all_vars(mod, data)

  if(!is.null(weights) & length(weights) != nrow(data))
    stop('`weights` and `data` must be the same length.')

  # Add weights
  data$weights <- weights

  # Keep completes only
  data <- na.omit(data)

  # Remove any booleans
  if(all(data$`T` == TRUE))
    data$`T` <- NULL
  if(all(data$`F` == FALSE))
    data$`F` <- NULL

  data

}
