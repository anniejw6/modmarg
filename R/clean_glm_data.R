clean_glm_data <- function(mod, data, weights){

  # Grab only necessary variables
  data <- get_all_vars(mod, data)

  if(!is.null(weights) & length(weights) != nrow(data))
    stop('`weights` and `data` must be the same length.')

  # Add weights
  wgt_col <- paste(sample(c(letters, 1:9), 10), collapse = '')
  data[[wgt_col]] <- weights

  # Keep completes only
  data <- na.omit(data)

  # Remove weights
  weights <- data[[wgt_col]]
  data[[wgt_col]] <- NULL

  # Remove any booleans
  if(all(data$`T` == TRUE))
    data$`T` <- NULL
  if(all(data$`F` == FALSE))
    data$`F` <- NULL

  list(data = data, weights = weights)

}
