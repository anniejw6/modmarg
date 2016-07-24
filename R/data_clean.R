#' Transform data
#'
#' The reason why you want to use this instead of ?transform is that
#' the syntax for factors is really annoying.
#'
#' @param df dataframe
#' @param var_name variable name
#' @param value value of variable
#'
#' @return dataframe with transformed variable
#' @export
#' @examples
#' data(mtcars)
#' mtcars$gear <- factor(mtcars$gear)
#' df3 <- transform(mtcars, gear = factor(3, levels = levels(mtcars$gear)))
#' df <- apply_transform(df = mtcars, var_name = 'gear', value = 3)
#' str(df3)
#' str(df)
#' all(df == df3)
apply_transform <- function(df, var_name, value){

  stopifnot( is.factor(df[[var_name]]) | is.numeric(df[[var_name]]) )

  # figure out if factor
  if(is.factor(df[[var_name]])){
    # necessary because of factors
    df[[var_name]] <- factor(value, levels = levels(df[[var_name]]))
  } else {
    df[[var_name]] <- value
  }

  df

}

