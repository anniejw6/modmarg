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
at_transform <- function(df, var_name, value){

  stopifnot( is.factor(df[[var_name]]) | is.numeric(df[[var_name]]),
             is.data.frame(df) )

  # figure out if factor
  if(is.factor(df[[var_name]])){
    # necessary because of factors
    df[[var_name]] <- factor(value, levels = levels(df[[var_name]]))
  } else {
    df[[var_name]] <- value
  }

  df

}


#' Apply multiple transformations
#'
#' @param df dataframe
#' @param var_name variable
#' @param values vector of values for that variable
#'
#' @return list of transformed dataframes
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars$gear <- factor(mtcars$gear)
#' df3 <- transform(mtcars, gear = factor(3, levels = levels(mtcars$gear)))
#' df <- at_transforms(df = mtcars, var_name = 'gear', values = c(3, 5))
at_transforms <- function(df, var_name, values){

  stopifnot(is.vector(values), is.character(var_name), var_name %in% names(df),
            is.data.frame(df) )

  lapply(values, function(x) at_transform(df, var_name, x))

}
