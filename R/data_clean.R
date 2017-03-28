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
#' mod <- glm(cyl ~ gear + mpg, data = mtcars)
#' df3 <- transform(mtcars[, c('cyl', 'gear', 'mpg')],
#'                  gear = factor(3, levels = levels(mtcars$gear)))
#' df <- at_transform(mod = mod, df = mtcars, var_name = 'gear', value = 3)
#' str(df3)
#' str(df)
#' all(df == df3)
at_transform <- function(mod, df, var_name, value){

  if( is.factor(df[[var_name]]) ){
    df[[var_name]] <- as.character(value)
  } else {
    df[[var_name]] <- value
  }
  
  df
}

#' Apply multiple transformations
#'
#' @param model_df dataframe used in model (not model.matrix)
#' @param at_list list of transformations, in the format of list("variable" = c("values"))
#' @param model model used to generate model_df
#' @return list of dataframes, each transformed
#' @export
#'
#' @examples
#' data(mtcars)
#' at_list <- list("mpg" = c(15, 21), "disp" = c(140, 180))
#' at_transforms(mtcars, at_list)
at_transforms <- function(model_df, at_list, mod){

  # Figure out all transformations
  all_combos <- expand.grid(at_list)

  # Allocate vector to hold them
  df <- vector(mode = 'list', length  = nrow(all_combos))

  # Loop through all combinations
  for(i in 1:nrow(all_combos)){

    df_tmp <- model_df

    for(j in names(all_combos)){

      df_tmp <- at_transform(df = df_tmp, mod = mod,
                             var_name = j, value = all_combos[i, j])
    }

    df[[i]] <- df_tmp
  }

  # Give names to list
  named_mat <- sapply(names(at_list), function(name){ sprintf("%s = %s", name, all_combos[[name]]) })
  names(df) <- do.call(paste, data.frame(named_mat))

  # Return
  df
}


#' Generate "at" transformation list for a single variable
#'
#' This function will return all levels of the variable of interest
#'
#' @param df dataframe to be transformed
#' @param var_interest variable of interest
#' @param at_var_interest at levels for variables of interest, defaults to NULL
#'
#' @return named list of all values for variable of interest
#' @examples
#' data(mtcars)
#' mtcars$gear <- factor(mtcars$gear)
#' gen_at_list(mtcars, 'gear')
gen_at_list <- function(df, var_interest, at_var_interest = NULL){

  stopifnot(var_interest %in% names(df))
  if(is.null(at_var_interest) & !is.factor(df[[var_interest]])) stop('Must specify at_var_interest for continuous variables')

  if(is.null(at_var_interest)){
    # Get all unique values
    val_interest <- unique(df[[var_interest]])
    # order and put into list
    val_interest <- list(val_interest[order(val_interest)])
  } else {
    val_interest <- list(at_var_interest)
  }

  # Give name to list
  names(val_interest) <- var_interest

  val_interest
}
