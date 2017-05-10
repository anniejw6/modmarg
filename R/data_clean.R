# Transform data (big wrapper)
# Would use `transform`, but the syntax for factors is annoying
at_transform <- function(df, var_name, value){

  # df: dataframe of values
  # var_name: character, variable name
  # value: character or numeric, value of variable
  #
  # return: dataframe with transformed variable

  # figure out if factor
  if(is.factor(df[[var_name]])){
    # necessary because of factors
    df[[var_name]] <- factor(value, levels = levels(df[[var_name]]))
  } else {
    df[[var_name]] <- value
  }

  df

}

# Apply multiple transformations
at_transforms <- function(model_df, at_list){

  # model_df: dataframe used in model (not model.matrix)
  # at_list: list of transformations, in the format of
  #          `list("variable" = c("values"))`
  #
  # return: list of dataframes, each transformed

  # Figure out all transformations
  all_combos <- expand.grid(at_list)

  # Allocate vector to hold them
  df <- vector(mode = 'list', length  = nrow(all_combos))

  # Loop through all combinations
  for(i in 1:nrow(all_combos)){

    df_tmp <- model_df

    for(j in names(all_combos)){

      df_tmp <- at_transform(df = df_tmp,
                             var_name = j, value = all_combos[i, j])
    }

    df[[i]] <- df_tmp
  }

  # Give names to list
  named_mat <- sapply(names(at_list), function(name){
    sprintf("%s = %s", name, all_combos[[name]]) })
  names(df) <- do.call(paste, data.frame(named_mat))

  # Return
  df
}

# Generate "at" transformation list for a single variable
gen_at_list <- function(df, var_interest, at_var_interest = NULL){

  # df: dataframe of values
  # var_interest: character, variable of interest
  # at_var_interest: vector, levels of variables of interest, defaults to NULL
  #
  # Return named list of all values for variable of interest

  stopifnot(var_interest %in% names(df))

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
