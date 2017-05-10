# Transform data (big wrapper)
# Would use `transform`, but the syntax for factors is annoying
at_transform <- function(var, value){

  # var: vector
  # value: character or numeric, value of variable
  #
  # return: vector with transformed variable

  if(is.factor(var)){
    factor(rep(value, length(var)), levels = levels(var))
  } else {
    rep(value, length(var))
  }

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
  for(i in seq_len(nrow(all_combos))){

    df_tmp <- model_df

    for(j in names(all_combos)){
      df_tmp[[j]] <- at_transform(var = df_tmp[[j]], value = all_combos[i, j])
    }

    df[[i]] <- df_tmp
  }

  # Give names to list
  names(df) <- apply(all_combos, 1, FUN = function(x){
    paste(names(all_combos), "=", x, collapse = ' ')
  })

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
