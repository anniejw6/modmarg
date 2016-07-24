#' Estimating predictive margins on a model
#'
#' @param mod model object, currently only support those of class glm
#' @param var_interest variable of interest
#' @param type either 'levels' (predicted outcomes) or 'effects' (dydx), defaults to 'levels'
#' @param at list, should be in the format of list('var_name' = c(values)), defaults to NULL.
#' This calculates the margins of the variable at these particular variables.
#'
#' @return
#' @export
#'
#' @examples
#' data(mtcars)
#' mtcars$gear <- factor(mtcars$gear)
#' mod <- glm(vs ~ gear + mpg * disp, mtcars, family = 'binomial')
#' var_interest <- 'gear'
#' type <- 'levels'
#' at <- list(mpg = c(15, 21), disp = c(140, 180))
mod_marg2 <- function(mod, var_interest,
                      type = 'levels',
                      at = NULL){

  stopifnot(
    'glm' %in% class(mod),
    var_interest %in% names(mod$model),
    all(names(at) %in% names(mod$model))
    # TODO: warning if at contains extrapolated values
    )

  # Transform things ----

  # Transform the ats
  if(!is.null(at)){

    all_combos <- expand.grid(at)
    df <- list(length = nrow(all_combos))

    for(i in 1:nrow(all_combos)){

      df_tmp <- mod$model

      for(j in names(all_combos)){

        df_tmp <- at_transform(df = df_tmp, var_name = j, value = all_combos[i, j])
      }

      df[[i]] <- df_tmp
    }

    # TODO: write wrapper function around this shit.
    names(df) <- do.call(paste,
                         data.frame(
                           do.call(cbind, lapply(names(all_combos),
                                                 function(name) sprintf("%s = %s", name, all_combos[[name]]))
                                   )
                           )
                         )
  } else {

    df <- list(mod$model)

  }


  ### TODO: one oyu have an "at" dataframe (or multiple),
  # write ONE function to act on that ---> returns levels or effects, i.e., full output dataframe

  # Transform the var

  # for every level of the variable of interest, create the transformations
  # for every dataframe that's transformed based on "at"
  df <- lapply(unique(df[[var_interest]]), function(unique_var_interest){

    lapply(df, function(x){
      at_transform(df = x, var_name = var_interest, value = unique_var_interest)
    })

  })


  # Get Margins -----
  predict_modelmat(model, #
                   transformed_df, #
                   formula) #

  get_margins(preds) # list of predictions (grab from looped-ver predict_modelmat)

  # get effects <- to write

  # Get SE -----

  jacob_level(pred_values, covar_matrix, link_deriv) # run this always
  jacob_effect(jacobian, base_rn) # run this if effect


  # Format Output -----
  format_output( # maybe reformat this to var, value, at?
    margin_labels,
    pred_margins,
    se,
    cofint
  )

}


