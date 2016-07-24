#' Get Margins
#'
#' @param preds list of individual predictive margins
#'
#' @return vector of means
#' @export
#' @examples
#' get_margins(list(1:3, 8:10))
get_margins <- function(preds){

  stopifnot(is.list(preds), all(vapply(preds, is.numeric, logical(1) )) )

  unlist(lapply(preds, mean))

}
