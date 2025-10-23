#' @title Get the dimensions of a data object
#' @description
#' Get the "dimension" of an R array or vector.
#' @param data (`any`)\cr
#'   The data object to get the dimensions of.
#' @return (`integer()`)
#' @export
get_dims <- function(data) {
  if (is.null(dim(data))) {
    if (length(data) == 1) {
      return(1L)
    } else if (length(data) == 0) {
      return(integer())
    } else {
      return(length(data))
    }
  }
  dim(data)
}
