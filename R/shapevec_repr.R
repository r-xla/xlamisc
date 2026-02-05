#' @title Format shape vector as string
#' @description
#' Formats a shape vector as a string like `(2,3,4)`.
#' NA values are replaced with `?`.
#' @param shape (`integer()`)\cr
#'   A shape vector.
#' @return (`character(1)`)\cr
#'   The formatted shape string.
#' @export
shapevec_repr <- function(shape) {
  shape[is.na(shape)] <- "?"
  sprintf("(%s)", paste(shape, collapse = ","))
}

#' @title Format multiple shape vectors
#' @description
#' Applies [shapevec_repr()] to multiple shapes and combines them.
#' @param ... \cr
#'   Shape vectors to format.
#' @return (`character(1)`)\cr
#'   The formatted shapes, separated by ", ".
#' @export
shapevec_reprs <- function(...) {
  paste(vapply(list(...), shapevec_repr, character(1)), collapse = ", ")
}
