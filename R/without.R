#' @title Remove elements from a vector
#' @description
#' Remove elements from a vector by index.
#' Also works for empty indices.
#' @param x (`vector`)\cr
#'   The vector to remove elements from.
#' @param indices (`integer()`)\cr
#'   The indices to remove.
#' @return (`vector`)\cr
#'   The vector with the elements removed.
#' @export
without <- function(x, indices) {
  if (length(indices)) {
    x[-indices]
  } else {
    x
  }
}
