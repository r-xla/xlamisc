#' @title Sequence of length 0
#' @name seq0
#' @description
#' Like [`seq_len`] and [`seq_along`] but starts at 0.
#' @param n (`integer(1)`)\cr
#'   The length of the sequence.
#' @param x (`integer()`)\cr
#'   The vector to sequence along.
#' @return (`integer()`)
#' @export
seq_len0 <- function(n) {
  x <- seq_len(n)
  if (length(x)) x - 1L else integer()
}

#' @rdname seq0
#' @export
seq_along0 <- function(x) {
  x <- seq_along(x)
  if (length(x)) x - 1L else integer()
}
