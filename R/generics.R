#' @title Shape
#' @description
#' Get the shape of a tensor.
#' @param x (any)\cr
#'   Object.
#' @param ... (any)\cr
#'   Additional arguments.
#' @return (`integer()`)\cr
#'   The shape of the tensor.
#' @export
shape <- function(x, ...) {
  UseMethod("shape")
}

#' @title Data Type
#' @description
#' Get the data type of a tensor.
#' The return type depends on the specific tensor implementation.
#' @param x (any)\cr
#'   Object.
#' @param ... (any)\cr
#'   Additional arguments.
#' @return (any)
#' @export
dtype <- function(x, ...) {
  UseMethod("dtype")
}

#' @title Device
#' @description
#' Get the device of a tensor.
#' @param x (any)\cr
#'   Object.
#' @param ... (any)\cr
#'   Additional arguments.
#' @return (any)
#' @export
device <- function(x, ...) {
  UseMethod("device")
}

#' @title As Array
#' @description
#' Convert a value (usually a tensor from some framework) to an R array.
#' @param x (any)\cr
#'   Object.
#' @param ... (any)\cr
#'   Additional arguments.
#' @return (any)
#' @export
as_array <- function(x, ...) {
  UseMethod("as_array")
}


#' @title As Raw
#' @description
#' Convert a value (usually a tensor from some framework) to a `raw()` vector.
#' @param x (any)\cr
#'   Object.
#' @param ... (any)\cr
#'   Additional arguments.
#' @return (any)
#' @export
as_raw <- function(x, ...) {
  UseMethod("as_raw")
}

#' @title Number of Axes
#' @description
#' Get the number of axes of a tensor.
#' @param x (any)\cr
#'   Object.
#' @return (integer)
#' @export
naxes <- function(x) {
  length(shape(x))
}

#' @title Number of Elements
#' @description
#' Get the number of elements of a tensor.
#' @param x (any)\cr
#'   Object.
#' @return (integer)
#' @export
nelts <- function(x) {
  prod(shape(x))
}

#' @export
shape.array <- function(x, ...) {
  dim(x)
}
