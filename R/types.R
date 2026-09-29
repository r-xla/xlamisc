#' @import checkmate
#' @importFrom cli cli_abort
#' @importFrom rlang %||%
#' @importFrom utils hashtab sethash gethash
NULL

#' DataType
#'
#' @description
#' `DataType` is the S3 class representing a tensor element type.
#' It is an enum: one singleton object per supported dtype, wrapping the
#' canonical dtype string. The members are the element types supported by
#' the r-xla stack:
#'
#' - boolean: `bool`
#' - signed integers: `i8`, `i16`, `i32`, `i64`
#' - unsigned integers: `ui8`, `ui16`, `ui32`, `ui64`
#' - floats: `f16`, `bf16`, `f32`, `f64`
#' - complex: `c64`, `c128`
#'
#' Construct a `DataType` with [as_dtype()]. Inspect it with [dtype_width()]
#' and the [is_dtype_float()] family of predicates.
#'
#' @seealso [as_dtype()], [dtype_width()], [is_dtype_float()]
#' @name DataType
NULL

# One entry per dtype: category ("bool"/"int"/"uint"/"float"/"complex") and
# bit width. Order and spelling define the enum.
dtype_info <- list(
  bool = list(category = "bool", bits = 1L),
  i8 = list(category = "int", bits = 8L),
  i16 = list(category = "int", bits = 16L),
  i32 = list(category = "int", bits = 32L),
  i64 = list(category = "int", bits = 64L),
  ui8 = list(category = "uint", bits = 8L),
  ui16 = list(category = "uint", bits = 16L),
  ui32 = list(category = "uint", bits = 32L),
  ui64 = list(category = "uint", bits = 64L),
  f16 = list(category = "float", bits = 16L),
  bf16 = list(category = "float", bits = 16L),
  f32 = list(category = "float", bits = 32L),
  f64 = list(category = "float", bits = 64L),
  c64 = list(category = "complex", bits = 64L),
  c128 = list(category = "complex", bits = 128L)
)

# Hash table mapping every accepted dtype spelling (the canonical names plus
# the "pred"/"i1" aliases for "bool") to list(object, category, bits), where
# `object` is the singleton DataType. Built in .onLoad(): hashtabs must not be
# stored in the namespace's lazy-load database.
dtype_table <- NULL

build_dtype_table <- function() {
  table <- hashtab(size = length(dtype_info) + 2L)
  for (name in names(dtype_info)) {
    info <- dtype_info[[name]]
    sethash(table, name, list(
      object = structure(name, class = "DataType"),
      category = info$category,
      bits = info$bits
    ))
  }
  for (alias in c("pred", "i1")) {
    sethash(table, alias, gethash(table, "bool"))
  }
  table
}

# The table entry for a DataType; errors on names outside the enum (which can
# only be reached via hand-crafted objects).
dtype_entry <- function(x) {
  assert_dtype(x)
  gethash(dtype_table, unclass(x)) %||%
    cli_abort("Unknown dtype: {unclass(x)}")
}

#' @title Convert to DataType
#' @description
#' Convert to [`DataType`].
#' @param x (any)\cr
#'   A dtype string (e.g. `"f32"`, `"i64"`, `"bool"`; the aliases `"pred"`
#'   and `"i1"` map to `"bool"`) or a [`DataType`] object.
#' @return `DataType`
#' @export
as_dtype <- function(x) {
  UseMethod("as_dtype")
}

#' @export
as_dtype.default <- function(x) {
  cli_abort("Cannot convert {.cls {class(x)[1]}} to DataType")
}

#' @export
as_dtype.DataType <- function(x) {
  x
}

#' @export
as_dtype.character <- function(x) {
  assert_string(x)
  entry <- gethash(dtype_table, x) %||% cli_abort("Unsupported dtype: {x}")
  entry$object
}

#' @title Is DataType
#' @description
#' Check if an object is a [`DataType`].
#' @param x (any)\cr
#'   Object to check.
#' @return `logical(1)`
#' @export
is_dtype <- function(x) {
  inherits(x, "DataType")
}

#' @title Assert DataType
#' @description
#' Assert that an object is a [`DataType`].
#' @param x (any)\cr
#'   Object to check.
#' @param arg (`character(1)`)\cr
#'   Argument name for error messages.
#' @return `NULL` (invisibly). Called for side effects.
#' @export
assert_dtype <- function(x, arg = rlang::caller_arg(x)) {
  if (!is_dtype(x)) {
    cli_abort("{.arg {arg}} must be a DataType")
  }
}

#' @title Width of a DataType
#' @description
#' The width in bits of one element of the given [`DataType`].
#' For `bool` this is `1`; for complex types it is the total width of the
#' element (`c64` -> `64`).
#' @param x ([`DataType`])
#' @return `integer(1)`
#' @export
dtype_width <- function(x) {
  dtype_entry(x)$bits
}

#' @title Category of a DataType
#' @description
#' The category of a [`DataType`]: one of `"bool"`, `"int"`, `"uint"`,
#' `"float"`, or `"complex"`.
#' @param x ([`DataType`])
#' @return `character(1)`
#' @seealso [is_dtype_float()] and friends for per-category predicates
#' @export
dtype_category <- function(x) {
  dtype_entry(x)$category
}

#' @title DataType Category Predicates
#' @description
#' Check which category a [`DataType`] belongs to:
#' - `is_dtype_float()`: floating point, including `bf16`.
#' - `is_dtype_int()`: signed integers.
#' - `is_dtype_uint()`: unsigned integers.
#' - `is_dtype_bool()`: `bool`.
#' - `is_dtype_complex()`: `c64`, `c128`.
#' @param x ([`DataType`])
#' @return `logical(1)`
#' @export
is_dtype_float <- function(x) {
  dtype_category(x) == "float"
}

#' @rdname is_dtype_float
#' @export
is_dtype_int <- function(x) {
  dtype_category(x) == "int"
}

#' @rdname is_dtype_float
#' @export
is_dtype_uint <- function(x) {
  dtype_category(x) == "uint"
}

#' @rdname is_dtype_float
#' @export
is_dtype_bool <- function(x) {
  dtype_category(x) == "bool"
}

#' @rdname is_dtype_float
#' @export
is_dtype_complex <- function(x) {
  dtype_category(x) == "complex"
}

#' @export
`==.DataType` <- function(e1, e2) {
  if (is.character(e1)) {
    e1 <- as_dtype(e1)
  }
  if (is.character(e2)) {
    e2 <- as_dtype(e2)
  }
  if (!is_dtype(e1) || !is_dtype(e2)) {
    return(FALSE)
  }
  unclass(e1) == unclass(e2)
}

#' @export
`!=.DataType` <- function(e1, e2) {
  # jarl-ignore comparison_negation: `e1 != e2` would recurse into this method
  !(e1 == e2)
}

#' @export
as.character.DataType <- function(x, ...) {
  as.character(unclass(x))
}

#' @export
print.DataType <- function(x, ...) {
  cat("<", as.character(x), ">\n", sep = "")
  invisible(x)
}

#' @exportS3Method cli::cli_format
cli_format.DataType <- function(x, style = NULL, ...) {
  as.character(x)
}
