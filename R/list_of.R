#' @title Create a new list of class
#' @description
#' Returns a constructor for a typed list. The constructor checks that
#' `items` is a list, but does not iterate `items` to validate
#' per-element class -- this makes it cheap enough to call from hot
#' paths like IR construction. Trust the caller to pass items of the
#' right class; downstream consumers will fail with their own (clearer)
#' error if not.
#' @param class_name The name of the class
#' @param item_class The class of the items in the list. Documentation
#'   only -- not enforced at construction.
#' @return A list-of-`class_name` constructor.
#' @export
new_list_of <- function(class_name, item_class) {
  classes <- c(class_name, "list_of", "list")
  function(items = list()) {
    if (!is.list(items)) {
      cli::cli_abort(
        "`items` must be a list, not {.cls {class(items)[[1L]]}}"
      )
    }
    structure(items, class = classes)
  }
}

#' @export
`==.list_of` <- function(e1, e2) {
  length(e1) == length(e2) &&
    all(
      vapply(
        seq_along(e1),
        function(i) {
          e1[[i]] == e2[[i]]
        },
        logical(1)
      )
    )
}

#' @export
`!=.list_of` <- function(e1, e2) {
  length(e1) != length(e2) ||
    any(
      vapply(
        seq_along(e1),
        function(i) {
          e1[[i]] != e2[[i]]
        },
        logical(1)
      )
    )
}

#' @export
length.list_of <- function(x) {
  length(unclass(x))
}
