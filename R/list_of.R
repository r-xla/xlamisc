#' @title Create a new list of class
#' @description Create a new list of class
#' @param class_name The name of the class
#' @param item_class The class of the items in the list
#' @param validator A validator function
#' @return A list of class
#' @export
new_list_of <- function(class_name, item_class, validator = NULL) {
  function(items = list()) {
    checkmate::assert_list(items, item_class)

    # Run custom validator if provided
    if (!is.null(validator)) {
      validator <- get("validator") # r-cmd-check NOTE: undefined global
      err <- validator(items)
      if (!checkmate::test_null(err)) {
        cli::cli_abort(err)
      }
    }

    structure(
      items,
      class = c(class_name, "list_of", "list")
    )
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
