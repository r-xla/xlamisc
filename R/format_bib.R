#' Format Bibentries in Roxygen
#'
#' @description
#' Operates on a named list of [bibentry()] entries and formats them for
#' documentation with \CRANpkg{roxygen2}.
#'
#' * `format_bib()` is intended to be called in the `@references` section and
#'   formats the complete entry using [tools::toRd()].
#' * `cite_bib()` returns a short inline citation in the format
#'   `"LastName (Year)"`.
#'
#' @param ... (`character()`)\cr
#'   One or more names of bibentries.
#' @param bibentries (named `list()`)\cr
#'   Named list of bibentries. If `NULL`, looks up an object called
#'   `bibentries` in the calling environment.
#'
#' @return (`character(1)`).
#'
#' @export
#' @examples
#' bibentries = list(R = citation())
#' format_bib("R")
#' cite_bib("R")
format_bib = function(..., bibentries = NULL) {
  # nolint
  if (is.null(bibentries)) {
    bibentries = get("bibentries", envir = parent.frame())
  }
  checkmate::assert_list(bibentries, "bibentry", names = "unique")
  keys = list(...)
  str = vapply(
    keys,
    function(entry) tools::toRd(bibentries[[entry]]),
    character(1)
  )
  paste0(str, collapse = "\n\n")
}

#' @rdname format_bib
#' @export
cite_bib = function(..., bibentries = NULL) {
  # nolint
  if (is.null(bibentries)) {
    bibentries = get("bibentries", envir = parent.frame())
  }
  checkmate::assert_list(bibentries, "bibentry", names = "unique")

  keys = list(...)
  str = vapply(
    keys,
    function(entry) {
      x = bibentries[[entry]]
      family = x$author[[1L]]$family
      if (is.null(family)) {
        family = x$author[[1L]]
      }
      sprintf("%s (%s)", family, x$year)
    },
    character(1)
  )

  if (length(str) >= 3L) {
    str = c(toString(utils::head(str, -1L)), utils::tail(str, 1L))
  }

  paste0(str, collapse = " and ")
}
