#' Format Bibentries in Roxygen
#'
#' @description
#' Operates on a named list of [bibentry()] entries and formats them for
#' documentation with \CRANpkg{roxygen2}.
#'
#' * `format_bib()` is intended to be called in the `@references` section and
#'   formats the complete entry using [tools::toRd()].
#' * `cite_bib()` returns a short inline citation listing all authors, e.g.
#'   `"A (Year)"` for one author, `"A & B (Year)"` for two, `"A, B & C (Year)"`
#'   for three, and `"A et al. (Year)"` for four or more.
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
format_bib <- function(..., bibentries = NULL) {
  # nolint
  if (is.null(bibentries)) {
    bibentries <- get("bibentries", envir = parent.frame())
  }
  checkmate::assert_list(bibentries, "bibentry", names = "unique")
  keys <- list(...)
  str <- vapply(
    keys,
    function(entry) tools::toRd(bibentries[[entry]]),
    character(1)
  )
  paste0(str, collapse = "\n\n")
}

#' @rdname format_bib
#' @export
cite_bib <- function(..., bibentries = NULL) {
  # nolint
  if (is.null(bibentries)) {
    bibentries <- get("bibentries", envir = parent.frame())
  }
  checkmate::assert_list(bibentries, "bibentry", names = "unique")

  keys <- list(...)
  str <- vapply(
    keys,
    function(entry) {
      x <- bibentries[[entry]]
      sprintf("%s (%s)", format_authors(x$author), x$year)
    },
    character(1)
  )

  if (length(str) >= 3L) {
    str <- c(toString(utils::head(str, -1L)), utils::tail(str, 1L))
  }

  paste0(str, collapse = " and ")
}

# Formats a `person()` list as e.g. "A", "A & B", "A, B & C", "A et al."
format_authors <- function(authors) {
  families <- vapply(
    authors,
    function(p) {
      family <- p$family
      # Organizational/mononymous authors (e.g. person(given = "R Core Team"))
      # have no family name.
      if (is.null(family)) as.character(p) else family
    },
    character(1)
  )

  n <- length(families)
  if (n <= 2L) {
    paste(families, collapse = " & ")
  } else if (n == 3L) {
    paste0(toString(families[1:2]), " & ", families[3L])
  } else {
    paste(families[1L], "et al.")
  }
}
