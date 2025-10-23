## copied from: https://github.com/lawremi/wizrd/blob/main/R/utils.R
#
##' @title Create a list property
##' @description
##' Create a list property.
##' @param class The class of the list.
##' @param ... The properties of the list.
##' @export
#list_of <- function(class, ...) {
#  new_list_property(of = class, ...)
#}
#
#new_list_property <- function(
#  ...,
#  validator = NULL,
#  default = if (isTRUE(named)) {
#    quote(setNames(list(), character()))
#  } else {
#    quote(list())
#  },
#  of = S7::class_any,
#  named = NA,
#  min_length = 0L,
#  max_length = Inf
#) {
#  prop <- S7::new_property(
#    S7::class_list,
#    ...,
#    validator = function(value) {
#      c(
#        if (
#          !identical(of, S7::class_any) &&
#            !all(vapply(value, S7:::class_inherits, logical(1L), of))
#        ) {
#          paste("must only contain elements of class", S7:::class_desc(of))
#        },
#        if (!is.null(of_validator)) {
#          msgs <- unlist(lapply(value, of_validator))
#          if (length(msgs) > 0L) {
#            paste(
#              "element(s) failed validation:",
#              paste0("'", unique(msgs), "'", collapse = ", ")
#            )
#          }
#        },
#        if (isTRUE(named) && is.null(names(value))) {
#          "must have names"
#        },
#        if (identical(named, FALSE) && !is.null(names(value))) {
#          "must not have names"
#        },
#        if (length(value) < min_length || length(value) > max_length) {
#          paste0("must have length in [", min_length, ", ", max_length, "]")
#        },
#        if (!is.null(validator)) {
#          validator(value)
#        }
#      )
#    },
#    default = default
#  )
#  prop$of <- of
#  if (inherits(of, "S7_property")) {
#    of_validator <- of$validator
#    of <- of$class
#  } else {
#    of_validator <- NULL
#  }
#  prop$named <- named
#  class(prop) <- c("list_S7_property", class(prop))
#  prop
#}
#
