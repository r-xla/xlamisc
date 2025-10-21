#' @title Least Recently Used Cache
#'
#' @description
#' An implementation of a least-recently-used cache built on top of [`utils::hashtab`], which
#' therefore takes care of hash collisions.
#'
#' @param capacity (`integer(1)`)\cr
#'   Number of items the cache holds.
#' @param key (`any`)\cr
#'   Key.
#' @param value (`any`)\cr
#'   Value
#' @param default (`any`)\cr
#'   Default value returned by `get()` when `key` is not present.
#'
#' @export
LRUCache <- R6::R6Class(
  classname = "LRUCache",
  public = list(
    #' @description Initialize a new LRU cache with a given `capacity`.
    initialize = function(capacity) {
      stopifnot(is.numeric(capacity), capacity >= 1, length(capacity) == 1)
      private$max_capacity <- as.integer(capacity)

      private$ht <- utils::hashtab(
        type = "identical",
        size = max(2L, private$max_capacity * 2L)
      )

      private$head <- new.env(parent = emptyenv())
      private$tail <- new.env(parent = emptyenv())
      private$head[["next"]] <- private$tail
      private$head$prev <- NULL
      private$tail$prev <- private$head
      private$tail[["next"]] <- NULL

      private$n_items <- 0L
    },

    #' @description Get the value for `key` and mark it as most-recently-used.
    get = function(key, default = NULL) {
      node <- utils::gethash(private$ht, key, nomatch = NULL)
      if (is.null(node)) return(default)
      private$move_to_front(node)
      node$value
    },

    #' @description Set the value for `key`, updating recency and evicting LRU as needed.
    set = function(key, value) {
      node <- utils::gethash(private$ht, key, nomatch = NULL)
      if (!is.null(node)) {
        node$value <- value
        private$move_to_front(node)
        return(invisible(value))
      }
      node <- private$mk_node(key, value)
      utils::sethash(private$ht, key, node)
      private$add_front(node)
      private$n_items <- private$n_items + 1L
      if (private$n_items > private$max_capacity) {
        victim <- private$pop_lru()
        if (!is.null(victim)) {
          utils::remhash(private$ht, victim$key)
          private$n_items <- private$n_items - 1L
        }
      }
      invisible(value)
    },

    #' @description Check whether a given `key` exists in the cache.
    has = function(key) {
      !is.null(utils::gethash(private$ht, key, nomatch = NULL))
    },

    #' @description Remove `key` from the cache, returning `TRUE` if it was present.
    remove = function(key) {
      node <- utils::gethash(private$ht, key, nomatch = NULL)
      if (is.null(node)) return(FALSE)
      private$remove_node(node)
      utils::remhash(private$ht, key)
      private$n_items <- private$n_items - 1L
      TRUE
    },

    #' @description Clear all entries from the cache.
    clear = function() {
      utils::clrhash(private$ht)
      private$head[["next"]] <- private$tail
      private$tail$prev <- private$head
      private$n_items <- 0L
      invisible(NULL)
    },

    #' @description Return the number of items currently stored.
    size = function() {
      private$n_items
    },

    #' @description Return the maximum capacity of the cache.
    capacity = function() {
      private$max_capacity
    },

    #' @description Return keys ordered from most-recently-used to least-recently-used.
    keys_mru_to_lru = function() {
      out <- list()
      i <- 0L
      cur <- private$head[["next"]]
      while (!identical(cur, private$tail)) {
        i <- i + 1L
        out[[i]] <- cur$key
        cur <- cur[["next"]]
      }
      out
    }
  ),
  private = list(
    ht = NULL,
    head = NULL,
    tail = NULL,
    n_items = 0L,
    max_capacity = NULL,

    mk_node = function(key, value) {
      n <- new.env(parent = emptyenv())
      n$key <- key
      n$value <- value
      n$prev <- NULL
      n[["next"]] <- NULL
      n
    },
    add_front = function(node) {
      node$prev <- private$head
      node[["next"]] <- private$head[["next"]]
      private$head[["next"]]$prev <- node
      private$head[["next"]] <- node
    },
    remove_node = function(node) {
      p <- node$prev
      q <- node[["next"]]
      p[["next"]] <- q
      q$prev <- p
      node$prev <- NULL
      node[["next"]] <- NULL
    },
    move_to_front = function(node) {
      private$remove_node(node)
      private$add_front(node)
    },
    pop_lru = function() {
      n <- private$tail$prev
      if (identical(n, private$head)) return(NULL)
      private$remove_node(n)
      n
    }
  )
)
