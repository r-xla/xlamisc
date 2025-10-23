#' @title Least Recently Used Cache
#' @importFrom utils hashtab
#'
#' @description
#' An implementation of a least-recently-used cache built on top of [`utils::hashtab`].
#' Therefore, arbitrary keys can be used, as opposed to the implementation in \CRANpkg{cachemem},
#' which relies on environments.
#'
#' @details
#' This LRU cache is implemented as a combination of a hashmap and a doubly linked list.
#' The hashmap is used for lookups and the doubly linked list is used to maintain the
#' ordering of most recently used to least recently used items.
#' Whenever an element is added or accessed, it is moved to the front of the list.
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
LRUCache <- R6Class(
  classname = "LRUCache",
  public = list(
    #' @description Initialize a new LRU cache with a given `capacity`.
    initialize = function(capacity) {
      stopifnot(is.numeric(capacity), capacity >= 1, length(capacity) == 1)
      private$.capacity <- as.integer(capacity)

      private$.hash_table <- hashtab(
        size = max(2L, private$.capacity * 2L)
      )

      # they don't contain data, they just mark the boundaries of the linked list
      private$.head <- private$.mk_node(NULL, NULL)
      private$.tail <- private$.mk_node(NULL, NULL)
      private$.head[["next"]] <- private$.tail
      private$.tail[["prev"]] <- private$.head

      private$.n_items <- 0L
    },

    #' @description Get the value for `key` and mark it as most-recently-used.
    get = function(key, default = NULL) {
      node <- utils::gethash(private$.hash_table, key, nomatch = NULL)
      if (is.null(node)) {
        return(default)
      }
      private$.move_to_front(node)
      node[["value"]]
    },

    #' @description Set the value for `key`, updating recency and evicting LRU as needed.
    set = function(key, value) {
      if (is.null(value)) {
        stop("NULL values cannot be stored.")
      }
      node <- utils::gethash(private$.hash_table, key, nomatch = NULL)
      if (!is.null(node)) {
        node[["value"]] <- value
        private$.move_to_front(node)
        return(invisible(value))
      }
      node <- private$.mk_node(key, value)
      utils::sethash(private$.hash_table, key, node)
      private$.add_front(node)
      private$.n_items <- private$.n_items + 1L
      if (private$.n_items > private$.capacity) {
        victim <- private$.pop_lru()
        utils::remhash(private$.hash_table, victim[["key"]])
        private$.n_items <- private$.n_items - 1L
      }
      invisible(value)
    },

    #' @description Check whether a given `key` exists in the cache.
    has = function(key) {
      !is.null(utils::gethash(private$.hash_table, key, nomatch = NULL))
    },

    #' @description Remove `key` from the cache, returning the value if it was present or `NULL` otherwise.
    remove = function(key) {
      node <- utils::gethash(private$.hash_table, key, nomatch = NULL)
      if (is.null(node)) {
        return(NULL)
      }
      private$.remove_node(node)
      utils::remhash(private$.hash_table, key)
      private$.n_items <- private$.n_items - 1L
      node[["value"]]
    },

    #' @description Clear all entries from the cache.
    clear = function() {
      utils::clrhash(private$.hash_table)
      private$.head[["next"]] <- private$.tail
      private$.tail[["prev"]] <- private$.head
      private$.n_items <- 0L
      invisible(NULL)
    },

    #' @description Return keys ordered from most-recently-used to least-recently-used.
    keys_mru_to_lru = function() {
      out <- vector("list", private$.n_items)
      cur <- private$.head[["next"]]
      for (i in seq_len(private$.n_items)) {
        out[[i]] <- cur[["key"]]
        cur <- cur[["next"]]
      }
      out
    }
  ),
  active = list(
    #' @field capacity (`integer(1)`)\cr
    #'   The maximum capacity of the cache.
    capacity = function() {
      private$.capacity
    },
    #' @field size (`integer(1)`)\cr
    #'   The number of items currently stored.
    size = function() {
      private$.n_items
    }
  ),
  private = list(
    .hash_table = NULL,
    .head = NULL,
    .tail = NULL,
    .n_items = 0L,
    .capacity = NULL,

    .mk_node = function(key, value) {
      n <- hashtab()
      n[["key"]] <- key
      n[["value"]] <- value
      n[["prev"]] <- NULL
      n[["next"]] <- NULL
      n
    },
    .add_front = function(node) {
      node[["prev"]] <- private$.head
      node[["next"]] <- private$.head[["next"]]
      private$.head[["next"]][["prev"]] <- node
      private$.head[["next"]] <- node
    },
    .remove_node = function(node) {
      p <- node[["prev"]]
      q <- node[["next"]]
      p[["next"]] <- q
      q[["prev"]] <- p
      node[["prev"]] <- NULL
      node[["next"]] <- NULL
    },
    .move_to_front = function(node) {
      private$.remove_node(node)
      private$.add_front(node)
    },
    .pop_lru = function() {
      n <- private$.tail[["prev"]]
      if (identical(n, private$.head)) {
        return(NULL)
      }
      private$.remove_node(n)
      n
    }
  )
)
