test_that("LRUCache initialization works", {
  cache <- LRUCache$new(10)
  expect_equal(cache$size, 0)
  expect_equal(cache$capacity, 10)
})

test_that("can add and get elements", {
  cache <- LRUCache$new(2)
  cache$set("a", 1)
  expect_equal(cache$get("a"), 1)
  cache$set("b", 2)
  expect_equal(cache$get("a"), 1)
  expect_equal(cache$get("b"), 2)
  expect_equal(cache$get("c", 123), 123)
  expect_equal(cache$get("c"), NULL)
})

test_that("has works", {
  cache <- LRUCache$new(1)
  cache$set("a", 1)
  expect_true(cache$has("a"))
  expect_false(cache$has("b"))
})

test_that("can remove elements", {
  cache <- LRUCache$new(1)
  cache$set("a", 1)
  expect_equal(cache$remove("a"), 1)
  expect_null(cache$remove("a"))
})

test_that("clear works", {
  cache <- LRUCache$new(2)
  cache$set("a", 1)
  cache$set("b", 2)
  expect_equal(cache$size, 2)
  cache$clear()
  expect_equal(cache$size, 0)
  expect_null(cache$get("a"))
  expect_null(cache$get("b"))
})

test_that("LRU order is maintained (MRU -> LRU)", {
  cache <- LRUCache$new(3)
  cache$set("a", 1)
  expect_equal(cache$get("a"), 1)
  cache$set("b", 2)
  expect_equal(cache$get("b"), 2)
  cache$set("c", 3)
  expect_equal(cache$get("c"), 3)
  expect_equal(cache$keys_mru_to_lru(), list("c", "b", "a"))

  expect_equal(cache$get("a"), 1)
  expect_equal(cache$keys_mru_to_lru(), list("a", "c", "b"))

  cache$set("b", 22)
  expect_equal(cache$get("b"), 22)
  expect_equal(cache$keys_mru_to_lru(), list("b", "a", "c"))

  cache$set("c", 33)
  expect_equal(cache$keys_mru_to_lru(), list("c", "b", "a"))

  cache$set("d", 4)
  expect_equal(cache$get("d"), 4)
  expect_equal(cache$keys_mru_to_lru(), list("d", "c", "b"))
})

test_that("cannot add NULL element", {
  cache <- LRUCache$new(1)
  expect_error(cache$set("a", NULL), "NULL values cannot be stored")
})
