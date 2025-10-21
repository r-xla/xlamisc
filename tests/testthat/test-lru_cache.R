test_that("LRUCache basic operations work", {
  cache <- LRUCache$new(2)
  expect_equal(cache$size(), 0L)
  expect_equal(cache$capacity(), 2L)

  expect_null(cache$get("a"))
  expect_false(cache$has("a"))

  cache$set("a", 1)
  expect_true(cache$has("a"))
  expect_equal(cache$get("a"), 1)
  expect_equal(cache$size(), 1L)

  cache$set("b", 2)
  expect_equal(cache$size(), 2L)

  # access 'a' to make it MRU, so 'b' will be LRU
  expect_equal(cache$get("a"), 1)
  cache$set("c", 3) # evicts 'b'
  expect_false(cache$has("b"))
  expect_true(cache$has("a"))
  expect_true(cache$has("c"))

  # remove specific key
  expect_true(cache$remove("a"))
  expect_false(cache$remove("a"))
  expect_equal(cache$size(), 1L)

  # clear
  cache$clear()
  expect_equal(cache$size(), 0L)
})

test_that("LRU order is maintained (MRU -> LRU)", {
  cache <- LRUCache$new(3)
  cache$set("a", 1)
  cache$set("b", 2)
  cache$set("c", 3)
  # Current order: c, b, a
  expect_equal(cache$keys_mru_to_lru(), list("c", "b", "a"))

  # Touch 'a' -> now order: a, c, b
  expect_equal(cache$get("a"), 1)
  expect_equal(cache$keys_mru_to_lru(), list("a", "c", "b"))

  # Update existing key moves to front
  cache$set("b", 22)
  expect_equal(cache$keys_mru_to_lru(), list("b", "a", "c"))

  # Insert beyond capacity evicts LRU ('c')
  cache$set("d", 4)
  expect_equal(cache$keys_mru_to_lru(), list("d", "b", "a"))
  expect_false(cache$has("c"))
})

