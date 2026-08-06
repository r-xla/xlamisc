test_that("new_list_of constructs and classes the list", {
  Foos <- new_list_of("Foos", "Foo")
  x <- Foos(list(1, 2))
  expect_s3_class(x, c("Foos", "list_of", "list"), exact = TRUE)
  expect_length(x, 2L)
  expect_length(Foos(), 0L)
})

test_that("new_list_of rejects non-lists", {
  Foos <- new_list_of("Foos", "Foo")
  expect_error(Foos(1:3), "must be a list")
})

test_that("new_list_of runs the validator", {
  Foos <- new_list_of("Foos", "Foo", validator = function(items) {
    if (length(items) > 2L) "too many items" else NULL
  })
  expect_length(Foos(list(1, 2)), 2L)
  expect_error(Foos(list(1, 2, 3)), "too many items")
})

test_that("new_list_of validates the list before the validator sees it", {
  Foos <- new_list_of("Foos", "Foo", validator = function(items) {
    stop("validator must not run on a non-list")
  })
  expect_error(Foos(1:3), "must be a list")
})
