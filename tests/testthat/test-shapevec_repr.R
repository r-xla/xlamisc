test_that("shapevec_repr formats shapes correctly", {
  expect_equal(shapevec_repr(c(2, 3, 4)), "(2,3,4)")
  expect_equal(shapevec_repr(c(1)), "(1)")
  expect_equal(shapevec_repr(integer(0)), "()")
})

test_that("shapevec_repr handles NA as ?", {

  expect_equal(shapevec_repr(c(2, NA, 4)), "(2,?,4)")
  expect_equal(shapevec_repr(c(NA, NA)), "(?,?)")
})

test_that("shapevec_reprs combines multiple shapes", {
  expect_equal(shapevec_reprs(c(2, 3), c(4, 5)), "(2,3), (4,5)")
  expect_equal(shapevec_reprs(c(1)), "(1)")
  expect_equal(shapevec_reprs(c(1, NA), c(2)), "(1,?), (2)")
})
