dtype_names <- c(
  "bool",
  "i8", "i16", "i32", "i64",
  "ui8", "ui16", "ui32", "ui64",
  "f16", "bf16", "f32", "f64",
  "c64", "c128"
)

test_that("as_dtype covers every member and round-trips its name", {
  for (name in dtype_names) {
    dt <- as_dtype(name)
    expect_s3_class(dt, "DataType")
    expect_identical(as.character(dt), name)
    expect_true(is_dtype(dt))
  }
})

test_that("as_dtype returns singletons", {
  expect_identical(as_dtype("f32"), as_dtype("f32"))
  expect_identical(as_dtype(as_dtype("i8")), as_dtype("i8"))
})

test_that("bool aliases", {
  expect_identical(as_dtype("pred"), as_dtype("bool"))
  expect_identical(as_dtype("i1"), as_dtype("bool"))
})

test_that("as_dtype rejects unknown names and non-strings", {
  expect_error(as_dtype("f128"), "Unsupported dtype")
  expect_error(as_dtype("token"), "Unsupported dtype")
  expect_error(as_dtype("f8e5m2"), "Unsupported dtype")
  expect_error(as_dtype("f4e2m1fn"), "Unsupported dtype")
  expect_error(as_dtype("i2"), "Unsupported dtype")
  expect_error(as_dtype("ui4"), "Unsupported dtype")
  expect_error(as_dtype(32L), "Cannot convert")
  expect_error(as_dtype(c("f32", "f64")))
})

test_that("dtype_width", {
  expect_identical(dtype_width(as_dtype("bool")), 1L)
  expect_identical(dtype_width(as_dtype("i8")), 8L)
  expect_identical(dtype_width(as_dtype("ui64")), 64L)
  expect_identical(dtype_width(as_dtype("f16")), 16L)
  expect_identical(dtype_width(as_dtype("bf16")), 16L)
  expect_identical(dtype_width(as_dtype("c64")), 64L)
  expect_identical(dtype_width(as_dtype("c128")), 128L)
  expect_error(dtype_width("f32"), "DataType")
})

test_that("dtype_category", {
  expect_identical(dtype_category(as_dtype("bool")), "bool")
  expect_identical(dtype_category(as_dtype("i32")), "int")
  expect_identical(dtype_category(as_dtype("ui8")), "uint")
  expect_identical(dtype_category(as_dtype("bf16")), "float")
  expect_identical(dtype_category(as_dtype("c64")), "complex")
  expect_error(dtype_category("f32"), "DataType")
})

test_that("category predicates", {
  expect_true(is_dtype_bool(as_dtype("bool")))
  expect_true(is_dtype_int(as_dtype("i32")))
  expect_true(is_dtype_uint(as_dtype("ui8")))
  expect_true(is_dtype_float(as_dtype("f64")))
  expect_true(is_dtype_float(as_dtype("bf16")))
  expect_true(is_dtype_float(as_dtype("f16")))
  expect_true(is_dtype_complex(as_dtype("c128")))

  expect_false(is_dtype_int(as_dtype("ui32")))
  expect_false(is_dtype_uint(as_dtype("i32")))
  expect_false(is_dtype_float(as_dtype("i32")))
  expect_false(is_dtype_bool(as_dtype("i8")))
  expect_false(is_dtype_complex(as_dtype("f64")))

  expect_error(is_dtype_float("f32"), "DataType")
})

test_that("equality and inequality", {
  expect_true(as_dtype("f32") == as_dtype("f32"))
  expect_false(as_dtype("f32") == as_dtype("f64"))
  expect_true(as_dtype("f32") != as_dtype("f64"))
  expect_false(as_dtype("f16") == as_dtype("bf16"))
})

test_that("equality with character", {
  expect_true(as_dtype("f32") == "f32")
  expect_true("f32" == as_dtype("f32"))
  expect_false(as_dtype("f32") != "f32")
  expect_false(as_dtype("f32") == "f64")
  expect_true(as_dtype("bool") == "i1")
  expect_true(as_dtype("bool") == "pred")
  expect_true("i1" == as_dtype("bool"))
  expect_false(as_dtype("bool") == "f32")
})

test_that("print and cli_format", {
  expect_output(print(as_dtype("f32")), "<f32>", fixed = TRUE)
  expect_output(print(as_dtype("bool")), "<bool>", fixed = TRUE)
  expect_identical(cli::cli_format(as_dtype("bf16")), "bf16")
})
