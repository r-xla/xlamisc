# DataType

`DataType` is the S3 class representing a tensor element type. It is an
enum: one singleton object per supported dtype, wrapping the canonical
dtype string. The members are the element types supported by the r-xla
stack:

- boolean: `bool`

- signed integers: `i8`, `i16`, `i32`, `i64`

- unsigned integers: `ui8`, `ui16`, `ui32`, `ui64`

- floats: `f16`, `bf16`, `f32`, `f64`

- complex: `c64`, `c128`

Construct a `DataType` with
[`as_dtype()`](https://r-xla.github.io/xlamisc/reference/as_dtype.md).
Inspect it with
[`dtype_width()`](https://r-xla.github.io/xlamisc/reference/dtype_width.md)
and the
[`is_dtype_float()`](https://r-xla.github.io/xlamisc/reference/is_dtype_float.md)
family of predicates.

## See also

[`as_dtype()`](https://r-xla.github.io/xlamisc/reference/as_dtype.md),
[`dtype_width()`](https://r-xla.github.io/xlamisc/reference/dtype_width.md),
[`is_dtype_float()`](https://r-xla.github.io/xlamisc/reference/is_dtype_float.md)
