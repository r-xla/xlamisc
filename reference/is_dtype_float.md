# DataType Category Predicates

Check which category a
[`DataType`](https://r-xla.github.io/xlamisc/reference/DataType.md)
belongs to:

- `is_dtype_float()`: floating point, including `bf16`.

- `is_dtype_int()`: signed integers.

- `is_dtype_uint()`: unsigned integers.

- `is_dtype_bool()`: `bool`.

- `is_dtype_complex()`: `c64`, `c128`.

## Usage

``` r
is_dtype_float(x)

is_dtype_int(x)

is_dtype_uint(x)

is_dtype_bool(x)

is_dtype_complex(x)
```

## Arguments

- x:

  ([`DataType`](https://r-xla.github.io/xlamisc/reference/DataType.md))

## Value

`logical(1)`
