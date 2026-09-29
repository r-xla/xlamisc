# Width of a DataType

The width in bits of one element of the given
[`DataType`](https://r-xla.github.io/xlamisc/reference/DataType.md). For
`bool` this is `1`; for complex types it is the total width of the
element (`c64` -\> `64`).

## Usage

``` r
dtype_width(x)
```

## Arguments

- x:

  ([`DataType`](https://r-xla.github.io/xlamisc/reference/DataType.md))

## Value

`integer(1)`
