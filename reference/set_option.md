# Set an `xpose` option

Set an `xpose` option

## Usage

``` r
set_option(xpdb, ...)
```

## Arguments

- xpdb:

  \<`xpose_data`[xpose::xpose_data](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Arguments in the form of `option = value`

## Value

`xp_xtras` object

## Examples

``` r
xpdb_x <- set_option(xpdb_x, quiet = TRUE)
```
