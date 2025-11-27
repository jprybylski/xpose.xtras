# Convert an object to an `xpose_data` and `xp_xtras` object

This function masks the default in `xpose` package, adding the
`xp_xtras` class to default `xpose_data` objects.

## Usage

``` r
as_xpdb_x(x)

as_xp_xtras(x)

check_xpdb_x(x, .warn = TRUE)

check_xp_xtras(...)
```

## Arguments

- x:

  Suspected `xp_xtras` object

- .warn:

  \<`logical`\> Whether to warn if `xpose_data` but not `xp_xtras`

- ...:

  Forwarded

## Value

\<[`xpose_data`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
and \<`xp_xtras`\> object

## Examples

``` r
xp_x <- as_xpdb_x(xpose::xpdb_ex_pk)
check_xpdb_x(xp_x)
#> [1] TRUE
```
