# Check an `xpose_set` object

Check an `xpose_set` object

## Usage

``` r
check_xpose_set(xpdb_s, .warn = TRUE)

check_xpose_set_item(xpdb_s_i, .example = xpdb_set)
```

## Arguments

- xpdb_s:

  \<[`xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)\>
  An xpose_set object

- .warn:

  \<`logical`\> Display a warning on failure.

- xpdb_s_i:

  \<[`xpose_set_item`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)\>
  An xpose_set_item object (element of an xpose_set)

- .example:

  \<`xpose_set`\> Basis of comparison for `xpose_s_i`

## Value

TRUE or error thrown

## Examples

``` r
check_xpose_set(xpdb_set)
#> [1] TRUE

check_xpose_set_item(xpdb_set$mod1)
#> [1] TRUE
```
