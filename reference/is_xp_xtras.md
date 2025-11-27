# Basic class checker for `xp_xtras`

Basic class checker for `xp_xtras`

## Usage

``` r
is_xp_xtras(x)
```

## Arguments

- x:

  Object to test

## Value

\<`logical`\> TRUE if `xp_xtras` object

## Examples

``` r
is_xp_xtras(xpose::xpdb_ex_pk)
#> [1] FALSE

is_xp_xtras(xpdb_x)
#> [1] TRUE
```
