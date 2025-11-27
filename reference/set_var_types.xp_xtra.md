# Set variable types

**\[experimental\]**

\<[`set_var_types`](https://uupharmacometrics.github.io/xpose/reference/set_vars.html)\>
wrapper that accepts tidyselect syntax. Character vector-based selection
still works.

`set_var_types_x` accepts `xpose_data` or `xp_xtras` objects.

`set_var_types` without `_x` is defined with S3 methods. To maintain
`xpose` expectations, the default method is
\<[`set_var_types`](https://uupharmacometrics.github.io/xpose/reference/set_vars.html)\>,
but if an `xp_xtras` object is used, the method uses `set_var_types_x`.

## Usage

``` r
# S3 method for class 'xp_xtras'
set_var_types(xpdb, .problem = NULL, ..., auto_factor = TRUE, quiet)
```

## Arguments

- xpdb:

  An `xpose_data` object.

- .problem:

  The problem number to which the edits will be applied.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Passed to
  \<[`set_var_types`](https://uupharmacometrics.github.io/xpose/reference/set_vars.html)\>
  after processing.

- auto_factor:

  If `TRUE` new columns assigned to the type 'catcov' will be converted
  to factor.

- quiet:

  Logical, if `FALSE` messages are printed to the console.

## Value

An xpose_data object

## Examples

``` r
data("xpdb_ex_pk", package = "xpose")

# Change variable type
xpdb_2 <- set_var_types_x(
  xpdb_ex_pk, .problem = 1,
  idv = TAD,
  catcov = starts_with("MED"),
  contcov = c(CLCR,AGE)
  )
```
