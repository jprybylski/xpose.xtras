# Master xpdb editing function

Generic function used to build dedicated editing functions

## Usage

``` r
edit_xpose_data(
  .fun,
  .fname,
  .data,
  ...,
  .problem,
  .source,
  .where,
  check_quos = FALSE
)
```

## Arguments

- .fun:

  An editing function to be applied to the data.

- .fname:

  The name of the editing function.

- .data:

  An xpose database object.

- ...:

  Name-value pairs of expressions. Use `NULL` to drop a variable.

- .problem:

  The problem from which the data will be modified

- .source:

  The source of the data in the xpdb. Can either be 'data' or an output
  file extension e.g. 'phi'.

- .where:

  A vector of element names to be edited in special (e.g.
  `.where = c('vpc_dat', 'aggr_obs')` with vpc).

- check_quos:

  Check that variables referenced exists. `TRUE` matches the behavior of
  \<[`xpose::edit_xpose_data`](https://uupharmacometrics.github.io/xpose/reference/edit_xpose_data.html)\>

  These arguments are automatically quoted and evaluated in the context
  of the data frame. They support unquoting and splicing. See the dplyr
  vignette("programming") for an introduction to these concepts.

## Value

The modified `xpose_data` object
