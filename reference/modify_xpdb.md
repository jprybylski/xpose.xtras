# Add, remove or rename variables in an xpdb

`mutate_x()` adds new variables and preserves existing ones.
[`select()`](https://dplyr.tidyverse.org/reference/select.html) keeps
only the listed variables;
[`rename()`](https://dplyr.tidyverse.org/reference/rename.html) keeps
all variables.

**Note:** this function uses
[`xpose.xtras::edit_xpose_data`](https://jprybylski.github.io/xpose.xtras/reference/edit_xpose_data.md),
but is otherwise the same as
\<[`xpose::mutate`](https://uupharmacometrics.github.io/xpose/reference/reexports.html)\>.

## Usage

``` r
mutate_x(.data, ..., .problem, .source, .where)

rename_x(.data, ..., .problem, .source, .where)
```

## Arguments

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

## Value

An updated `xpose` data object
