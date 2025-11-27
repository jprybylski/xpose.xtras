# Group/ungroup and summarize variables in an xpdb

`group_by_x()` takes an existing table and converts it into a grouped
table where operations are performed "by group". `ungroup()` removes
grouping. `summarize()` reduces multiple values down to a single value.

**Note:** this function uses
[`xpose.xtras::edit_xpose_data`](https://jprybylski.github.io/xpose.xtras/reference/edit_xpose_data.md),
but is otherwise the same as
\<[`xpose::group_by`](https://uupharmacometrics.github.io/xpose/reference/reexports.html)\>.

## Usage

``` r
group_by_x(.data, ..., .problem, .source, .where)

ungroup_x(.data, ..., .problem, .source, .where)
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

Group data in an `xpose` data object
