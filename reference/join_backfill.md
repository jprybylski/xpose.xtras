# Left join, backfilling shared columns instead of duplicating them

As
\<[`dplyr::left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)\>,
but any column present in both `x` and `y` (besides the join keys) is
coalesced instead of suffixed: values already present in `x` are kept,
and only missing (`NA`) values are filled in from `y`.

## Usage

``` r
join_backfill(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
)
```

## Arguments

- x:

  An `xpose_data` or `xp_xtras` object.

- y:

  A data frame (or another object coercible to one) to join in.

- by:

  Join specification, as in
  \<[`dplyr::left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)\>.
  If `NULL`, a natural join is performed using variables common to `x`
  and `y`.

- copy:

  If `x` and `y` are not from the same source and `copy = TRUE`, `y` is
  copied to bring it into the same source as `x`. See
  \<[`dplyr::left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)\>.

- suffix:

  Suffixes used internally to disambiguate a column shared by `x` and
  `y` before it is backfilled into a single column; not visible in the
  result.

- ...:

  Other parameters passed onto
  \<[`dplyr::left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)\>.

- keep:

  Passed to
  \<[`dplyr::left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)\>.
  Note that duplicate join key columns (`keep = TRUE`) are backfilled
  together like any other shared column, rather than kept separate.

## Value

A data frame
