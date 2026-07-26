# Backfill missing variables via a left join

**\[experimental\]**

\<[`dplyr::left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)\>
wrapper for `xpose_data` (and, by inheritance, `xp_xtras`) objects.
Unlike a plain `left_join()`, a column present in both `x` and `y`
(other than the join keys) is not duplicated with `.x`/`.y` suffixes:
missing (`NA`) values already in `x` are backfilled from the matching
value in `y`, while non-missing values already in `x` are left
untouched. This makes it straightforward to backfill a variable (or set
of variables) that is only partially recorded, from a second data source
keyed on the same join variable(s) (e.g. `ID`).

`left_join_x()` accepts `xpose_data`/`xp_xtras` objects directly, with
an additional `.problem` argument restricting which problem(s) the join
is applied to.

`left_join()` without `_x` is defined as an S3 method on `xpose_data`,
so that the usual
\<[`dplyr::left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)\>
generic dispatches here automatically (`xp_xtras` objects are handled
the same way, via class inheritance).

## Usage

``` r
left_join_x(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL,
  .problem = NULL
)

# S3 method for class 'xpose_data'
left_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL,
  .problem = NULL
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

- .problem:

  The problem number(s) to which the join will be applied. Uses all
  problems if `NULL`.

## Value

An updated `xpose_data`/`xp_xtras` object.

## Examples

``` r
# Some subjects are missing an APGR score in the base dataset
xpdb_missing <- pheno_base %>%
  mutate_x(APGR = dplyr::if_else(ID %in% c("1", "2"), NA, APGR))

# A separate table with the (complete) values, keyed on ID
apgr_lookup <- xpose::get_data(pheno_base, quiet = TRUE) %>%
  dplyr::distinct(ID, APGR)

# Existing APGR values are kept; only the missing ones are filled in
left_join_x(xpdb_missing, apgr_lookup, by = "ID")
#> 
#> ── ~ xp_xtras object 
#> Model description: na
#> run6.lst overview: 
#>  - Software: nonmem 7.5.0 
#>  - Attached files (memory usage 502 Kb): 
#>    + obs tabs: $prob no.1 (modified): run6tab 
#>    + sim tabs: <none> 
#>    + output files: run6.cor, run6.cov, run6.ext, run6.grd, run6.phi, run6.shk 
#>    + special: <none> 
#>  - gg_theme: theme_readable 
#>  - xp_theme: xp_xtra_theme new_x$xp_theme 
#>  - Options: dir = ~/Step 3/Step 5/Step 6, quiet = TRUE, manual_import = NULL, cvtype = exact
```
