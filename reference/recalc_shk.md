# Recalculate eta shrinkage from individual estimates

Unlike
[`get_shk()`](https://jprybylski.github.io/xpose.xtras/reference/get_shk.md),
which parses the eta shrinkage NONMEM itself reported in the output
file, this recalculates shrinkage directly from the individual
(empirical Bayes) eta estimates found in the data, using the standard
\\100 \times (1 - SD(\eta)/\omega)\\ formula, where \\\omega\\ is the
standard deviation implied by the associated diagonal omega estimate.

## Usage

``` r
recalc_shk(
  xpdb,
  ...,
  .etastype = 1,
  .problem = NULL,
  .subprob = NULL,
  .method = NULL,
  drop_fixed = TRUE,
  quiet
)
```

## Arguments

- xpdb:

  \<`xpose_data`[xpose::xpose_data](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  or `xp_xtras` object

- ...:

  \<`tidyselect`\> Which eta column(s) to recalculate shrinkage for.
  Defaults to every `eta` column for `.problem`.

- .etastype:

  \<`numeric(1)`\> `1` (the default) excludes, for each eta, individuals
  whose estimate is a "true zero" (exactly `0`, as opposed to merely
  shrunk near it) from the calculation; `0` keeps them. See Details.

- .problem:

  \<`numeric`\> Problem number to use. Uses the xpose default if not
  provided.

- .subprob:

  \<`numeric`\> Subproblem number to use. Uses the xpose default if not
  provided.

- .method:

  \<`character`\> Method to use. Uses the xpose default if not provided.

- drop_fixed:

  \<`logical`\> Drop fixed etas (which have no meaningful shrinkage to
  recalculate), as in
  [`xpose::drop_fixed_cols`](https://uupharmacometrics.github.io/xpose/reference/drop_fixed_cols.html).

- quiet:

  \<`logical`\> Silence extra debugging output

## Value

A tibble with one row per eta, reporting the omega used, the number of
individuals excluded (if any), and the recalculated shrinkage (as a
percentage, to stay consistent with
[`get_shk()`](https://jprybylski.github.io/xpose.xtras/reference/get_shk.md)).

## Details

An eta is a "true zero" for an individual when NONMEM never had grounds
to move it away from its prior mean of `0`, eg an individual with no
observations contributing to the objective function. That is different
from an eta that is merely shrunk close to `0` through legitimate
estimation, and including "true zero" individuals in the shrinkage
calculation biases it, since they carry no information about the actual
empirical distribution of etas. `.etastype = 1` (the default) excludes
them from the calculation; `.etastype = 0` reproduces the traditional,
unadjusted calculation.

## Examples

``` r
recalc_shk(xpdb_x)
#> # A tibble: 3 × 8
#>   problem subprob method eta   omega     n n_excluded shrinkage
#>     <dbl>   <dbl> <chr>  <chr> <dbl> <int>      <int>     <dbl>
#> 1       1       1 foce   ETA1  0.27     74          0      52.9
#> 2       1       1 foce   ETA2  0.195    74          0      68.5
#> 3       1       1 foce   ETA3  1.38     74          0      10.2

# Just a subset of etas...
recalc_shk(xpdb_x, ETA1)
#> # A tibble: 1 × 8
#>   problem subprob method eta   omega     n n_excluded shrinkage
#>     <dbl>   <dbl> <chr>  <chr> <dbl> <int>      <int>     <dbl>
#> 1       1       1 foce   ETA1   0.27    74          0      52.9

# Including "true zero" etas in the calculation
recalc_shk(xpdb_x, .etastype = 0)
#> # A tibble: 3 × 8
#>   problem subprob method eta   omega     n n_excluded shrinkage
#>     <dbl>   <dbl> <chr>  <chr> <dbl> <int>      <int>     <dbl>
#> 1       1       1 foce   ETA1  0.27     74          0      52.9
#> 2       1       1 foce   ETA2  0.195    74          0      68.5
#> 3       1       1 foce   ETA3  1.38     74          0      10.2
```
