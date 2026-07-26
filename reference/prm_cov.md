# Continuous/categorical covariate effect tables

Computes, for each covariate association declared with
[`add_cov_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_cov_association.md),
the covariate's effect on the associated parameter (as a ratio to the
parameter's typical value, `1` at the reference covariate value/level)
at a handful of representative evaluation points, with an uncertainty
interval propagated from the effect-size theta's standard error. This is
what
[`xplot_forest()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_forest.md)
plots; calling these directly is mostly useful for inspecting the
numbers before/without plotting.

`prm_contcov()` handles continuous covariates (`linear`, `power`,
`exponential`, `hockey`, or `custom` associations), evaluated at the
low/reference/high points. `prm_catcov()` handles categorical covariates
(`catshift` or `custom` associations), evaluated at every observed
level. `prm_cov()` combines both.

## Usage

``` r
prm_contcov(
  xpdb,
  ...,
  .problem = NULL,
  .subprob = NULL,
  .method = NULL,
  ci_method = c("simulation", "delta"),
  probs = c(0.05, 0.95),
  level = 0.95,
  nsim = 1000,
  keep_draws = FALSE,
  quiet
)

prm_catcov(
  xpdb,
  ...,
  .problem = NULL,
  .subprob = NULL,
  .method = NULL,
  ci_method = c("simulation", "delta"),
  level = 0.95,
  nsim = 1000,
  keep_draws = FALSE,
  quiet
)

prm_cov(
  xpdb,
  ...,
  .problem = NULL,
  .subprob = NULL,
  .method = NULL,
  ci_method = c("simulation", "delta"),
  probs = c(0.05, 0.95),
  level = 0.95,
  nsim = 1000,
  keep_draws = FALSE,
  quiet
)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> object with associations declared via
  [`add_cov_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_cov_association.md)

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Optional `param ~ covariate` selectors (bare, unquoted, same style as
  [`drop_cov_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_cov_association.md))
  to restrict which declared associations are computed. Defaults to all
  of them.

- .problem:

  \<`numeric`\> Problem number.

- .subprob:

  \<`numeric`\> Subprob number.

- .method:

  \<`numeric`\> Method.

- ci_method:

  \<`character`\> `"simulation"` (default) draws `nsim` samples of each
  theta from `N(theta_hat, se)` (mirrors
  [`mutate_prm()`](https://jprybylski.github.io/xpose.xtras/reference/mutate_prm.md)'s
  `.autose` approach) and propagates them through the (possibly
  nonlinear) effect_ratio function, taking the resulting sample
  quantiles as the interval; most accurate for strongly nonlinear forms
  (`power`, `exponential`, `hockey`). `"delta"` is a first-order
  analytic (numerical-gradient) log-scale approximation – cheap and
  deterministic, but less accurate the more nonlinear the association
  is. Both treat multiple thetas (eg `hockey`, multi-level `catshift`)
  as independent, ignoring any covariance between them.

- probs:

  \<`numeric(2)`\> For `prm_contcov()`: quantiles of the covariate's
  observed data used as the "low"/"high" evaluation points.

- level:

  \<`numeric`\> Confidence level for the effect interval.

- nsim:

  \<`numeric`\> Number of simulation draws, when
  `ci_method = "simulation"`.

- keep_draws:

  \<`logical`\> If `TRUE` (requires `ci_method = "simulation"`), attach
  a `draws` list-column: the raw `nsim` simulated effect-ratio draws
  behind each row's CI. Mainly intended for a forest-plot violin/density
  layer; most users won't need this.

- quiet:

  Silence extra output.

## Value

A `prm_cov_tbl` tibble with one row per (parameter, covariate,
evaluation point): `param`, `covariate`, `covtype`, `level`
(`"low"`/`"ref"`/`"high"` for continuous, the raw category value for
categorical), `value` (the covariate value/level backing that row),
`is_ref` (`TRUE` for the reference row/level – always
`effect`/`ci_low`/`ci_high` `== 1`, by construction), `effect`,
`ci_low`, `ci_high`, `ci_method`.

## See also

[`add_cov_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_cov_association.md),
[`xplot_forest()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_forest.md)

## Examples

``` r

xpdb_x %>%
  add_cov_association(TVCL ~ power(CLCR, THETA7, ref = 64)) %>%
  prm_contcov()
#> # A tibble: 3 × 10
#>   param covariate covtype level value is_ref effect ci_low ci_high ci_method 
#> * <chr> <chr>     <chr>   <chr> <chr> <lgl>   <dbl>  <dbl>   <dbl> <chr>     
#> 1 TVCL  CLCR      cont    low   40    FALSE   0.997  0.995   0.998 simulation
#> 2 TVCL  CLCR      cont    ref   64    TRUE    1      1       1     simulation
#> 3 TVCL  CLCR      cont    high  102   FALSE   1.00   1.00    1.00  simulation
#> # `effect` is a ratio to the parameter's typical value (1 at the reference
#> covariate value/level); CI via simulation.

xpdb_x %>%
  add_cov_association(TVCL ~ catshift(SEX, THETA4, ref = 1)) %>%
  prm_catcov()
#> # A tibble: 2 × 10
#>   param covariate covtype level value is_ref effect ci_low ci_high ci_method 
#> * <chr> <chr>     <chr>   <chr> <chr> <lgl>   <dbl>  <dbl>   <dbl> <chr>     
#> 1 TVCL  SEX       cat     1     1     TRUE     1      1       1    simulation
#> 2 TVCL  SEX       cat     2     2     FALSE    1.21   1.18    1.24 simulation
#> # `effect` is a ratio to the parameter's typical value (1 at the reference
#> covariate value/level); CI via simulation.

xpdb_x %>%
  add_cov_association(
    TVCL ~ power(CLCR, THETA7, ref = 64),
    TVCL ~ catshift(SEX, THETA4, ref = 1)
  ) %>%
  prm_cov()
#> # A tibble: 5 × 10
#>   param covariate covtype level value is_ref effect ci_low ci_high ci_method 
#>   <chr> <chr>     <chr>   <chr> <chr> <lgl>   <dbl>  <dbl>   <dbl> <chr>     
#> 1 TVCL  CLCR      cont    low   40    FALSE   0.997  0.995   0.998 simulation
#> 2 TVCL  CLCR      cont    ref   64    TRUE    1      1       1     simulation
#> 3 TVCL  CLCR      cont    high  102   FALSE   1.00   1.00    1.00  simulation
#> 4 TVCL  SEX       cat     1     1     TRUE    1      1       1     simulation
#> 5 TVCL  SEX       cat     2     2     FALSE   1.21   1.18    1.24  simulation
#> # `effect` is a ratio to the parameter's typical value (1 at the reference
#> covariate value/level); CI via simulation.

# Restrict to one association, and use the analytic delta-method CI
xpdb_x %>%
  add_cov_association(
    TVCL ~ power(CLCR, THETA7, ref = 64),
    TVCL ~ catshift(SEX, THETA4, ref = 1)
  ) %>%
  prm_cov(TVCL ~ CLCR, ci_method = "delta")
#> # A tibble: 3 × 10
#>   param covariate covtype level value is_ref effect ci_low ci_high ci_method
#>   <chr> <chr>     <chr>   <chr> <chr> <lgl>   <dbl>  <dbl>   <dbl> <chr>    
#> 1 TVCL  CLCR      cont    low   40    FALSE   0.997  0.995   0.998 delta    
#> 2 TVCL  CLCR      cont    ref   64    TRUE    1      1       1     delta    
#> 3 TVCL  CLCR      cont    high  102   FALSE   1.00   1.00    1.00  delta    
#> # `effect` is a ratio to the parameter's typical value (1 at the reference
#> covariate value/level); CI via delta.
```
