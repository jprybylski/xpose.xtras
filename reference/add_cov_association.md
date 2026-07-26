# Describe parameter/covariate associations

The relationship between a structural parameter and a covariate can be
described, so that the covariate's effect on that parameter – and the
uncertainty of that effect – can later be visualized with
[`xplot_forest()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_forest.md)
(via
[`prm_cov()`](https://jprybylski.github.io/xpose.xtras/reference/prm_cov.md)/[`prm_contcov()`](https://jprybylski.github.io/xpose.xtras/reference/prm_cov.md)/[`prm_catcov()`](https://jprybylski.github.io/xpose.xtras/reference/prm_cov.md)).

This is deliberately parallel to
[`add_prm_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_prm_association.md):
the same formula-based declaration style, the same two-stage
check-then-process validation, and the same "redeclare to replace"
upsert behavior. It is a separate mechanism (own storage, own getters)
because a covariate association needs more shape than an omega
association – a covariate column, a *required* reference value, and (for
categorical covariates or `custom()`) more than one theta – and it
produces a *range* of effect sizes rather than a single scalar CV.

## Usage

``` r
add_cov_association(xpdb, ..., .problem, .subprob, .method, quiet)

drop_cov_association(xpdb, ..., .problem, .subprob, .method, quiet)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  One or more formulas that define associations between a parameter and
  a covariate. One list of formulas can also be used, but a warning is
  generated.

  For `drop_cov_association`, these should be formulas of the form
  `param ~ covariate` (both bare, unquoted selectors; `covariate` must
  be the literal covariate column name as declared).

- .problem:

  \<`numeric`\> Problem number to apply this relationship.

- .subprob:

  \<`numeric`\> Subprob number to apply this relationship.

- .method:

  \<`numeric`\> Method to apply this relationship.

- quiet:

  Silence extra output.

## Value

An updated `xp_xtras` object

## Details

Format for associations is:

`LHS ~ fun(COVARIATE, THETA..., ref = ..., ...)`

- LHS: Selector for a fixed-effect (theta) parameter, exactly as in
  [`add_prm_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_prm_association.md)
  (`the{m}`, `{name}` or `{label}`, unquoted). Multiple parameters can
  share one association with `+` (eg, a covariate that affects both `CL`
  and `Q` through the same theta).

- RHS `COVARIATE`: The first (positional) argument. The bare, unquoted
  name of a `contcov`/`catcov` column (see
  [`xpose::xp_var()`](https://uupharmacometrics.github.io/xpose/reference/xp_var.html))
  – *not* a fixed-effect or omega selector.

- RHS `THETA...`: One or more further positional arguments, selecting
  the fixed-effect parameter(s) that carry the covariate effect
  magnitude (same selector rules as LHS – `the{m}`/`{name}`/ `{label}`,
  unquoted). How many are expected depends on `fun`; see the built-in
  list below.

- RHS `ref`: **Required, named, no default**, for every association
  regardless of `fun`. This is the covariate value (for continuous
  covariates) or raw level (for categorical covariates) that the effect
  is normalized against – the point at which the reported effect ratio
  is exactly `1`. There is no way to safely infer this from the `xpdb`
  alone (NONMEM control streams commonly normalize a covariate against a
  hardcoded constant baked into the code, which is invisible to
  `xpose`), so it must always be stated explicitly, the same way
  [`add_prm_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_prm_association.md)'s
  `nmboxcox` requires an explicit `lambda`.

All built-ins express the covariate's effect as a multiplicative
`effect_ratio` on the parameter's typical value, and are constructed so
that `effect_ratio == 1` whenever the covariate equals `ref`,
*regardless of the theta value*. Available built-ins:

- `linear(COV, THETA, ref=)`: \\1 + \theta (COV - ref)\\

- `power(COV, THETA, ref=)`: \\(COV / ref)^\theta\\ (allometric)

- `exponential(COV, THETA, ref=)`: \\e^{\theta (COV - ref)}\\

- `additive(COV, THETA, ref=)`: \\(\theta + COV) / (\theta + ref)\\. An
  uncommon but simple form where the covariate is added directly (with
  an implicit coefficient of `1`, unlike `linear`'s explicit slope) to
  an intercept-like `THETA`, eg `CL = THETA(n) + WT` in the underlying
  model.

- `hockey(COV, THETA_LO, THETA_HI, ref=, brk=ref)`: PsN's "hockey-stick"
  two-slope piecewise-linear model – \\1 + \theta\_{lo} (COV - ref)\\
  when `COV <= brk`, \\1 + \theta\_{hi} (COV - ref)\\ when `COV > brk`.
  `brk` (the breakpoint) defaults to `ref` (PsN's usual default:
  breakpoint = normalization reference), but can be given separately, eg
  for a covariate normalized to its observed median while the clinically
  meaningful cutpoint is a round number (`ref = 90, brk = 60` for an
  eGFR-like covariate).

- `catshift(COV, THETA..., ref=)`: One theta per non-reference raw level
  of a categorical covariate, `effect_ratio = 1 + THETA_i` for level `i`
  (`1` at `ref`). Assumes the typical NONMEM pattern of one theta per
  non-reference category (eg `IF (RACE.EQ.2) CLCOV = THETA(9)`). Thetas
  are matched to non-reference levels in ascending raw-value order – if
  that order is ambiguous or wrong for a given model, use `custom()`
  instead.

For anything else, `custom(COV, THETA..., ref=, fun=)` is the escape
hatch: `fun` is a function of `(cov, ref, theta)` (`theta` is always a
numeric vector, even when only one `THETA` selector is given) returning
the effect ratio. Because `custom()` can't be verified by construction
the way the built-ins can, `add_cov_association()` validates it at
declaration time by evaluating `fun(ref, ref, theta)` for a few probe
values of `theta` (not the currently-fitted value, which could
coincidentally pass while `fun` is still wrong for other theta values)
and requires each to equal `1`; if it doesn't, the error states the
required invariant and shows what `fun` actually returned, rather than
failing silently or only much later during plotting.

### A note on parameter/theta scale

The computed effect ratio never touches the LHS parameter's own fitted
value – only the covariate-effect theta(s), the covariate value, and
`ref` – so it does not matter whether the LHS was itself parameterized
on a log, logit, or identity scale in the control stream
([`get_prm()`](https://jprybylski.github.io/xpose.xtras/reference/get_prm.md)'s
`transform` argument only affects diagonal OMEGA/SIGMA reporting
(variance -\> SD, covariance -\> correlation); THETA values are always
the raw fitted estimate regardless of `transform`, so there is nothing
to reconcile there either).

What *is* assumed is that the chosen association – a builtin or
`custom()` – is an exact match for the functional form actually used in
the model code, including any scale factor baked into that code (eg a
covariate effect entered as `THETA(n)*(COV-ref)/100`). There is no way
to verify this from the `xpdb` alone; if a builtin's literal formula
(see above) doesn't match the real model, the result will be a
numerically valid but silently wrong effect ratio, not an error – the
same caveat
[`add_prm_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_prm_association.md)
already carries for CV% calculation.

If the covariate-effect theta itself is not already on the scale a
builtin expects (eg it was fitted on a logit or other transformed scale,
or needs some other rescaling to match one of the literal formulas
above), transform it back with
[`mutate_prm()`](https://jprybylski.github.io/xpose.xtras/reference/mutate_prm.md)
*before* declaring the association – the same recommended workflow as
[`add_prm_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_prm_association.md)'s
own untransformed-theta requirement.

## See also

[`add_prm_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_prm_association.md),
[`prm_cov()`](https://jprybylski.github.io/xpose.xtras/reference/prm_cov.md),
[`mutate_prm()`](https://jprybylski.github.io/xpose.xtras/reference/mutate_prm.md)

## Examples

``` r

# xpdb_x's THETA7 ("CRCL on CL") is a genuine covariate effect already
# in the model, so this is a faithful (if allometric-flavored, for
# illustration) description of it:
xpdb_x %>%
  add_cov_association(TVCL ~ power(CLCR, THETA7, ref = 64)) %>%
  prm_cov()
#> # A tibble: 3 × 10
#>   param covariate covtype level value is_ref effect ci_low ci_high ci_method 
#>   <chr> <chr>     <chr>   <chr> <chr> <lgl>   <dbl>  <dbl>   <dbl> <chr>     
#> 1 TVCL  CLCR      cont    low   40    FALSE   0.997  0.995   0.998 simulation
#> 2 TVCL  CLCR      cont    ref   64    TRUE    1      1       1     simulation
#> 3 TVCL  CLCR      cont    high  102   FALSE   1.00   1.00    1.00  simulation
#> # `effect` is a ratio to the parameter's typical value (1 at the reference
#> covariate value/level); CI via simulation.

# hockey-stick (PsN-style): different slope above/below the reference
xpdb_x %>%
  add_cov_association(TVCL ~ hockey(CLCR, THETA7, THETA4, ref = 64)) %>%
  prm_cov()
#> # A tibble: 3 × 10
#>   param covariate covtype level value is_ref effect ci_low ci_high ci_method 
#>   <chr> <chr>     <chr>   <chr> <chr> <lgl>   <dbl>  <dbl>   <dbl> <chr>     
#> 1 TVCL  CLCR      cont    low   40    FALSE   0.828  0.747   0.913 simulation
#> 2 TVCL  CLCR      cont    ref   64    TRUE    1      1       1     simulation
#> 3 TVCL  CLCR      cont    high  102   FALSE   8.90   7.68   10.1   simulation
#> # `effect` is a ratio to the parameter's typical value (1 at the reference
#> covariate value/level); CI via simulation.

# Categorical: one theta per non-reference level. SEX has 2 levels
# (1, 2), so catshift needs exactly one theta for the non-reference
# level; THETA4 is reused here purely for illustration.
xpdb_x %>%
  add_cov_association(TVCL ~ catshift(SEX, THETA4, ref = 1)) %>%
  prm_cov()
#> # A tibble: 2 × 10
#>   param covariate covtype level value is_ref effect ci_low ci_high ci_method 
#>   <chr> <chr>     <chr>   <chr> <chr> <lgl>   <dbl>  <dbl>   <dbl> <chr>     
#> 1 TVCL  SEX       cat     1     1     TRUE     1      1       1    simulation
#> 2 TVCL  SEX       cat     2     2     FALSE    1.21   1.18    1.24 simulation
#> # `effect` is a ratio to the parameter's typical value (1 at the reference
#> covariate value/level); CI via simulation.

# custom(): fun(cov, ref, theta) must equal 1 when cov == ref
xpdb_x %>%
  add_cov_association(
    TVCL ~ custom(CLCR, THETA7, ref = 64,
                fun = function(cov, ref, theta) (cov/ref)^theta)
  ) %>%
  prm_cov()
#> # A tibble: 3 × 10
#>   param covariate covtype level value is_ref effect ci_low ci_high ci_method 
#>   <chr> <chr>     <chr>   <chr> <chr> <lgl>   <dbl>  <dbl>   <dbl> <chr>     
#> 1 TVCL  CLCR      cont    low   40    FALSE   0.997  0.995   0.998 simulation
#> 2 TVCL  CLCR      cont    ref   64    TRUE    1      1       1     simulation
#> 3 TVCL  CLCR      cont    high  102   FALSE   1.00   1.00    1.00  simulation
#> # `effect` is a ratio to the parameter's typical value (1 at the reference
#> covariate value/level); CI via simulation.

# Dropping an association is easy
bad_assoc <- xpdb_x %>%
  add_cov_association(TVCL ~ power(CLCR, THETA7, ref = 64))
bad_assoc %>%
  drop_cov_association(TVCL ~ CLCR) %>%
  prm_cov()
#> # A tibble: 0 × 10
#> # ℹ 10 variables: param <chr>, covariate <chr>, covtype <chr>, level <chr>,
#> #   value <chr>, is_ref <lgl>, effect <dbl>, ci_low <dbl>, ci_high <dbl>,
#> #   ci_method <chr>
```
