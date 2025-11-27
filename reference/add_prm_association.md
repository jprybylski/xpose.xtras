# Describe parameter associations

The relationship between structural parameters and omega parameters can
be described. This is useful if it deviates from the typical log-normal.

Default transformations are those that are built into `pmxcv`, but see
examples for how associations can be described for other relationships.

***Note:*** When these associations are used to calculate CV%, it is
assumed that the value for the theta parameter is *untransformed*. So,
if a parameter is fitted in the logit scale, the value should be
transformed back to normal scale with
[`mutate_prm()`](https://jprybylski.github.io/xpose.xtras/reference/mutate_prm.md)
(eg, `mutate_prm(the~plogis`) before declaring `the~logit(ome)`.

## Usage

``` r
add_prm_association(xpdb, ..., .problem, .subprob, .method, quiet)

drop_prm_association(xpdb, ..., .problem, .subprob, .method, quiet)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> object

- ...:

  ...
  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  One or more formulas that define associations between parameters. One
  list of formulas can also be used, but a warning is generated.

  For `drop_prm_association`, these dots should be selectors for which
  associations will be dropped (`the2, the3,...`). Fixed effect
  selectors only will work.

- .problem:

  \<`numeric`\> Problem number to apply this relationship.

- .subprob:

  \<`numeric`\> Problem number to apply this relationship.

- .method:

  \<`numeric`\> Problem number to apply this relationship.

- quiet:

  Silence extra output.

## Value

An updated `xp_xtras` object

## Details

At time of writing, the built-in distributions for `pmxcv` are below.
Those marked with an asterisk require a fixed effect parameter to
calculate CV.

- `log` typical log-normal. Optional `exact` parameter (if `TRUE`,
  default, will not calculate with integration); this is unrelated to
  the `cvtype` option. **Note**, if `cvtype` option is set to `"sqrt"`,
  log-normal `get_prm` CVs will use the square root, not any integration
  or analytical estimate, regardless of how this association is
  specified.

- `logexp`\* modified log-normal `log(1+X)`

- `logit`\* logit-normal

- `arcsin`\* arcsine-transform

- `nmboxcox`\* Box-Cox transform as typically implemented in
  pharmacometrics. Requires a `lambda` parameter.

To pass a custom parameter, use `custom` transform, and pass `pdist` and
`qdist` to that transform. See Examples.

Reminder about `qdist` and `pdist`: Consider that `qlogis` transforms a
proportion to a continuous, unbounded number; it is the `logit`
transform. The `plogis` function converts a continuous, unbounded number
to a proportion; it is the *inverse* `logit` transform. Other R `stats`
functions work similarly, and as such functions used as `qdist` and
`pdist` values are expected to act similarly.

Note that the functions used in describing associations are not real
functions, it is just the syntax for this application. Based on
examples, be mindful of where positional arguments would acceptable and
where named arguments are required. Care has been given to provide a
modest amount of flexibility with informative errors for fragile points,
but not every error can be anticipated. If this function or downstream
results from it seem wrong, the association syntax should be
scrutinized. These "functions" are not processed like in
[`mutate_prm`](https://jprybylski.github.io/xpose.xtras/reference/mutate_prm.md),
so (eg) `the2` will not be substituted for the value of `the2`; if
`lambda` is a fitted value (like `the2`), in that edge case the value of
`the2` should be written explicitly in the association formula, and if
any `mutate_prm` changes `the2` then users should be mindful of the new
association needed. This may be updated in the future.

Format for associations is: `LHS~fun(OMEGA, args...)`

- LHS: Selector for a fixed effect parameter. Can be `the{m}` (eg,
  the1), `{name}` (eg, THETA1) or `{label}` (eg, TVCL). These should
  *not* be quoted. Multiple associations can be defined at once with
  `+`. Cannot be empty.

- RHS: Should be a simple call to only one function, which should be
  custom or one of the built-in distributions or `custom(...)`. A lot of
  things can look like simple calls, so may not break immediately; keep
  to the described format and everything should be fine.

- RHS OMEGA: Selector for omega variable. Similar rules to the fixed
  effect selector. Can be `ome{m}`, `{name}` or `{label}`, limited to
  diagonal elements. Should *not* be quoted. `OMEGA` is not a named
  argument (`OMEGA={selector}` should **not** be considered valid);
  whatever is used as the first argument to the "function" will be
  considered an OMEGA selector. **NOTE**, if selecting an OMEGA
  parameter by name (eg, `OMEGA(2,2)`), backticks (eg
  `` `OMEGA(2,2)` ``) must be used or else the selection will throw an
  error.

- RHS args: Applies when the distribution has extra arguments. If these
  are limited to 1, can be passed by position (eg, `lambda` for
  `nmboxcox` and `exact` for `log`). For `custom()`, `qdist`, `pdist`
  and any arguments needed to pass to them should be named.

For the `nmboxcox` transformation, a lambda value (especially negative
ones) may not work well with the integration-based CV estimation. This
may occur even if the lambda is fitted and stable in that fitting, but
it cannot be predicted which ones will be affected. This note is
intended to forewarn that this might happen.

## References

Prybylski, J.P. Reporting Coefficient of Variation for Logit, Box-Cox
and Other Non-log-normal Parameters. Clin Pharmacokinet 63, 133-135
(2024).
[doi:10.1007/s40262-023-01343-2](https://doi.org/10.1007/s40262-023-01343-2)

## See also

[`dist.intcv`](https://rdrr.io/pkg/pmxcv/man/dist.intcv.html)

## Examples

``` r
pheno_base %>%
   add_prm_association(the1~log(IIVCL),V~log(IIVV)) %>%
   get_prm() # get_prm is the only way to see the effect of associations
#> Returning parameter estimates from $prob no.1, subprob no.1, method foce
#> # A tibble: 8 × 12
#>   type  name      label  value       se     rse fixed diagonal     m     n    cv
#> * <chr> <chr>     <chr> <num:>  <num:3> <num:3> <lgl> <lgl>    <int> <int> <num>
#> 1 the   THETA1    "CL"  0.0068  5.12e-4  0.0753 FALSE NA           1    NA  NA  
#> 2 the   THETA2    "V"   1.4     7.56e-2  0.0538 FALSE NA           2    NA  NA  
#> 3 the   THETA3    "RUV… 2.86    4.46e-1  0.156  FALSE NA           3    NA  NA  
#> 4 the   THETA4    "RUV… 0      NA       NA      TRUE  NA           4    NA  NA  
#> 5 ome   OMEGA(1,… "IIV… 0.489   7.64e-2  0.156  FALSE TRUE         1     1  52.0
#> 6 ome   OMEGA(2,… ""    0.998   1.24e-1  0.125  FALSE FALSE        2     1  NA  
#> 7 ome   OMEGA(2,… "IIV… 0.393   5.32e-2  0.135  FALSE TRUE         2     2  40.9
#> 8 sig   SIGMA(1,… ""    1      NA       NA      TRUE  TRUE         1     1  NA  
#> # ℹ 1 more variable: shk <num:3>
#> # Parameter table includes the following associations: CL~log(IIVCL) and
#> V~log(IIVV)

# These values are not fitted as logit-normal, but
# just to illustrate:
pheno_final %>%
   add_prm_association(the1~logit(IIVCL),Vpkg~logit(IIVV)) %>%
   get_prm()
#> Returning parameter estimates from $prob no.1, subprob no.1, method foce
#> # A tibble: 7 × 12
#>   type  name       label     value        se      rse fixed diagonal     m     n
#> * <chr> <chr>      <chr>   <num:4>   <num:4>  <num:4> <lgl> <lgl>    <int> <int>
#> 1 the   THETA1     "CLpk… 0.004813  2.365e-4  0.04914 FALSE NA           1    NA
#> 2 the   THETA2     "Vpkg" 0.9964    2.642e-2  0.02652 FALSE NA           2    NA
#> 3 the   THETA3     "RUVA… 2.784     2.513e-1  0.09027 FALSE NA           3    NA
#> 4 ome   OMEGA(1,1) "IIVC… 0.2009    5.108e-2  0.2543  FALSE TRUE         1     1
#> 5 ome   OMEGA(2,1) ""     0.7236    2.654e-1  0.3668  FALSE FALSE        2     1
#> 6 ome   OMEGA(2,2) "IIVV" 0.1576    2.614e-2  0.1659  FALSE TRUE         2     2
#> 7 sig   SIGMA(1,1) ""     1        NA        NA       TRUE  TRUE         1     1
#> # ℹ 2 more variables: cv <num:4>, shk <num:4>
#> # Parameter table includes the following associations: CLpkg~logit(IIVCL) and
#> Vpkg~logit(IIVV)

# ... same for Box-Cox
pheno_base %>%
   add_prm_association(V~nmboxcox(IIVV, lambda=0.5)) %>%
   # Naming the argument is optional
   add_prm_association(CL~nmboxcox(IIVCL, -0.1)) %>%
   get_prm()
#> Returning parameter estimates from $prob no.1, subprob no.1, method foce
#> # A tibble: 8 × 12
#>   type  name      label  value       se     rse fixed diagonal     m     n    cv
#> * <chr> <chr>     <chr> <num:>  <num:3> <num:3> <lgl> <lgl>    <int> <int> <num>
#> 1 the   THETA1    "CL"  0.0068  5.12e-4  0.0753 FALSE NA           1    NA  NA  
#> 2 the   THETA2    "V"   1.4     7.56e-2  0.0538 FALSE NA           2    NA  NA  
#> 3 the   THETA3    "RUV… 2.86    4.46e-1  0.156  FALSE NA           3    NA  NA  
#> 4 the   THETA4    "RUV… 0      NA       NA      TRUE  NA           4    NA  NA  
#> 5 ome   OMEGA(1,… "IIV… 0.489   7.64e-2  0.156  FALSE TRUE         1     1  54.5
#> 6 ome   OMEGA(2,… ""    0.998   1.24e-1  0.125  FALSE FALSE        2     1  NA  
#> 7 ome   OMEGA(2,… "IIV… 0.393   5.32e-2  0.135  FALSE TRUE         2     2  38.3
#> 8 sig   SIGMA(1,… ""    1      NA       NA      TRUE  TRUE         1     1  NA  
#> # ℹ 1 more variable: shk <num:3>
#> # Parameter table includes the following associations: CL~nmboxcox(IIVCL) and
#> V~nmboxcox(IIVV)

# A 'custom' use-case is when logexp, log(1+X), is
# desired but 1 is too large.
# Again, for this example, treating this like it applies here.
pheno_base %>%
  add_prm_association(V~custom(IIVV, qdist=function(x) log(0.001+x),
        pdist=function(x) exp(x)-0.001)) %>%
   get_prm()
#> Returning parameter estimates from $prob no.1, subprob no.1, method foce
#> # A tibble: 8 × 12
#>   type  name      label  value       se     rse fixed diagonal     m     n    cv
#> * <chr> <chr>     <chr> <num:>  <num:3> <num:3> <lgl> <lgl>    <int> <int> <num>
#> 1 the   THETA1    "CL"  0.0068  5.12e-4  0.0753 FALSE NA           1    NA  NA  
#> 2 the   THETA2    "V"   1.4     7.56e-2  0.0538 FALSE NA           2    NA  NA  
#> 3 the   THETA3    "RUV… 2.86    4.46e-1  0.156  FALSE NA           3    NA  NA  
#> 4 the   THETA4    "RUV… 0      NA       NA      TRUE  NA           4    NA  NA  
#> 5 ome   OMEGA(1,… "IIV… 0.489   7.64e-2  0.156  FALSE TRUE         1     1  52.0
#> 6 ome   OMEGA(2,… ""    0.998   1.24e-1  0.125  FALSE FALSE        2     1  NA  
#> 7 ome   OMEGA(2,… "IIV… 0.393   5.32e-2  0.135  FALSE TRUE         2     2  41.0
#> 8 sig   SIGMA(1,… ""    1      NA       NA      TRUE  TRUE         1     1  NA  
#> # ℹ 1 more variable: shk <num:3>
#> # Parameter table includes the following associations: V~custom(IIVV)

# Dropping association is easy
bad_assoc <- pheno_final %>%
   add_prm_association(the1~logit(IIVCL),Vpkg~logit(IIVV))
bad_assoc %>% get_prm()
#> Returning parameter estimates from $prob no.1, subprob no.1, method foce
#> # A tibble: 7 × 12
#>   type  name       label     value        se      rse fixed diagonal     m     n
#> * <chr> <chr>      <chr>   <num:4>   <num:4>  <num:4> <lgl> <lgl>    <int> <int>
#> 1 the   THETA1     "CLpk… 0.004813  2.365e-4  0.04914 FALSE NA           1    NA
#> 2 the   THETA2     "Vpkg" 0.9964    2.642e-2  0.02652 FALSE NA           2    NA
#> 3 the   THETA3     "RUVA… 2.784     2.513e-1  0.09027 FALSE NA           3    NA
#> 4 ome   OMEGA(1,1) "IIVC… 0.2009    5.108e-2  0.2543  FALSE TRUE         1     1
#> 5 ome   OMEGA(2,1) ""     0.7236    2.654e-1  0.3668  FALSE FALSE        2     1
#> 6 ome   OMEGA(2,2) "IIVV" 0.1576    2.614e-2  0.1659  FALSE TRUE         2     2
#> 7 sig   SIGMA(1,1) ""     1        NA        NA       TRUE  TRUE         1     1
#> # ℹ 2 more variables: cv <num:4>, shk <num:4>
#> # Parameter table includes the following associations: CLpkg~logit(IIVCL) and
#> Vpkg~logit(IIVV)
bad_assoc %>%
  drop_prm_association(the1) %>%
  get_prm()
#> Returning parameter estimates from $prob no.1, subprob no.1, method foce
#> # A tibble: 7 × 12
#>   type  name       label     value        se      rse fixed diagonal     m     n
#> * <chr> <chr>      <chr>   <num:4>   <num:4>  <num:4> <lgl> <lgl>    <int> <int>
#> 1 the   THETA1     "CLpk… 0.004813  2.365e-4  0.04914 FALSE NA           1    NA
#> 2 the   THETA2     "Vpkg" 0.9964    2.642e-2  0.02652 FALSE NA           2    NA
#> 3 the   THETA3     "RUVA… 2.784     2.513e-1  0.09027 FALSE NA           3    NA
#> 4 ome   OMEGA(1,1) "IIVC… 0.2009    5.108e-2  0.2543  FALSE TRUE         1     1
#> 5 ome   OMEGA(2,1) ""     0.7236    2.654e-1  0.3668  FALSE FALSE        2     1
#> 6 ome   OMEGA(2,2) "IIVV" 0.1576    2.614e-2  0.1659  FALSE TRUE         2     2
#> 7 sig   SIGMA(1,1) ""     1        NA        NA       TRUE  TRUE         1     1
#> # ℹ 2 more variables: cv <num:4>, shk <num:4>
#> # Parameter table includes the following associations: Vpkg~logit(IIVV)
```
