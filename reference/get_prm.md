# Access model parameters

Access model parameter estimates from an xpdb object.

Methods have been added to implement extensions. See Details.

## Usage

``` r
get_prm(
  xpdb,
  .problem = NULL,
  .subprob = NULL,
  .method = NULL,
  digits = 4,
  transform = TRUE,
  show_all = FALSE,
  quiet
)
```

## Arguments

- xpdb:

  An `xpose_data` object from which the model output file data will be
  extracted.

- .problem:

  The problem to be used, by default returns the last one for each file.

- .subprob:

  The subproblem to be used, by default returns the last one for each
  file.

- .method:

  The estimation method to be used, by default returns the last one for
  each file

- digits:

  The number of significant digits to be displayed.

- transform:

  Should diagonal OMEGA and SIGMA elements be transformed to standard
  deviation and off diagonal elements be transformed to correlations.

- show_all:

  Logical, whether the 0 fixed off-diagonal elements should be removed
  from the output.

- quiet:

  Logical, if `FALSE` messages are printed to the console.

## Value

A tibble for single problem/subprob or a named list for multiple
problem\|subprob.

## Details

When using an \<`xp_xtra`\> object, this function will add a column to
the output where CV% for each diagonal element of omega is calculated.
This CV% is with respect to the resulting structural parameter, so
unless the default log-normal association is applicable update with
[`add_prm_association`](https://jprybylski.github.io/xpose.xtras/reference/add_prm_association.md).

For log-normal, users may prefer to use the first-order CV%
(\\\sqrt{\omega^2}\\) instead of the exact. In such case,
`xpdb <- set_option(xpdb, cvtype="sqrt")` will get that preferred form.

If a single omega parameter is associated with multiple fixed effect
parameters, the `cv` column will be a list. For the `omega` row
associated with multiple fixed effect parameters, there will be multiple
CV values. This will be the case even if the transformation is
log-normal and therefore scale-invariant, given the need for generality.

**Note** the approach used to calculate CV% assumes an untransformed
scale for the fitted parameter value (unrelated to `transform`=TRUE).
That means, for example, that for a logit-normal fitted parameter value,
it is expected the value will be something constrained between 0 and 1,
not the unbounded, continuous transformed value. The function
\<[`mutate_prm`](https://jprybylski.github.io/xpose.xtras/reference/mutate_prm.md)\>
is intended to help where that might be an issue.

## References

Prybylski, J.P. Reporting Coefficient of Variation for Logit, Box-Cox
and Other Non-log-normal Parameters. Clin Pharmacokinet 63, 133-135
(2024).
[doi:10.1007/s40262-023-01343-2](https://doi.org/10.1007/s40262-023-01343-2)

## See also

[`add_prm_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_prm_association.md)

## Examples

``` r
# xpose parameter table
get_prm(xpose::xpdb_ex_pk, .problem = 1)
#> Returning parameter estimates from $prob no.1, subprob no.1, method foce
#> # A tibble: 11 × 10
#>    type  name       label      value       se     rse fixed diagonal     m     n
#>  * <chr> <chr>      <chr>      <dbl>    <dbl>   <dbl> <lgl> <lgl>    <dbl> <dbl>
#>  1 the   THETA1     "TVCL"   2.63e+1  0.892    0.0339 FALSE NA           1    NA
#>  2 the   THETA2     "TVV"    1.35e+0  0.0438   0.0325 FALSE NA           2    NA
#>  3 the   THETA3     "TVKA"   4.20e+0  0.809    0.192  FALSE NA           3    NA
#>  4 the   THETA4     "LAG"    2.08e-1  0.0157   0.0755 FALSE NA           4    NA
#>  5 the   THETA5     "Prop. … 2.05e-1  0.0224   0.110  FALSE NA           5    NA
#>  6 the   THETA6     "Add. E… 1.06e-2  0.00366  0.347  FALSE NA           6    NA
#>  7 the   THETA7     "CRCL o… 7.17e-3  0.00170  0.237  FALSE NA           7    NA
#>  8 ome   OMEGA(1,1) "IIV CL" 2.70e-1  0.0233   0.0862 FALSE TRUE         1     1
#>  9 ome   OMEGA(2,2) "IIV V"  1.95e-1  0.0320   0.164  FALSE TRUE         2     2
#> 10 ome   OMEGA(3,3) "IIV KA" 1.38e+0  0.202    0.146  FALSE TRUE         3     3
#> 11 sig   SIGMA(1,1) ""       1   e+0 NA       NA      TRUE  TRUE         1     1

# xpose.xtra parameter table (basically the same)
get_prm(pheno_final, .problem = 1)
#> Returning parameter estimates from $prob no.1, subprob no.1, method foce
#> # A tibble: 7 × 12
#>   type  name  label    value        se      rse fixed diagonal     m     n    cv
#> * <chr> <chr> <chr>  <num:4>   <num:4>  <num:4> <lgl> <lgl>    <int> <int> <num>
#> 1 the   THET… "CLp… 0.004813  2.365e-4  0.04914 FALSE NA           1    NA NA   
#> 2 the   THET… "Vpk… 0.9964    2.642e-2  0.02652 FALSE NA           2    NA NA   
#> 3 the   THET… "RUV… 2.784     2.513e-1  0.09027 FALSE NA           3    NA NA   
#> 4 ome   OMEG… "IIV… 0.2009    5.108e-2  0.2543  FALSE TRUE         1     1 20.30
#> 5 ome   OMEG… ""    0.7236    2.654e-1  0.3668  FALSE FALSE        2     1 NA   
#> 6 ome   OMEG… "IIV… 0.1576    2.614e-2  0.1659  FALSE TRUE         2     2 15.86
#> 7 sig   SIGM… ""    1        NA        NA       TRUE  TRUE         1     1 NA   
#> # ℹ 1 more variable: shk <num:4>

# For the sake of example, even though these were all lognormal:
pheno_final %>%
  add_prm_association(CLpkg~logit(IIVCL)) %>%
  add_prm_association(Vpkg~nmboxcox(IIVV, lambda = 0.01)) %>%
  get_prm(.problem = 1)
#> Returning parameter estimates from $prob no.1, subprob no.1, method foce
#> # A tibble: 7 × 12
#>   type  name  label    value        se      rse fixed diagonal     m     n    cv
#> * <chr> <chr> <chr>  <num:4>   <num:4>  <num:4> <lgl> <lgl>    <int> <int> <num>
#> 1 the   THET… "CLp… 0.004813  2.365e-4  0.04914 FALSE NA           1    NA NA   
#> 2 the   THET… "Vpk… 0.9964    2.642e-2  0.02652 FALSE NA           2    NA NA   
#> 3 the   THET… "RUV… 2.784     2.513e-1  0.09027 FALSE NA           3    NA NA   
#> 4 ome   OMEG… "IIV… 0.2009    5.108e-2  0.2543  FALSE TRUE         1     1 20.19
#> 5 ome   OMEG… ""    0.7236    2.654e-1  0.3668  FALSE FALSE        2     1 NA   
#> 6 ome   OMEG… "IIV… 0.1576    2.614e-2  0.1659  FALSE TRUE         2     2 15.85
#> 7 sig   SIGM… ""    1        NA        NA       TRUE  TRUE         1     1 NA   
#> # ℹ 1 more variable: shk <num:4>
#> # Parameter table includes the following associations: CLpkg~logit(IIVCL) and
#> Vpkg~nmboxcox(IIVV)

```
