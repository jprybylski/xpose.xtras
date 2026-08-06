# Derive per-individual contribution to eta shrinkage

Computes, for each selected eta, a diagnostic column highlighting each
individual's contribution to shrinkage: \\\log((\eta_i - \bar\eta)^2)\\,
ie the log of the squared deviation from the population mean eta. Since
shrinkage itself is `100 * (1 - SD(eta)/omega)` (see
[`recalc_shk()`](https://jprybylski.github.io/xpose.xtras/reference/recalc_shk.md)),
and `SD(eta)^2` is the mean of these per-individual squared deviations,
this highlights which individuals are pulling shrinkage down. The `log`
spreads out values close to `0`, ie individuals contributing the least
(the most heavily shrunk).

`derive_shk()` returns the augmented data as a plain data frame, like
[`xpose::get_data()`](https://uupharmacometrics.github.io/xpose/reference/get_data.html)'s
output. `backfill_shk()` joins the new column(s) back into `xpdb` and
tags them with the `shk` variable type. This has to be backfilled rather
than parsed, since it isn't something NONMEM (or any other supported
software) reports directly.

## Usage

``` r
derive_shk(xpdb, ..., .problem = NULL, quiet)

backfill_shk(xpdb, ..., .problem = NULL, quiet)
```

## Arguments

- xpdb:

  \<`xpose_data`[xpose::xpose_data](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  or `xp_xtras` object

- ...:

  \<`tidyselect`\> Which eta column(s) to derive a shrinkage
  contribution for. Defaults to every `eta` column for `.problem`.

- .problem:

  \<`numeric`\> Problem number to use. Uses the xpose default if not
  provided.

- quiet:

  \<`logical`\> Silence extra debugging output

## Value

For `derive_shk()`, a data frame with one new column per selected eta,
named `<eta>_SHK`. For `backfill_shk()`, the updated `xp_xtras` object,
with those columns joined in and typed `shk`.

## Examples

``` r
derive_shk(xpdb_x) %>%
  dplyr::select(ID, dplyr::ends_with("_SHK")) %>%
  head()
#> # A tibble: 6 × 4
#>   ID    ETA1_SHK ETA2_SHK ETA3_SHK
#>   <fct>    <dbl>    <dbl>    <dbl>
#> 1 110      -6.00    -8.18     1.27
#> 2 110      -6.00    -8.18     1.27
#> 3 110      -6.00    -8.18     1.27
#> 4 110      -6.00    -8.18     1.27
#> 5 110      -6.00    -8.18     1.27
#> 6 110      -6.00    -8.18     1.27

xpdb_x %>%
  backfill_shk() %>%
  list_vars()
#> List of available variables for problem no. 1
#>  - Subject identifier (id)               : ID
#>  - Dependent variable (dv)               : DV
#>  - Independent variable (idv)            : TIME
#>  - Dose amount (amt)                     : AMT
#>  - Event identifier (evid)               : EVID
#>  - Model typical predictions (pred)      : PRED
#>  - Model individual predictions (ipred)  : IPRED
#>  - Model parameter (param)               : KA, CL, V, ALAG1
#>  - Eta (eta)                             : ETA1, ETA2, ETA3
#>  - Shrinkage contribution (shk)          : ETA1_SHK, ETA2_SHK, ETA3_SHK
#>  - Residuals (res)                       : CWRES, IWRES, RES, WRES
#>  - Categorical covariates (catcov)       : SEX [0], MED1 [0], MED2 [0]
#>  - Continuous covariates (contcov)       : CLCR, AGE, WT
#>  - Compartment amounts (a)               : A1, A2
#>  - Not attributed (na)                   : DOSE, SS, II, TAD, CPRED
#> List of available variables for problem no. 2
#>  - Subject identifier (id)               : ID
#>  - Dependent variable (dv)               : DV
#>  - Independent variable (idv)            : TIME
#>  - Dose amount (amt)                     : AMT
#>  - Event identifier (evid)               : EVID
#>  - Model individual predictions (ipred)  : IPRED
#>  - Not attributed (na)                   : DOSE, TAD, SEX, CLCR, AGE, WT
```
