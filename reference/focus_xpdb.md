# Focus on an xpdb object in an xpose_set

For piping, set is passed, but with S3 method transformations are
applied to the focused `xpdb` object.

## Usage

``` r
focus_xpdb(xpdb_s, ..., .add = FALSE)

unfocus_xpdb(xpdb_s)

focused_xpdbs(xpdb_s)

focus_function(xpdb_s, fn, ...)

focus_qapply(xpdb_s, fn, ..., .mods = everything())
```

## Arguments

- xpdb_s:

  \<[`xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)\>
  An xpose_set object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  One or more xpdb objects to focus on

- .add:

  \<[`logical`](https://rdrr.io/r/base/logical.html)\> Should the focus
  be added to the existing focus? (default: `FALSE`)

- fn:

  \<`function`\> to apply to focused `xpose_data` objects

- .mods:

  \<`tidyselect`\> Model names in set to quick-apply a function. See
  Details.

## Value

An `xpose_set` object with the focused xpdb object(s)

## Details

While these functions are used internally, it is recognized that they
may have value in user scripting. It is hoped these are
self-explanatory, but the examples should address common uses.

*Note:* `focus_qapply()` (re)focuses as specified in `.mods` and then
un-focuses all elements of the set so should only be used in the case
where a quick application suffices. Otherwise, focusing with a sequence
of `focus_function` calls (or a monolithic single `focus_function` call
with a custom function) should be preferred.

## Examples

``` r
# Select two xpdb objects to focus on
xpdb_set %>% focus_xpdb(mod2,fix1)
#> 
#> ── xpose_set object ────────────────────────────────────────────────────────────
#> • Number of models: 4
#> • Model labels: mod1, mod2, fix1, and fix2
#> • Number of relationships: 3
#> • Focused xpdb objects: mod2 and fix1
#> • Exposed properties: none
#> • Base model: none

# Add a focus
xpdb_set %>% focus_xpdb(mod2,fix1) %>% focus_xpdb(mod1, .add=TRUE)
#> 
#> ── xpose_set object ────────────────────────────────────────────────────────────
#> • Number of models: 4
#> • Model labels: mod1, mod2, fix1, and fix2
#> • Number of relationships: 3
#> • Focused xpdb objects: mod1, mod2, and fix1
#> • Exposed properties: none
#> • Base model: none

# Remove focus
xpdb_set %>% focus_xpdb(mod2,fix1) %>% focus_xpdb()
#> 
#> ── xpose_set object ────────────────────────────────────────────────────────────
#> • Number of models: 4
#> • Model labels: mod1, mod2, fix1, and fix2
#> • Number of relationships: 3
#> • Focused xpdb objects: none
#> • Exposed properties: none
#> • Base model: none

# Focus function and tidyselect
pheno_set %>%
  focus_xpdb(everything()) %>%
  # Add iOFV col and iofv type to all xpdbs in set
  focus_function(backfill_iofv) %>%
  # Show 1... can do all like this, too, but no need
  unfocus_xpdb() %>%
  select(run6) %>%
  {.[[1]]$xpdb} %>%
  list_vars()
#> List of available variables for problem no. 1
#>  - Subject identifier (id)               : ID
#>  - Dependent variable (dv)               : DV
#>  - Independent variable (idv)            : TIME
#>  - Dose amount (amt)                     : AMT
#>  - Event identifier (evid)               : EVID
#>  - Missing dependent variable (mdv)      : MDV
#>  - Model typical predictions (pred)      : PRED
#>  - Model individual predictions (ipred)  : IPRED
#>  - Eta (eta)                             : ETA1, ETA2
#>  - Individual OFV (iofv)                 : iOFV
#>  - Residuals (res)                       : IWRES, CWRES, NPDE, RES, WRES
#>  - Categorical covariates (catcov)       : APGR ('Apgar score') [10]
#>  - Continuous covariates (contcov)       : WT ('Weight', kg)
#>  - Not attributed (na)                   : IRES, CL, V, CRES

# Quick-apply version of previous example
pheno_set %>%
  focus_qapply(backfill_iofv) %>%
  select(run6) %>%
  {.[[1]]$xpdb} %>%
  list_vars()
#> List of available variables for problem no. 1
#>  - Subject identifier (id)               : ID
#>  - Dependent variable (dv)               : DV
#>  - Independent variable (idv)            : TIME
#>  - Dose amount (amt)                     : AMT
#>  - Event identifier (evid)               : EVID
#>  - Missing dependent variable (mdv)      : MDV
#>  - Model typical predictions (pred)      : PRED
#>  - Model individual predictions (ipred)  : IPRED
#>  - Eta (eta)                             : ETA1, ETA2
#>  - Individual OFV (iofv)                 : iOFV
#>  - Residuals (res)                       : IWRES, CWRES, NPDE, RES, WRES
#>  - Categorical covariates (catcov)       : APGR ('Apgar score') [10]
#>  - Continuous covariates (contcov)       : WT ('Weight', kg)
#>  - Not attributed (na)                   : IRES, CL, V, CRES
```
