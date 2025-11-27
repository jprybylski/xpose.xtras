# Add individual objective function to data

Add individual objective function to data

## Usage

``` r
backfill_iofv(xpdb, .problem = NULL, .subprob = NULL, .label = "iOFV")
```

## Arguments

- xpdb:

  \<`xpose_data`\> or \<`xp_xtras`\> object

- .problem:

  Problem number

- .subprob:

  Subproblem number

- .label:

  The name of the new column. `iOFV` is the default.

## Value

\<`xp_xtras`\> object with new column in the data and a column with
`iofv` var type.

## Details

This function will only work for objects with software listed as
`nonmem` or `nlmixr2`. For `nonmem`, the object should haves a `phi`
file and with an `OBJ` column in that file. For `nlmixr2`, the fit
object data should have individual observation likelihoods in a column
called `NLMIXRLLIKOBS` (this is a current standard, but is checked at
runtime).

## Examples

``` r
xpdb_x %>%
  backfill_iofv() %>%
  list_vars()
#> List of available variables for problem no. 1
#>  - Subject identifier (id)               : ID
#>  - Dependent variable (dv)               : DV
#>  - Independent variable (idv)            : TIME
#>  - Time after dose (tad) (tad)           : TAD
#>  - Dose amount (amt)                     : AMT
#>  - Event identifier (evid)               : EVID
#>  - Model typical predictions (pred)      : PRED
#>  - Model individual predictions (ipred)  : IPRED
#>  - Model parameter (param)               : KA, CL, V, ALAG1
#>  - Eta (eta)                             : ETA1, ETA2, ETA3
#>  - Individual OFV (iofv)                 : iOFV
#>  - Residuals (res)                       : CWRES, IWRES, RES, WRES
#>  - Categorical covariates (catcov)       : SEX [0], MED1 [0], MED2 [0]
#>  - Continuous covariates (contcov)       : CLCR, AGE, WT
#>  - Compartment amounts (a)               : A1, A2
#>  - Not attributed (na)                   : DOSE, SS, II, CPRED
#> List of available variables for problem no. 2
#>  - Subject identifier (id)               : ID
#>  - Dependent variable (dv)               : DV
#>  - Independent variable (idv)            : TIME
#>  - Time after dose (tad) (tad)           : TAD
#>  - Dose amount (amt)                     : AMT
#>  - Event identifier (evid)               : EVID
#>  - Model individual predictions (ipred)  : IPRED
#>  - Not attributed (na)                   : DOSE, SEX, CLCR, AGE, WT
```
