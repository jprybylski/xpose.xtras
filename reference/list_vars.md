# Updates to `list_vars`

To accommodate changes made in `xpose.xtras`,
\<[`list_vars`](https://uupharmacometrics.github.io/xpose/reference/list_vars.html)\>
needed some minimal updates.

## Usage

``` r
list_vars(xpdb, .problem = NULL, ...)

# Default S3 method
list_vars(xpdb, .problem = NULL, ...)

# S3 method for class 'xp_xtras'
list_vars(xpdb, .problem = NULL, ...)
```

## Arguments

- xpdb:

  \<`xpose_data`\> or \<`xp_xtras`\> object

- .problem:

  \<`numeric`\> Problem number to use. Uses the all problems if `NULL`

- ...:

  Should be blank.

## Value

\<`tibble`\> of variables

## Examples

``` r
list_vars(xpose::xpdb_ex_pk)
#> 
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
#>  - Residuals (res)                       : CWRES, IWRES, RES, WRES
#>  - Categorical covariates (catcov)       : SEX, MED1, MED2
#>  - Continuous covariates (contcov)       : CLCR, AGE, WT
#>  - Compartment amounts (a)               : A1, A2
#>  - Not attributed (na)                   : DOSE, SS, II, TAD, CPRED
#> 
#> List of available variables for problem no. 2 
#>  - Subject identifier (id)               : ID
#>  - Dependent variable (dv)               : DV
#>  - Independent variable (idv)            : TIME
#>  - Dose amount (amt)                     : AMT
#>  - Event identifier (evid)               : EVID
#>  - Model individual predictions (ipred)  : IPRED
#>  - Not attributed (na)                   : DOSE, TAD, SEX, CLCR, AGE, WT

list_vars(xpdb_x)
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
