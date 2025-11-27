# Set probability columns for categorical endpoints

For categorical DVs or similar endpoints (such as censoring flag
columns, like `BLQ`), this function allows probability columns to be
defined for each level.

## Usage

``` r
set_dv_probs(
  xpdb,
  .problem = NULL,
  ...,
  .dv_var = NULL,
  .handle_missing = c("quiet", "warn", "error")
)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> object

- .problem:

  \<`numeric`\> Problem number to use. Uses the all problems if `NULL`

- ...:

  Formulas where LHS are levels or pseudo-functions (see Details), and
  RHS are columns with probabilities of those levels.

- .dv_var:

  \<`tidyselect`\> of column having the categorical observation. Default
  is first-listed `catdv`.

- .handle_missing:

  \<`character`\> How to handle missing levels: "quiet", "warn", or
  "error"

## Value

\<`xp_xtras`\> object with updated probabilities

## Details

The same probability cannot be assigned to multiple values.
Pseudo-functions can be used, or new columns can be created to overcome
this limitation. The available pseudo-functions should be written like
`ge(value)` (for `>=`), `gt(value)` (for `>`), etc. These comparison
names are those used in Perl, Fortran and many other languages. The
function `eq()` should not be used, but it will be ignored either way;
equivalence is implied with the base syntax.

## Examples

``` r
pkpd_m3 %>%
 # Not necessary, but correct to set var type before using this
 set_var_types(.problem=1, catdv=BLQ, dvprobs=LIKE) %>%
 # Set var type. Warnings can be helpful unless an inverse likelihood column is available
 set_dv_probs(.problem=1, 1~LIKE, .dv_var = BLQ, .handle_missing = "warn") %>%
 list_vars()
#> Warning: BLQ values are missing in probabilities: BLQ. If other probabilities don't add
#> up 1, the inverse will apply to uncounted levels.
#> Warning: This check does not consider qualifiers that may implicitly include uncounted
#> levels (eg, `lt(1)`).
#> List of available variables for problem no. 1
#>  - Subject identifier (id)               : ID
#>  - Dependent variable (dv)               : DV
#>  - Categorical endpoint (catdv)          : BLQ [0]
#>  - DV Probabilities (dvprobs)            : LIKE [P(*.eq.1)]
#>  - Independent variable (idv)            : TIME
#>  - Dose amount (amt)                     : AMT
#>  - Event identifier (evid)               : EVID
#>  - Missing dependent variable (mdv)      : MDV
#>  - Model typical predictions (pred)      : PRED
#>  - Eta (eta)                             : ETA1, ETA2
#>  - Residuals (res)                       : NPDE, IWRES, CWRES, RES, WRES
#>  - Not attributed (na)                   : DOSE, LLOQ, IPRE, IRES, BASE, KDEG, IMAX, IC50, CL, VC, Q, VP, KA

# Same as above with demo of inverse column
pkpd_m3 %>%
 xpose::mutate(INVLIKE = 1-LIKE) %>%
 set_var_types(.problem=1, catdv=BLQ, dvprobs=c(LIKE,INVLIKE)) %>%
 # Note no warning
 set_dv_probs(.problem=1, 1~LIKE, 0~INVLIKE, .dv_var = BLQ, .handle_missing = "warn")%>%
 list_vars()
#> List of available variables for problem no. 1
#>  - Subject identifier (id)               : ID
#>  - Dependent variable (dv)               : DV
#>  - Categorical endpoint (catdv)          : BLQ [0]
#>  - DV Probabilities (dvprobs)            : LIKE [P(*.eq.1)], INVLIKE [P(*.eq.0)]
#>  - Independent variable (idv)            : TIME
#>  - Dose amount (amt)                     : AMT
#>  - Event identifier (evid)               : EVID
#>  - Missing dependent variable (mdv)      : MDV
#>  - Model typical predictions (pred)      : PRED
#>  - Eta (eta)                             : ETA1, ETA2
#>  - Residuals (res)                       : NPDE, IWRES, CWRES, RES, WRES
#>  - Not attributed (na)                   : DOSE, LLOQ, IPRE, IRES, BASE, KDEG, IMAX, IC50, CL, VC, Q, VP, KA

# With categorical model
vismo_pomod  %>%
 # Update var types
 set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
 # Warning (as noted), does not recognize 3 is covered implicitly. That's ok!
 set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23, .handle_missing = "warn")%>%
 list_vars()
#> Warning: DV values are missing in probabilities: DV. If other probabilities don't add up
#> 1, the inverse will apply to uncounted levels.
#> Warning: This check does not consider qualifiers that may implicitly include uncounted
#> levels (eg, `lt(1)`).
#> List of available variables for problem no. 1
#>  - Subject identifier (id)               : ID
#>  - Categorical endpoint (catdv)          : DV [0]
#>  - DV Probabilities (dvprobs)            : P0 [P(*.eq.0)], P1 [P(*.eq.1)], P23 [P(*.ge.2)]
#>  - Independent variable (idv)            : TIME
#>  - Dose amount (amt)                     : AMT
#>  - Event identifier (evid)               : EVID
#>  - Model typical predictions (pred)      : PRED
#>  - Eta (eta)                             : ETA1
#>  - Residuals (res)                       : RES, WRES
#>  - Not attributed (na)                   : STUDY, COHORT, PTNM, FRMU, BAAG, AGE, BWT, PTS, B1, B2, Y, U449E, CE

# Same as above, but...
vismo_pomod  %>%
 set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
 # Default is to not bother users with a warning
 set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23)%>%
 list_vars()
#> List of available variables for problem no. 1
#>  - Subject identifier (id)               : ID
#>  - Categorical endpoint (catdv)          : DV [0]
#>  - DV Probabilities (dvprobs)            : P0 [P(*.eq.0)], P1 [P(*.eq.1)], P23 [P(*.ge.2)]
#>  - Independent variable (idv)            : TIME
#>  - Dose amount (amt)                     : AMT
#>  - Event identifier (evid)               : EVID
#>  - Model typical predictions (pred)      : PRED
#>  - Eta (eta)                             : ETA1
#>  - Residuals (res)                       : RES, WRES
#>  - Not attributed (na)                   : STUDY, COHORT, PTNM, FRMU, BAAG, AGE, BWT, PTS, B1, B2, Y, U449E, CE
```
