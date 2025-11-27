# Transform parameter values in place

Apply transformations to fitted parameter values.

As fitted, sometimes parameter values are not as easy to communicate,
but to transform them outside of the `xpose` ecosystem limits some
available features. To have the best experience, this function can
update the parameter values that are used by `xpose` `get_prm`
functions. At this time these transformations are not applied to `param`
vars
([`list_vars`](https://jprybylski.github.io/xpose.xtras/reference/list_vars.md)),
but that can already be done with the `mutate` method.

**This only works for theta parameters.**

All valid mutations are applied sequentially, so a double call to
`the2~the2^3` will result in effectively `the2~the2^9`, for example.

RSE values are calculated at runtime within `get_prm`, so they are not
updated (or updatable) with this function.

## Usage

``` r
mutate_prm(
  xpdb,
  ...,
  .autose = TRUE,
  .problem = NULL,
  .subprob = NULL,
  .method = NULL,
  .sesim = 1e+05,
  quiet
)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> object

- ...:

  ...
  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  One or more formulae that define transformations to parameters. RHS of
  formulas can be function or a value. That value can be a function call
  like in
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  (`the1~exp(the1)`).

- .autose:

  \<`logical`\> If a function is used for the transform then simulation
  is used to transform the current SE to a new SE. Precision of this
  transformation is dependent on `.sesim`. If parameter values are not
  assigned with a function, this option will simply scale SE to maintain
  the same RSE. See Details.

- .problem:

  \<`numeric`\> Problem number to apply this relationship.

- .subprob:

  \<`numeric`\> Problem number to apply this relationship.

- .method:

  \<`numeric`\> Problem number to apply this relationship.

- .sesim:

  \<`numeric`\> Length of simulated `rnorm` vector for `.autose`.

- quiet:

  Silence extra output.

## Value

An updated `xp_xtras` object with mutated parameters

## Details

### Important points about covariance and correlation (for NONMEM only)

Covariance and correlation parameters are adjusted when standard error
(SE) values are changed directly or with `.autose`. When a
transformation is applied as a function for the fixed effect parameter
(eg, `~plogis`), the resulting SE may have an unexpected scale; this is
because it is now reporting the standard deviation of a transformed and
potentially non-normal distribution. If the parameter were fit in the
transformed scale (constrained to any appropriate bounds), it would
likely have a different SE given that most covariance estimation methods
(excluding non-parametric and resampling-based) will treat the
constrained parameter as continuous and unconstrained.

The updates to variance-covariance values (and the correlation values,
though that is mostly invariant) are applied to the entire matrices.
When piped directly into `get_prm`, only the SE estimate is shown, but
\<[`get_file`](https://uupharmacometrics.github.io/xpose/reference/get_file.html)\>
can be used to see the complete updated variance-covariance values. This
could be useful if those matrices are being used to define priors for a
Bayesian model fitting, as the re-scaling of off-diagonal elements is
handled automatically.

For all software: A function to transform parameters will result in a
more accurate `autose` result. If a call (`the1~exp(the)`) or a value
(`the1~2`) are used, the standard error will be simply scaled.

## Examples

``` r
vismo_pomod %>%
  # Function
  mutate_prm(THETA11~exp) %>%
  # Value (se will not be scaled); plogis = inverse logit
  mutate_prm(THETA12~plogis(THETA12)) %>%
  get_prm()
#> Warning: Since a function was not provided, `autose` will be scaled to maintain the same
#> RSE.
#> Returning parameter estimates from $prob no.1, subprob no.1, method lce
#> Warning: [$prob no.1, subprob no.1, lce] $SIGMA labels did not match the number of SIGMAs in the `.ext` file.
#> Warning: Shrinkage missing for sigma estimates, if any are modeled. Using NA in this
#> table.
#> # A tibble: 16 × 12
#>    type  name    label   value       se     rse fixed diagonal     m     n    cv
#>  * <chr> <chr>   <chr> <num:3>  <num:3> <num:3> <lgl> <lgl>    <int> <int> <num>
#>  1 the   THETA1  "THE…  7.19   NA       NA      TRUE  NA           1    NA  NA  
#>  2 the   THETA2  "THE…  4.06   NA       NA      TRUE  NA           2    NA  NA  
#>  3 the   THETA3  "THE… -2.88   NA       NA      TRUE  NA           3    NA  NA  
#>  4 the   THETA4  "THE…  2.2    NA       NA      TRUE  NA           4    NA  NA  
#>  5 the   THETA5  "THE…  0.671  NA       NA      TRUE  NA           5    NA  NA  
#>  6 the   THETA6  "THE… -1.06   NA       NA      TRUE  NA           6    NA  NA  
#>  7 the   THETA7  "THE…  0.881  NA       NA      TRUE  NA           7    NA  NA  
#>  8 the   THETA8  "THE… -0.602  NA       NA      TRUE  NA           8    NA  NA  
#>  9 the   THETA9  "THE… -0.527  NA       NA      TRUE  NA           9    NA  NA  
#> 10 the   THETA10 "THE…  0.66   NA       NA      TRUE  NA          10    NA  NA  
#> 11 the   THETA11 "LOG…  0.0333  1.5 e-3  0.045  FALSE NA          11    NA  NA  
#> 12 the   THETA12 "THE…  0.0157  7.27e-4  0.0463 FALSE NA          12    NA  NA  
#> 13 the   THETA13 "THE… -4.35    2.75e-1  0.0631 FALSE NA          13    NA  NA  
#> 14 the   THETA14 "THE… 21.9     2.44e+0  0.111  FALSE NA          14    NA  NA  
#> 15 ome   OMEGA(… "ETA…  0.639   8.16e-2  0.128  FALSE TRUE         1     1  71.0
#> 16 sig   SIGMA(… ""     0      NA       NA      TRUE  TRUE         1     1  NA  
#> # ℹ 1 more variable: shk <num:3>

```
