# For a categorical DV variable, show associated probabilities

A convenient quick check for how probabilities are currently assigned,
based on
[`set_dv_probs`](https://jprybylski.github.io/xpose.xtras/reference/set_dv_probs.md).

## Usage

``` r
list_dv_probs(xpdb, .problem = NULL, .dv_var = NULL)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> object

- .problem:

  \<`numeric`\> Problem number to use. Uses the all problems if `NULL`

- .dv_var:

  \<`tidyselect`\> of column having the categorical observation. Default
  is first-listed `catdv`.

## Value

\<`tibble`\> of probabilities

## Examples

``` r
pkpd_m3 %>%
  set_dv_probs(1, 1~LIKE, .dv_var = BLQ) %>%
  list_dv_probs(.dv_var=BLQ)
#> Warning: Var types not properly assigned as `dvprobs`, but probabilities will still be
#> applied: LIKE
#> Warning: Var type for DV not properly assigned as `catdv`, but probabilities will still
#> be applied: BLQ
#> # A tibble: 1 × 3
#>   value qual  prob 
#>   <dbl> <chr> <chr>
#> 1     1 NA    LIKE 
```
