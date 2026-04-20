# Derive full parameter set for mammillary PK model

This function applies
[`rxode2::rxDerived`](https://nlmixr2.github.io/rxode2/reference/rxDerived.html)
to model parameters.

## Usage

``` r
derive_prm(
  xpdb,
  .prm = NULL,
  .problem,
  quiet = xpdb$options$quiet,
  prefix = ""
)

backfill_derived(
  xpdb,
  .prm = NULL,
  .problem,
  quiet = xpdb$options$quiet,
  ...,
  group_vars = "id"
)
```

## Arguments

- xpdb:

  xpdb \<`xpose_data`\> object

- .prm:

  \<`tidyselect`\> Parameters to convert (if `NULL`, the function will
  use `xp_var` `param` types)

- .problem:

  Optional. Problem to use.

- quiet:

  Optional. Extra output.

- prefix:

  If desired, apply prefix to new parameters.

- ...:

  Passed to `derive_prm()`

- group_vars:

  Variable type(s) to join derived parameters on.

## Value

\<`data.frame`\> of data with new parameters

## Examples

``` r
if (FALSE) { # \dontrun{
nlmixr2_m3 <- nlmixr_example("nlmixr2_m3")

nlmixr2_m3 %>%
  backfill_derived() %>%
  list_vars()

derive_prm(nlmixr2_m3)

# If param has no vars, .prm should be set
pheno_base %>%
  backfill_derived(
    .prm = c(CL,V)
  ) %>%
  list_vars()
} # }
```
