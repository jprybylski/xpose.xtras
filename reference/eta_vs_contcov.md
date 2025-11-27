# Eta continuous covariate plots (typical)

Eta continuous covariate plots (typical)

## Usage

``` r
eta_vs_contcov(
  xpdb,
  mapping = NULL,
  etavar = NULL,
  drop_fixed = TRUE,
  linsm = FALSE,
  type = "ps",
  title = "Eta versus continuous covariates | @run",
  subtitle = "Based on @nind individuals, Eta shrink: @etashk",
  caption = "@dir",
  tag = NULL,
  log = NULL,
  guide = TRUE,
  facets,
  .problem,
  quiet,
  ...
)
```

## Arguments

- xpdb:

  \<`xp_xtras> or <`xpose_data\`\> object

- mapping:

  `ggplot2` style mapping

- etavar:

  `tidyselect` for `eta` variables

- drop_fixed:

  As in `xpose`

- linsm:

  If `type` contains "s" should the smooth method by `lm`?

- type:

  Passed to `xplot_scatter`

- title:

  Plot title

- subtitle:

  Plot subtitle

- caption:

  Plot caption

- tag:

  Plot tag

- log:

  Log scale covariate value?

- guide:

  Add guide line?

- facets:

  Additional facets

- .problem:

  Problem number

- quiet:

  Silence output

- ...:

  Any additional aesthetics.

## Value

The desired plot

## Examples

``` r
# \donttest{

eta_vs_contcov(xpdb_x)
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Tidying data by ID, SEX, MED1, MED2, DOSE ... and 23 more variables
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Tidying data by ID, SEX, MED1, MED2, DOSE ... and 23 more variables
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Tidying data by ID, SEX, MED1, MED2, DOSE ... and 23 more variables
#> [[1]]
#> `geom_smooth()` using formula = 'y ~ x'

#> 
#> [[2]]
#> `geom_smooth()` using formula = 'y ~ x'

#> 
#> [[3]]
#> `geom_smooth()` using formula = 'y ~ x'

#> 

# Labels and units are also supported
xpdb_x %>%
  xpose::set_var_labels(AGE="Age", MED1 = "Digoxin") %>%
  xpose::set_var_units(AGE="yrs") %>%
  set_var_levels(SEX=lvl_sex(), MED1 = lvl_bin()) %>%
  eta_vs_contcov()
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `out = purrr::map_if(...)`.
#> Caused by warning:
#> ! In $prob no.2 columns: MED1 not present in the data.
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Tidying data by ID, SEX, MED1, MED2, DOSE ... and 23 more variables
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Tidying data by ID, SEX, MED1, MED2, DOSE ... and 23 more variables
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Tidying data by ID, SEX, MED1, MED2, DOSE ... and 23 more variables
#> [[1]]
#> `geom_smooth()` using formula = 'y ~ x'

#> 
#> [[2]]
#> `geom_smooth()` using formula = 'y ~ x'

#> 
#> [[3]]
#> `geom_smooth()` using formula = 'y ~ x'

#> 
# }
```
