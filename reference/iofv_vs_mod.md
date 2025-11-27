# Objective function changes across models

Another visualization of how individual objective functions change over
the course of model development.

## Usage

``` r
iofv_vs_mod(
  xpdb_s,
  ...,
  .lineage = FALSE,
  auto_backfill = FALSE,
  mapping = NULL,
  orientation = "x",
  type = "bjc",
  title = "Individual Ofvs across models",
  subtitle = "Based on @nind individuals, Initial Ofv: @ofv",
  caption = "Initial @dir",
  tag = NULL,
  axis.text = "@run",
  facets,
  .problem,
  quiet
)
```

## Arguments

- xpdb_s:

  \<`xpose_set`\> object

- ...:

  \<`tidyselect`\> of models in set. If empty, all models are used in
  order of their position in the set. May also use a formula, which will
  just be processed with
  [`all.vars()`](https://rdrr.io/r/base/allnames.html).

- .lineage:

  \<`logical`\> where if `TRUE`, `...` is processed

- auto_backfill:

  \<`logical`\> If `TRUE`, apply
  \<[`backfill_iofv()`](https://jprybylski.github.io/xpose.xtras/reference/backfill_iofv.md)\>
  automatically. `FALSE` by default to encourage data control as a
  separate process to plotting control.

- mapping:

  `ggplot2` style mapping

- orientation:

  Defaults to `x`

- type:

  Passed to
  \<[`xplot_boxplot`](https://jprybylski.github.io/xpose.xtras/reference/xplot_boxplot.md)\>

- title:

  Plot title

- subtitle:

  Plot subtitle

- caption:

  Plot caption

- tag:

  Plot tag

- axis.text:

  What to label the model. This is parsed on a per-model basis.

- facets:

  Additional facets

- .problem:

  Problem number

- quiet:

  Silence output

## Value

The desired plot

## Examples

``` r
# \donttest{

pheno_set %>%
  focus_qapply(backfill_iofv) %>%
  iofv_vs_mod()
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Tidying data by ID, TIME, AMT, WT, APGR ... and 17 more variables


pheno_set %>%
  focus_qapply(backfill_iofv) %>%
  iofv_vs_mod(run3,run11,run14,run15)
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Tidying data by ID, TIME, AMT, WT, APGR ... and 17 more variables


pheno_set %>%
  focus_qapply(backfill_iofv) %>%
  iofv_vs_mod(.lineage = TRUE)
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Tidying data by ID, TIME, AMT, WT, APGR ... and 17 more variables


# }
```
