# ROC Plot for categorical DVs

ROC Plot for categorical DVs

## Usage

``` r
roc_plot(
  xpdb,
  mapping = NULL,
  cutpoint = 1,
  group = "ID",
  type = "ca",
  title = "ROC curve @dvcol~@probcol | @run",
  subtitle = "Ofv: @ofv, Eps shrink: @epsshk",
  caption = "@dir",
  tag = NULL,
  guide = TRUE,
  facets,
  .problem,
  quiet,
  ...
)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> or \<`xpose_data`\> object

- mapping:

  `ggplot2` style mapping

- cutpoint:

  \<`numeric`\> Of defined probabilities, which one to use in plots.

- group:

  Variable by which to group points or text

- type:

  See Details.

- title:

  Plot title

- subtitle:

  Plot subtitle

- caption:

  Plot caption

- tag:

  Plot tag

- guide:

  Include unity line?

- facets:

  Additional facets

- .problem:

  Problem number

- quiet:

  Silence extra debugging output

- ...:

  Any additional aesthetics.

## Value

A desired plot

## Details

For type-based customization of plots:

- `c` ROC curve (using `geom_path`)

- `k` Key points on ROC curve (where on curve the threshold is
  `thres_fixed`) (using `geom_point`)

- `p` ROC space points (using `geom_point`)

- `t` ROC space text (using `geom_text`)

- `a` AUC in bottom right (using `geom_label`)

## See also

[`catdv_vs_dvprobs()`](https://jprybylski.github.io/xpose.xtras/reference/catdv_vs_dvprobs.md)

## Examples

``` r
# Note these examples are similar to catdv_vs_dvprobs

if (FALSE) { # \dontrun{
# Test M3 model
pkpd_m3 %>%
  # Need to ensure var types are set
  set_var_types(catdv=BLQ,dvprobs=LIKE) %>%
  # Set probs
  set_dv_probs(1, 1~LIKE, .dv_var = BLQ) %>%
  # Optional, but useful to set levels
  set_var_levels(1, BLQ = lvl_bin()) %>%
  # Generate typical ROC curve
  roc_plot()

# Test categorical model
vismo_xpdb <- vismo_pomod  %>%
  set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
  set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23)

# Various cutpoints (note axes labels and texts)
vismo_xpdb %>%
  roc_plot(type = "p") # space plot
vismo_xpdb %>%
  roc_plot(cutpoint=2, type = "cak") # with area and key point
vismo_xpdb %>%
  roc_plot(cutpoint=3, type = "cak")

# alternative model example
vismo_xpdb2  <- vismo_dtmm   %>%
  set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
  set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23)
vismo_xpdb2 %>%
  roc_plot(cutpoint=2, type = "cak")
} # }
```
