# Individual ROC plots

To identify any individual likelihood predictions that may be more
influential or unusual.

Note this function may have a long runtime.

## Usage

``` r
ind_roc(
  xpdb,
  mapping = NULL,
  cutpoint = 1,
  type = "ca",
  title = "Individual ROC curves | @run",
  subtitle = "Ofv: @ofv, Eps shrink: @epsshk",
  caption = "@dir | Page @page of @lastpage",
  tag = NULL,
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

- facets:

  Additional facets

- .problem:

  Problem number

- quiet:

  Silence extra debugging output

- ...:

  Any additional aesthetics.

## Value

The desired plot

## Details

For type-based customization of plots:

- `c` ROC curve (using `geom_path`)

- `k` Key points on ROC curve (where on curve the threshold is
  `thres_fixed`) (using `geom_point`)

- `p` ROC space points (using `geom_point`)

- `t` ROC space text (using `geom_text`)

- `a` AUC in bottom right (using `geom_label`)

## Examples

``` r
if (FALSE) { # \dontrun{
vismo_pomod  %>%
  set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
  set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23) %>%
  ind_roc()
} # }
```
