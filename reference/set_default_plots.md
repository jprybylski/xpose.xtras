# Set a default plot spec on an `xp_xtras` object

Stores a plot spec on `xpdb` that
[plot.xpose_data()](https://jprybylski.github.io/xpose.xtras/reference/plot.xpose_data.md)
uses instead of the package's built-in default whenever `plots` isn't
supplied directly to that call. Unlike
[`set_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_labs.md)/[`set_default_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_watermark.md),
this replaces the `xpdb`-level default wholesale rather than merging
with it: entries in a plot spec aren't addressable by a stable key (the
same plot function can legitimately appear more than once), so there is
no sensible key-by-key merge to perform.

## Usage

``` r
set_default_plots(xpdb, plots)
```

## Arguments

- xpdb:

  \<[`xpose_data`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  or \<`xp_xtras`\> object

- plots:

  A list of plot specs – see
  [plot.xpose_data()](https://jprybylski.github.io/xpose.xtras/reference/plot.xpose_data.md)
  for the accepted forms

## Value

`xp_xtras` object

## See also

[`set_xtras_options()`](https://jprybylski.github.io/xpose.xtras/reference/set_xtras_options.md)
for the session-option equivalent (`xpose.xtras.default_plots`), and
[`get_xtras_option()`](https://jprybylski.github.io/xpose.xtras/reference/get_xtras_option.md)
to check which one is currently dominant.

## Examples

``` r
xpdb2 <- set_default_plots(xpdb_x, list(~ xpose::dv_vs_ipred(.x), ~ xpose::eta_distrib(.x)))
names(plot(xpdb2, quiet = TRUE))
#> Using data from $prob no.1
#> Filtering data by EVID == 0
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Tidying data by ID, SEX, MED1, MED2, DOSE ... and 23 more variables
#> [1] "xpose::dv_vs_ipred" "xpose::eta_distrib"
```
