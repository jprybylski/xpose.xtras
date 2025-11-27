# Extra theme defaults

Adds aesthetics for plot components used in this package.

## Usage

``` r
xp_xtra_theme(base_on = NULL)
```

## Arguments

- base_on:

  `xp_theme` object to extend

## Value

An `xpose` theme object

## Details

This package attempts to generate a consistent theme even if users are
working with a highly customized `xp_theme`. There is are only a few
hard-coded aesthetics, and the rest are derived from existing aesthetics
in `base_on`, which defaults to the default from `xpose`.

Only a few options are worth noting. In
\<[`xplot_pairs`](https://jprybylski.github.io/xpose.xtras/reference/xplot_pairs.md)\>
(and functions using it), the aesthetics for `GGally`-specific elements
like `barDiag` are defined as `gga(element)_(aesthetic)`. The labeller
for pairs plots is also changed from the *de facto* default `label_both`
to `label_value`, but any labeller can be provided as `pairs_labeller`.
