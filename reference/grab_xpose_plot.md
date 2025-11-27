# Grab processed `xpose_plot`

This function is very simple and unlikely to capture every possible
situation. Paginated plots are not supported.

This is helpful for working with `xpose` plots in `patchwork` or
`ggpubr` functions.

## Usage

``` r
grab_xpose_plot(plot)
```

## Arguments

- plot:

  \<`xpose_plot`\> or list thereof

## Value

Grob or list of grobs

## Examples

``` r
single_plot <- xpdb_x %>%
eta_vs_catcov(etavar = ETA1) %>%
grab_xpose_plot()
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Tidying data by ID, DOSE, AMT, SS, II ... and 23 more variables
#> Warning: attributes are not identical across measure variables; they will be dropped

listof_plots <- xpdb_x %>%
eta_vs_catcov(etavar = c(ETA1,ETA3)) %>%
grab_xpose_plot()
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Tidying data by ID, DOSE, AMT, SS, II ... and 23 more variables
#> Warning: attributes are not identical across measure variables; they will be dropped
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Tidying data by ID, DOSE, AMT, SS, II ... and 23 more variables
#> Warning: attributes are not identical across measure variables; they will be dropped
```
