# Change colors of shark plots

This changes the point and text color in the `xp_theme` of an
`xpose_data` object.

## Usage

``` r
shark_colors(
  xpdb,
  upcolor = xp_xtra_theme(base_on = xpdb$xp_theme)$sharkup_color,
  dncolor = xp_xtra_theme(base_on = xpdb$xp_theme)$sharkdn_color
)
```

## Arguments

- xpdb:

  \<`xpose_data`\> object

- upcolor:

  Color for increasing dOFV

- dncolor:

  Color for decreasing dOFV

## Value

\<`xpose_data`\> object

## See also

[`shark_plot()`](https://jprybylski.github.io/xpose.xtras/reference/shark_plot.md)

## Examples

``` r
# \donttest{

# Where this would fit in a particular workflow
xpose_set(pheno_base, pheno_final) %>%
  # forward functions affecting xpdb objects
  focus_xpdb(everything()) %>%
  # Add iOFVs
  focus_function(backfill_iofv) %>%
  # Change color of all xpdb xp_themes (though only the first one needs to change)
  focus_function(
  function(x) shark_colors(
      x,
      upcolor = "purple",
      dncolor = "green"
    )) %>%
  # See new plot
  shark_plot()
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Warning: Guessing df uses the difference in unfixed parameters. For these models, that
#> is 0. Using a value of 1. Adjust `df` and `alpha` to change singificant level.

# }
```
