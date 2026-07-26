# Set default watermark options on an `xp_xtras` object

Stores default
[`add_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/add_watermark.md)
arguments on `xpdb` that
[`add_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/add_watermark.md)
will use for any of `label`/`colour`/`alpha`/`size`/ `angle`/`fontface`
not otherwise supplied, when `xpdb` is passed to it.

## Usage

``` r
set_default_watermark(xpdb, ...)
```

## Arguments

- xpdb:

  \<[`xpose_data`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  or \<`xp_xtras`\> object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  One or more of `label`, `colour`, `alpha`, `size`, `angle`, `fontface`

## Value

`xp_xtras` object

## See also

[`set_xtras_options()`](https://jprybylski.github.io/xpose.xtras/reference/set_xtras_options.md)
for the session-option equivalent (`xpose.xtras.default_watermark`), and
[`get_xtras_option()`](https://jprybylski.github.io/xpose.xtras/reference/get_xtras_option.md)
to check which one is currently dominant.

## Examples

``` r
xpdb_x <- set_default_watermark(xpdb_x, label = "PRELIMINARY", colour = "red")
p <- xpose::dv_vs_ipred(xpdb_x)
#> Using data from $prob no.1
#> Filtering data by EVID == 0
add_watermark(p, xpdb = xpdb_x)
#> `geom_smooth()` using formula = 'y ~ x'
```
