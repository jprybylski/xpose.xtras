# Add a watermark to a plot

Overlays large, semi-transparent, rotated text across a `ggplot` or
`xpose_plot` object (e.g. `"DRAFT"`, `"PRELIMINARY"`, `"CONFIDENTIAL"`).
Calling it directly always adds a watermark (falling back to the
built-in defaults below if nothing else is configured). It is also
applied automatically by
[print.xpose_plot()](https://jprybylski.github.io/xpose.xtras/reference/print.xpose_plot.md)
and
[`ggsave_xp()`](https://jprybylski.github.io/xpose.xtras/reference/ggsave_xp.md)
whenever a `default_watermark` is actually configured (see the
`auto_apply` entry in
[`set_xtras_options()`](https://jprybylski.github.io/xpose.xtras/reference/set_xtras_options.md))
– unlike this function, that automatic trigger never invents an
unconfigured watermark on its own.

`label`/`colour`/`alpha`/`size`/`angle`/`fontface` resolve with the same
precedence as
[`apply_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/apply_default_labs.md):
(in increasing precedence) the `xpose.xtras.default_watermark` R option,
`xpdb`-level defaults set via
[`set_default_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_watermark.md)
(when `xpdb` is supplied), then the argument itself when explicitly
passed. Built-in fallbacks (`"DRAFT"`, `"grey50"`, `0.3`, `24`, `30`,
`"bold"`) apply if a setting isn't resolved from any of those.

## Usage

``` r
add_watermark(plot, label, colour, alpha, size, angle, fontface, xpdb = NULL)
```

## Arguments

- plot:

  \<`ggplot`\> or \<`xpose_plot`\> object

- label:

  \<`character`\> Watermark text

- colour:

  \<`character`\> Text colour, passed to
  [`grDevices::adjustcolor()`](https://rdrr.io/r/grDevices/adjustcolor.html)

- alpha:

  \<`numeric`\> Transparency of the watermark text, between 0
  (invisible) and 1 (opaque)

- size:

  \<`numeric`\> Font size in points

- angle:

  \<`numeric`\> Rotation angle in degrees (counter-clockwise)

- fontface:

  \<`character`\> Font face, passed to
  [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html)

- xpdb:

  \<[`xpose_data`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  or \<`xp_xtras`\> object `plot` was built from, used to resolve
  `xpdb`-level defaults (see
  [`set_default_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_watermark.md))

## Value

`plot`, with the watermark layer added

## See also

[`set_xtras_options()`](https://jprybylski.github.io/xpose.xtras/reference/set_xtras_options.md)
for the full list of `xpose.xtras.*` session options, and
[`get_xtras_option()`](https://jprybylski.github.io/xpose.xtras/reference/get_xtras_option.md)
to check which tier (option/`xpdb`) is currently dominant for a given
`xpdb`.

## Examples

``` r
p <- xpose::dv_vs_ipred(xpose::xpdb_ex_pk)
#> Using data from $prob no.1
#> Filtering data by EVID == 0
add_watermark(p, label = "PRELIMINARY")
#> `geom_smooth()` using formula = 'y ~ x'


options(xpose.xtras.default_watermark = list(label = "CONFIDENTIAL", colour = "red"))
add_watermark(p)
#> `geom_smooth()` using formula = 'y ~ x'

options(xpose.xtras.default_watermark = NULL)
```
