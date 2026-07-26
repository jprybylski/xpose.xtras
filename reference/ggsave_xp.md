# Save a plot with `xpose.xtras` default output resolution

A
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)-compatible
wrapper (defaulting to
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
itself, but swappable via `save_fun` – e.g. for
`reportifyr::ggsave_with_metadata()` or any other function sharing
`ggsave()`'s `plot`/`filename`/`path`/`width`/`height` signature).
Before saving: resolved labels are applied via
[`apply_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/apply_default_labs.md)
and a configured watermark (if any) via
[`add_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/add_watermark.md)
– see `apply_labs`/`apply_watermark`, both of which default to the
`xpose.xtras.auto_apply` option (`TRUE` unless changed), the same switch
[print.xpose_plot()](https://jprybylski.github.io/xpose.xtras/reference/print.xpose_plot.md)
uses, so a plot looks the same whether it's viewed interactively or
saved with `ggsave_xp()`. `filename`/`path` have any `@keyword`
placeholders expanded via
[`xpose::parse_title()`](https://uupharmacometrics.github.io/xpose/reference/parse_title.html),
the same way
[`xpose::xpose_save()`](https://uupharmacometrics.github.io/xpose/reference/xpose_save.html)
does (independent of which `save_fun` is used, since most save functions
don't do this themselves); and `path`/`width`/`height`/`save_fun` fall
back to the `xpose.xtras.save_dir`, `xpose.xtras.save_width`,
`xpose.xtras.save_height`, and `xpose.xtras.save_fun` R options when not
supplied explicitly, so a project can set output defaults once (see
[`set_xtras_options()`](https://jprybylski.github.io/xpose.xtras/reference/set_xtras_options.md)).

## Usage

``` r
ggsave_xp(
  plot = ggplot2::last_plot(),
  filename,
  path = getOption("xpose.xtras.save_dir"),
  width = getOption("xpose.xtras.save_width", 7),
  height = getOption("xpose.xtras.save_height", 6),
  xpdb = NULL,
  apply_labs = getOption("xpose.xtras.auto_apply", TRUE),
  apply_watermark = getOption("xpose.xtras.auto_apply", TRUE),
  save_fun = getOption("xpose.xtras.save_fun", ggplot2::ggsave),
  ...
)
```

## Arguments

- plot:

  \<`ggplot`\> or \<`xpose_plot`\> object

- filename:

  \<`character`\> File name, optionally with `@keyword` placeholders
  (e.g. `"@run_@plotfun.pdf"`, see
  [`xpose::parse_title()`](https://uupharmacometrics.github.io/xpose/reference/parse_title.html)).
  `@plotfun` (the name of the function that built `plot`, e.g.
  `"dv_vs_ipred"`) only resolves when `plot` is an `xpose_plot`, since
  it isn't something a bare `xpdb` knows about

- path:

  \<`character`\> Directory to save in; falls back to the
  `xpose.xtras.save_dir` R option

- width, height:

  \<`numeric`\> Plot size (in inches by default, see `save_fun`'s own
  `units` argument if it has one); fall back to the
  `xpose.xtras.save_width`/`xpose.xtras.save_height` R options

- xpdb:

  \<[`xpose_data`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  or \<`xp_xtras`\> object `plot` was built from, forwarded to
  [`apply_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/apply_default_labs.md)/
  [`add_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/add_watermark.md)
  (see their `xpdb` argument) and used to resolve `@keyword`
  placeholders

- apply_labs:

  \<`logical`\> Apply
  [`apply_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/apply_default_labs.md)
  to `plot` before saving; defaults to the `xpose.xtras.auto_apply`
  option (`TRUE` unless changed)

- apply_watermark:

  \<`logical`\> Apply
  [`add_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/add_watermark.md)
  to `plot` before saving, if a `default_watermark` is configured at
  some tier (see
  [`add_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/add_watermark.md))
  – otherwise a no-op regardless; defaults to the
  `xpose.xtras.auto_apply` option (`TRUE` unless changed)

- save_fun:

  \<`function`\> The actual save function to call, e.g.
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  (the default) or a drop-in alternative such as
  `reportifyr::ggsave_with_metadata()`; falls back to the
  `xpose.xtras.save_fun` R option, then
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)

- ...:

  Passed on to `save_fun` (e.g. `device`, `dpi`, `units`, `bg`)

## Value

the result of `save_fun` (for the default
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html),
the saved file path, invisibly)

## See also

[`set_xtras_options()`](https://jprybylski.github.io/xpose.xtras/reference/set_xtras_options.md)
for the full list of `xpose.xtras.*` session options.

## Examples

``` r
if (FALSE) { # \dontrun{
options(xpose.xtras.save_dir = "figures", xpose.xtras.save_width = 8)
p <- xpose::dv_vs_ipred(xpose::xpdb_ex_pk)
ggsave_xp(p, filename = "dv_vs_ipred.png")
} # }
```
