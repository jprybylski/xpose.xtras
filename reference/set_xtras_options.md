# Set `xpose.xtras` session options

Convenience wrapper around base
[`options()`](https://rdrr.io/r/base/options.html) for the
`xpose.xtras.*` family of options recognized by this package. Prefixes
are added automatically and names are validated against the list below,
so e.g. `set_xtras_options(save_dir = "figures")` is equivalent to (but
safer against typos than) `options(xpose.xtras.save_dir = "figures")`.
Current values can be read back with regular
[`getOption()`](https://rdrr.io/r/base/options.html) (e.g.
`getOption("xpose.xtras.save_dir")`), or with
[`get_xtras_option()`](https://jprybylski.github.io/xpose.xtras/reference/get_xtras_option.md)
for `default_labs`/`default_watermark`, which also considers any
`xpdb`-level default.

## Usage

``` r
set_xtras_options(...)
```

## Arguments

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  One or more of `default_labs`, `default_watermark`, `save_dir`,
  `save_width`, `save_height`, `save_fun`, `gg_theme`, `xp_theme`, given
  as `name = value`

## Value

the previous values of the options that were set, invisibly (see
[`options()`](https://rdrr.io/r/base/options.html))

## Details

Recognized options (all unset, i.e. `NULL`, by default, except
`auto_apply` which defaults to `TRUE`):

- `auto_apply`:

  Whether
  [print.xpose_plot()](https://jprybylski.github.io/xpose.xtras/reference/print.xpose_plot.md)
  and
  [`ggsave_xp()`](https://jprybylski.github.io/xpose.xtras/reference/ggsave_xp.md)
  automatically apply configured `default_labs`/ `default_watermark`
  (labels always; a watermark only if `default_watermark` is actually
  set at some tier – there's no unprompted default watermark). Defaults
  to `TRUE`, but is a no-op until `default_labs`/`default_watermark` are
  themselves configured, so leaving it at its default has no visible
  effect on its own; set it to `FALSE` to opt out of the auto-apply
  behavior everywhere at once (or pass `apply_labs`/`apply_watermark` to
  a specific
  [`ggsave_xp()`](https://jprybylski.github.io/xpose.xtras/reference/ggsave_xp.md)
  call to opt out just there).
  [`print.xpose_plot()`](https://jprybylski.github.io/xpose.xtras/reference/print.xpose_plot.md)
  only ever sees the session-wide option tier (not an `xpdb`-level one)
  for the same reason noted under `default_labs` below.

- `default_labs`:

  Named list of default `title`/`subtitle`/ `caption`/`tag` templates
  (may contain `@keyword` placeholders, see
  [`xpose::parse_title()`](https://uupharmacometrics.github.io/xpose/reference/parse_title.html)).
  Used by
  [`apply_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/apply_default_labs.md)
  (and by extension
  [`ggsave_xp()`](https://jprybylski.github.io/xpose.xtras/reference/ggsave_xp.md))
  as the lowest-precedence source for any label a plot doesn't already
  have – xpdb-level defaults (see
  [`set_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_labs.md))
  and arguments passed directly to
  [`apply_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/apply_default_labs.md)
  both take precedence over this option.

- `default_watermark`:

  Named list of default
  [`add_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/add_watermark.md)
  arguments (any of `label`/`colour`/`alpha`/`size`/`angle`/
  `fontface`). Used by
  [`add_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/add_watermark.md)
  as the lowest-precedence source – xpdb-level defaults (see
  [`set_default_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_watermark.md))
  and arguments passed directly to
  [`add_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/add_watermark.md)
  both take precedence over this option.

- `default_plots`:

  A list of plot specs (see
  [plot.xpose_data()](https://jprybylski.github.io/xpose.xtras/reference/plot.xpose_data.md))
  used whenever `plots` isn't supplied directly to
  [plot.xpose_data()](https://jprybylski.github.io/xpose.xtras/reference/plot.xpose_data.md).
  Unlike `default_labs`/ `default_watermark`, this is resolved as a
  whole (not merged key by key) – xpdb-level defaults (see
  [`set_default_plots()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_plots.md))
  take precedence over this option, and a `plots` argument passed
  directly takes precedence over both.

- `save_dir`, `save_width`, `save_height`:

  Defaults for the `path`/`width`/`height` arguments of
  [`ggsave_xp()`](https://jprybylski.github.io/xpose.xtras/reference/ggsave_xp.md),
  used whenever those arguments aren't supplied explicitly.

- `save_fun`:

  Default save function for
  [`ggsave_xp()`](https://jprybylski.github.io/xpose.xtras/reference/ggsave_xp.md)
  (itself defaulting to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  when this is unset), for swapping in a drop-in alternative such as
  `reportifyr::ggsave_with_metadata()` project-wide instead of passing
  `save_fun` to every
  [`ggsave_xp()`](https://jprybylski.github.io/xpose.xtras/reference/ggsave_xp.md)
  call.

- `gg_theme`, `xp_theme`:

  Default `ggplot2` theme / `xpose` `xp_theme` (see
  [`xpose::update_themes()`](https://uupharmacometrics.github.io/xpose/reference/update_themes.html))
  applied to an `xpose_data` object the first time it's converted via
  [`as_xpdb_x()`](https://jprybylski.github.io/xpose.xtras/reference/xp_xtras.md),
  so a project-wide look can be set once per session instead of calling
  [`xpose::update_themes()`](https://uupharmacometrics.github.io/xpose/reference/update_themes.html)
  on every `xpdb` individually. Has no effect on `xpdb`s that are
  already `xp_xtras` objects.

## See also

[`get_xtras_option()`](https://jprybylski.github.io/xpose.xtras/reference/get_xtras_option.md)
to inspect which tier is currently dominant for a two-tier option;
[`apply_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/apply_default_labs.md),
[`add_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/add_watermark.md),
[`ggsave_xp()`](https://jprybylski.github.io/xpose.xtras/reference/ggsave_xp.md),
and
[`as_xpdb_x()`](https://jprybylski.github.io/xpose.xtras/reference/xp_xtras.md),
which consume these options.

## Examples

``` r
set_xtras_options(save_dir = "figures", default_watermark = list(label = "DRAFT"))
getOption("xpose.xtras.save_dir")
#> [1] "figures"
set_xtras_options(save_dir = NULL, default_watermark = NULL)
```
