# Inspect which `xpose.xtras` option value is dominant

For the two "two-tier" options – `default_labs` and `default_watermark`,
which can be set both as a session-wide R option (via
[`set_xtras_options()`](https://jprybylski.github.io/xpose.xtras/reference/set_xtras_options.md))
and per-`xpdb` (via
[`set_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_labs.md)
/
[`set_default_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_watermark.md))
– reports the value at each tier and which one is dominant, i.e. would
currently be used by
[`apply_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/apply_default_labs.md)
/
[`add_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/add_watermark.md)
absent any argument passed directly to those functions (which always
takes precedence over both tiers).

The other options recognized by
[`set_xtras_options()`](https://jprybylski.github.io/xpose.xtras/reference/set_xtras_options.md)
(`save_dir`, `save_width`, `save_height`, `gg_theme`, `xp_theme`) have
no `xpdb`-level tier to compare against (see the Details in
[`set_xtras_options()`](https://jprybylski.github.io/xpose.xtras/reference/set_xtras_options.md)),
so for those `xpdb` is always `NULL` and `dominant` is `"option"` or
`"neither"`.

## Usage

``` r
get_xtras_option(name, xpdb = NULL)
```

## Arguments

- name:

  \<`character`\> One of the option names recognized by
  [`set_xtras_options()`](https://jprybylski.github.io/xpose.xtras/reference/set_xtras_options.md)

- xpdb:

  \<[`xpose_data`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  or \<`xp_xtras`\> object to check for an `xpdb`-level value (optional;
  only consulted for `default_labs`/`default_watermark`)

## Value

a list with elements `option` (the
[`getOption()`](https://rdrr.io/r/base/options.html) value), `xpdb` (the
`xpdb`-level value, or `NULL`), and `dominant` (one of `"option"`,
`"xpdb"`, or `"neither"`)

## Examples

``` r
options(xpose.xtras.default_labs = list(caption = "session default"))
xpdb_x2 <- set_default_labs(xpdb_x, caption = "model-specific")
get_xtras_option("default_labs", xpdb_x2)
#> $option
#> $option$caption
#> [1] "session default"
#> 
#> 
#> $xpdb
#> $xpdb$caption
#> [1] "model-specific"
#> 
#> 
#> $dominant
#> [1] "xpdb"
#> 
options(xpose.xtras.default_labs = NULL)
```
