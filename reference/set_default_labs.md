# Set default plot label overrides on an `xp_xtras` object

Stores `title`/`subtitle`/`caption`/`tag` templates on `xpdb` that
[`apply_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/apply_default_labs.md)
will use to fill in (or overwrite) labels on any plot built from this
`xpdb`, when `xpdb` is passed to it. Values may contain the same
`@keyword` placeholders understood by
[`xpose::parse_title()`](https://uupharmacometrics.github.io/xpose/reference/parse_title.html)
(e.g. `"@nind"`, `"@nobs"`, `"@run"`), since they are resolved the same
way.

## Usage

``` r
set_default_labs(xpdb, ...)
```

## Arguments

- xpdb:

  \<[`xpose_data`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  or \<`xp_xtras`\> object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  One or more of `title`, `subtitle`, `caption`, `tag`, given as
  character strings

## Value

`xp_xtras` object

## See also

[`set_xtras_options()`](https://jprybylski.github.io/xpose.xtras/reference/set_xtras_options.md)
for the session-option equivalent (`xpose.xtras.default_labs`), and
[`get_xtras_option()`](https://jprybylski.github.io/xpose.xtras/reference/get_xtras_option.md)
to check which one is currently dominant.

## Examples

``` r
xpdb_x <- set_default_labs(xpdb_x, caption = "@nobs observations in @nind individuals")
p <- xpose::dv_vs_ipred(xpdb_x)
#> Using data from $prob no.1
#> Filtering data by EVID == 0
apply_default_labs(p, xpdb = xpdb_x)
#> `geom_smooth()` using formula = 'y ~ x'
```
