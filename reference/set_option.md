# Set an `xpose` option

Sets one or more entries in `xpdb$options`, merged in via
[`utils::modifyList()`](https://rdrr.io/r/utils/modifyList.html) – which
recurses into list-valued options, so setting a single key of an
existing named-list option (e.g. one label of `default_labs`, see
[`set_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_labs.md))
leaves its other keys untouched rather than replacing the whole list.
This is what
[`set_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_labs.md)
and
[`set_default_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_watermark.md)
are built on, and calling `set_option()` directly with
`default_labs`/`default_watermark` behaves the same way.

## Usage

``` r
set_option(xpdb, ...)
```

## Arguments

- xpdb:

  \<`xpose_data`[xpose::xpose_data](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Arguments in the form of `option = value`

## Value

`xp_xtras` object

## Examples

``` r

xpdb_x <- set_option(xpdb_x, quiet = TRUE)
```
