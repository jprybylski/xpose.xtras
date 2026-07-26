# Apply default label overrides to a plot

Resolves `title`/`subtitle`/`caption`/`tag` labels for `plot` from (in
increasing precedence):

1.  the `xpose.xtras.default_labs` R option (a named list, see
    examples),

2.  defaults set on `xpdb` via
    [`set_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_labs.md),
    when `xpdb` is supplied (a rendered plot does not retain enough of
    its source `xpdb` to look this up automatically – pass it
    explicitly),

3.  values passed directly via `...`.

By default only labels not already set on the plot are filled in; set
`overwrite = TRUE` to replace existing labels too.
[print.xpose_plot()](https://jprybylski.github.io/xpose.xtras/reference/print.xpose_plot.md)
calls this automatically whenever `xpose.xtras.auto_apply` is enabled
(the default; see
[`set_xtras_options()`](https://jprybylski.github.io/xpose.xtras/reference/set_xtras_options.md)),
but `apply_default_labs()` itself is a standalone function that doesn't
depend on that print method existing, so it works the same regardless of
whether that method survives issue \#36's eventual removal.

## Usage

``` r
apply_default_labs(plot, ..., xpdb = NULL, overwrite = FALSE)
```

## Arguments

- plot:

  \<`ggplot`\> or \<`xpose_plot`\> object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Direct overrides for `title`/`subtitle`/`caption`/`tag`, taking
  precedence over both the option- and `xpdb`-level defaults

- xpdb:

  \<[`xpose_data`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  or \<`xp_xtras`\> object `plot` was built from, used to resolve
  `xpdb`-level defaults (see
  [`set_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_labs.md))
  and to resolve `@keyword` placeholders in any of the labels. If
  omitted and `plot` is an `xpose_plot`, `@keyword` placeholders are
  still resolved (using the reduced context xpose attaches to the plot)
  but `xpdb`-level defaults are not available

- overwrite:

  \<`logical`\> Replace labels already present on `plot` (default
  `FALSE`, meaning only missing labels are filled in)

## Value

`plot`, with resolved labels applied

## See also

[`set_xtras_options()`](https://jprybylski.github.io/xpose.xtras/reference/set_xtras_options.md)
for the full list of `xpose.xtras.*` session options, and
[`get_xtras_option()`](https://jprybylski.github.io/xpose.xtras/reference/get_xtras_option.md)
to check which tier (option/`xpdb`) is currently dominant for a given
`xpdb`.

## Examples

``` r
options(xpose.xtras.default_labs = list(caption = "Draft -- do not distribute"))
p <- xpose::dv_vs_ipred(xpose::xpdb_ex_pk)
#> Using data from $prob no.1
#> Filtering data by EVID == 0
apply_default_labs(p)
#> `geom_smooth()` using formula = 'y ~ x'

options(xpose.xtras.default_labs = NULL)
```
