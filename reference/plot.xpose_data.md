# Generate a batch of diagnostic plots from an `xpdb`

Runs a list of plot-generating calls against `x` in one go, returning a
flat, named list of the results. Each element of `plots` (or the
resolved default, see Details) is either:

- a bare function taking a single `xpdb` argument, or

- a one-sided formula in the `~ fn(.x)` idiom used elsewhere in this
  package (see
  [`focus_function()`](https://jprybylski.github.io/xpose.xtras/reference/focus_xpdb.md)),
  letting extra arguments be baked straight into the call – e.g.
  `~ xpose::res_vs_idv(.x, res = "CWRES")`.

Both forms are converted to callables with
[`purrr::as_mapper()`](https://purrr.tidyverse.org/reference/as_mapper.html).
Because entries are just calls, the same underlying plot function can
appear more than once with different options (e.g. `res_vs_idv` for both
CWRES and IWRES) – see the examples.

If a single entry itself returns a list of plots (as e.g.
[`eta_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md)
can, given more than one eta), that list is flattened into the overall
output rather than kept as a nested list – so the return value is always
a flat list of `ggplot`/`xpose_plot` objects.

## Usage

``` r
# S3 method for class 'xpose_data'
plot(x, y, plots, ..., force = FALSE, quiet)
```

## Arguments

- x:

  \<[`xpose_data`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  or \<`xp_xtras`\> object

- y:

  unused; present only for consistency with the
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html)
  generic

- plots:

  A list of plot specs (see Description); defaults to the resolved value
  described in Details

- ...:

  unused

- force:

  \<`logical`\> If `FALSE` (the default), a failing plot immediately
  aborts the whole call. If `TRUE`, a failing plot is instead skipped
  (with a warning), and the rest of `plots` is still attempted.

- quiet:

  \<`logical`\> Silence the loading spinner and the summary warning
  issued when `force = TRUE` and at least one plot failed; defaults to
  `x$options$quiet`

## Value

A flat, named list of `ggplot`/`xpose_plot` objects

## Details

`plots` is resolved, in increasing precedence, from: this package's
built-in default (`dv_vs_ipred`, `dv_vs_pred`, `res_vs_idv`,
`res_vs_pred`, `eta_distrib`, `eta_grid`, `eta_vs_cov_grid`,
`ind_plots_sample`), the `xpose.xtras.default_plots` session option, an
`xpdb`-level default set via
[`set_default_plots()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_plots.md),
and finally the `plots` argument itself, if supplied.

A loading spinner is shown (interactive sessions only, unless
`quiet = TRUE`) while plots are generated. If a plot fails to generate,
the default (`force = FALSE`) is to immediately raise an error (showing
the original error as its parent) without returning any plots at all.
With `force = TRUE`, a failure is instead emitted as a warning and that
entry is skipped, so the rest of `plots` still gets a chance to run.

## Examples

``` r
# \donttest{
# the package's built-in default battery of diagnostic plots
default_plots <- plot(xpdb_x, quiet = TRUE)
#> Using data from $prob no.1
#> Filtering data by EVID == 0
#> Using data from $prob no.1
#> Filtering data by EVID == 0
#> Using data from $prob no.1
#> Filtering data by EVID == 0
#> Using data from $prob no.1
#> Filtering data by EVID == 0
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Tidying data by ID, SEX, MED1, MED2, DOSE ... and 23 more variables
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Filtering data by EVID == 0
#> Tidying data by ID, SEX, MED1, MED2, DOSE ... and 23 more variables
names(default_plots)
#> [1] "dv_vs_ipred"      "dv_vs_pred"       "res_vs_idv"       "res_vs_pred"     
#> [5] "eta_distrib"      "eta_grid"         "eta_vs_cov_grid"  "ind_plots_sample"

# a custom spec: bare functions, formulas, and the same function twice
custom_plots <- plot(
  xpdb_x,
  plots = list(
    xpose::dv_vs_ipred,
    ~ xpose::res_vs_idv(.x, res = "CWRES"),
    ~ xpose::res_vs_idv(.x, res = "IWRES")
  ),
  quiet = TRUE
)
#> Using data from $prob no.1
#> Filtering data by EVID == 0
#> Using data from $prob no.1
#> Filtering data by EVID == 0
#> Using data from $prob no.1
#> Filtering data by EVID == 0
names(custom_plots)
#> [1] "plot_1"                "xpose::res_vs_idv...2" "xpose::res_vs_idv...3"
# }
```
