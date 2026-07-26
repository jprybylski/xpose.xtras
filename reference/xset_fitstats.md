# Log-likelihood, AIC and BIC across an `xpose_set`

**\[experimental\]**

If no base model is provided, and if lineage is unclear, the first model
in the `xpose_set` is used as the base model, exactly as in
\<[`diff.xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/diff.xpose_set.md)\>.
Unlike [`diff()`](https://rdrr.io/r/base/diff.html), values are not
differenced, so a straightforward model-to-model comparison is possible.

As with the `xpose_data` methods, a component model that is itself a
model-averaged ("franken") object will cause an error.

## Usage

``` r
# S3 method for class 'xpose_set'
logLik(object, ...)

# S3 method for class 'xpose_set'
AIC(object, ..., k = 2)

# S3 method for class 'xpose_set'
BIC(object, ...)
```

## Arguments

- object:

  \<`xpose_set`\> object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Passed to
  \<[`xset_lineage`](https://jprybylski.github.io/xpose.xtras/reference/xset_lineage.md)\>.
  `.spinner=FALSE` can also be set here.

- k:

  \<`numeric`\> Penalty per parameter, as in
  \<[`stats::AIC`](https://rdrr.io/r/stats/AIC.html)\>.

## Value

\<`numeric`\> vector, or list thereof, following
\<[`xset_lineage`](https://jprybylski.github.io/xpose.xtras/reference/xset_lineage.md)\>.
