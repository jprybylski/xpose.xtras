# Display deltaOFV values across `xpose_set`

If no base model is provided, and if lineage is unclear, the first model
in the `xpose_set` is used as the base model.

## Usage

``` r
# S3 method for class 'xpose_set'
diff(x, ...)
```

## Arguments

- x:

  \<`xpose_set`\> object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Passed to
  \<[`xset_lineage`](https://jprybylski.github.io/xpose.xtras/reference/xset_lineage.md)\>.
  `.spinner=FALSE` can also be set here.

## Value

\<`numeric`\> vector of deltaOFV values
