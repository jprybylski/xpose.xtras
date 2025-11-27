# Expose a property of xpdb objects in an xpose_set

Expose a property of xpdb objects in an xpose_set

## Usage

``` r
expose_property(xpdb_s, ..., .problem = NULL, .subprob = NULL, .method = NULL)
```

## Arguments

- xpdb_s:

  \<[`xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)\>
  An xpose_set object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  One or more properties to expose

- .problem:

  \<`numeric`\> Problem number to apply this relationship.

- .subprob:

  \<`numeric`\> Problem number to apply this relationship.

- .method:

  \<`numeric`\> Problem number to apply this relationship.

## Value

An `xpose_set` object with the properties exposed

## Details

The property returned will be top-level, and to avoid conflicting names
will be prepended by `..` (e.g., `..descr`).

For some properties, transformations are applied automatically to make
them more useful. This includes:

- `etashk` and `epsshk`: transformed to numeric vectors as in
  \<[`get_shk`](https://jprybylski.github.io/xpose.xtras/reference/get_shk.md)\>

- `ofv` and other per-problem properties: transformed as needed and
  pulls from each `xpdb` default problem.

## See also

[`expose_param()`](https://jprybylski.github.io/xpose.xtras/reference/expose_param.md)

## Examples

``` r
xpdb_set <- expose_property(xpdb_set, descr)
xpdb_set$mod1$..descr
#> [1] "NONMEM PK example for xpose"

xpdb_set <- expose_property(xpdb_set, etashk)
xpdb_set$mod1$..etashk
#> [1]  9.3 28.7 23.7
```
