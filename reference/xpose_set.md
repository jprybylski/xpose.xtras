# Generate a set of `xpdb` objects

This function generates a set of xpose data (`xpdb`) objects that can be
used to define relationships between models. The

## Usage

``` r
xpose_set(..., .relationships = NULL, .as_ordered = FALSE)
```

## Arguments

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  `xpdb1, xpdb2, ...` A set of `xpdb` objects to be combined into a set.

- .relationships:

  \<[`list`](https://rdrr.io/r/base/list.html)\> A list of relationships
  between the `xpdb` objects. (see Details)

- .as_ordered:

  \<[`logical`](https://rdrr.io/r/base/logical.html)\> Alternative to
  `.relationships`, should the set of `xpdb` objects provided be
  considered a lineage (`grandparent, parent, child, ...`)?

## Value

A list of class `xpose_set`

## Details

Beyond just a list of `xpdb` objects, an `xpose_set` adds hierarchical
information.

When using `.relationships`, these should be expressed as tilde
formulas, where the left-hand side is children and the right and side is
parents. In the simplest case, this would be `child ~ parent`, but a
child can have multiple parents. This syntax expects that the names for
models is either declared as argument names in the call, or that the
variable names are directly used (i.e., not spliced or passed as an
unnamed list).

## Examples

``` r
data("xpdb_ex_pk", package = "xpose")

# Arbitrary copy
xpdb_ex_pk2 <- xpdb_ex_pk

# Simplest call
set1 <- xpose_set(xpdb_ex_pk, xpdb_ex_pk2)

# With predefined relationships
set2 <- xpose_set(xpdb_ex_pk, xpdb_ex_pk2,
  .relationships = list(xpdb_ex_pk2 ~ xpdb_ex_pk)
  )

# Alternative predefined relationships
set2b <- xpose_set(xpdb_ex_pk, xpdb_ex_pk2,
  .as_ordered = TRUE
  )

# With custom labels
set3 <- xpose_set(mod1 = xpdb_ex_pk, mod2 = xpdb_ex_pk2,
  .relationships = list(mod2 ~ mod1)
  )

# Alternative set3 using dyanmic dots
mod_list <- list(
  mod1 = xpdb_ex_pk,
  mod2 = xpdb_ex_pk2
)
mod_rels <- list(
  mod2 ~ mod1
)
set3b = xpose_set(!!!mod_list, .relationships = mod_rels)
```
