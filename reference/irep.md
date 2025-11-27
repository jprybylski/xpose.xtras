# Add simulation counter

For `xpose` version \> 0.5.0 **\[deprecated\]**

Because this has been fixed in the parent package, the fix will be
removed in an upcoming release.

Add a column containing a simulation counter (irep). A new simulation is
counted every time a value in x is different than its previous value and
is a duplicate.

This version of the function does not require IDs be ascending, but does
not work for datasets where IDs are repeated (not in sequence). Both
cases are read as separate individuals for NONMEM, but NONMEM does not
need to detect repetition of ID sequences (for NONMEM,
`1,1,2,2,3,3,1,1,2,2,3,3` is 6 individuals, regardless of being 2
repeats of 3 individuals). Given the vast majority of datasets use 1
individual per ID, (which cannot be said about IDs always being
ascending), only one of these corrections is implemented.

## Usage

``` r
irep(x, quiet = FALSE)
```

## Arguments

- x:

  The column to be used for computing simulation number, usually the ID
  column.

- quiet:

  Logical, if `FALSE` messages are printed to the console.

## Value

`<numeric>` vector tracking the number of simulations based on unique
subject IDs.

## Details

Bugfix for
[`irep`](https://uupharmacometrics.github.io/xpose/reference/irep.html).

## Examples

``` r
data("xpdb_ex_pk", package = "xpose")

xpdb_ex_pk_2 <- xpdb_ex_pk %>%
 mutate(sim_id = irep(ID), .problem = 2)
#> irep: 20 simulations found.
```
