# Patch condition number extraction

**\[experimental\]**

Bugfix for `xpose:::sum_condn`, the internal function `xpose` uses to
populate the `'condn'` (condition number) entry of `xpdb$summary`.

For NONMEM runs with more than one estimation method (e.g. `SAEM`
followed by importance sampling), the `.lst` file contains more than one
`EIGENVALUES OF COR MATRIX OF ESTIMATE` block. `xpose` always uses the
*first* block found, which is not necessarily from the final estimation
method, so the reported condition number can be wrong. This patch
instead uses the *last* block, matching the value reported by
NONMEM-adjacent tools such as PsN's `sumo`.

## Usage

``` r
patch_condn(xpdb)
```

## Arguments

- xpdb:

  An `xpose_data` or `xp_xtras` object.

## Value

The `xpdb` object, with a corrected `'condn'` entry in `xpdb$summary`
(unchanged if `xpdb` is not from `nonmem`, or if no eigenvalues could be
found).

## Examples

``` r
xpdb_ex_pk <- patch_condn(xpose::xpdb_ex_pk)
```
