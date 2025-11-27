# Select parameter row number

The selection rules are described in
\<[`add_prm_association`](https://jprybylski.github.io/xpose.xtras/reference/add_prm_association.md)\>.

## Usage

``` r
param_selector(sel, prm_tbl)
```

## Arguments

- sel:

  \<`character`\> Selector of any parameter

- prm_tbl:

  \<`tibble`\> Like the output of
  [`get_prm()`](https://jprybylski.github.io/xpose.xtras/reference/get_prm.md)

## Value

\<`integer`\> of selected row number. Can be used to get value and other
elements of information from `prm_tbl`.
