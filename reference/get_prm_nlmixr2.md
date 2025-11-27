# get_prm equivalent for nlmixr2 fits

This is intended to match the
\<[`xpose::get_prm`](https://uupharmacometrics.github.io/xpose/reference/get_prm.html)\>
rather than the updated
[`get_prm()`](https://jprybylski.github.io/xpose.xtras/reference/get_prm.md).

## Usage

``` r
get_prm_nlmixr2(
  xpdb,
  transform = formals(get_prm)$transform,
  show_all = formals(get_prm)$show_all,
  quiet = FALSE
)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> With nlmixr2 fit

- transform:

  \<`logical`\> as in
  [`get_prm()`](https://jprybylski.github.io/xpose.xtras/reference/get_prm.md)

- show_all:

  \<`logical`\> as in
  [`get_prm()`](https://jprybylski.github.io/xpose.xtras/reference/get_prm.md)

- quiet:

  \<`logical`\> as in
  [`get_prm()`](https://jprybylski.github.io/xpose.xtras/reference/get_prm.md)

## Value

a tibble with expected columns
