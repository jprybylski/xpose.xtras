# Typical processing for plots of 2 sets

Typical processing for plots of 2 sets

## Usage

``` r
two_set_dots(xpdb_s, ..., .inorder = FALSE, envir = parent.frame())
```

## Arguments

- xpdb_s:

  \<`xpose_set`\> object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Passed to
  \<[`select_subset`](https://jprybylski.github.io/xpose.xtras/reference/select_subset.md)\>

- .inorder:

  \<`logical`\> Regardless of base model or parentage, use the two plots
  in order of how they are in arguments. First plot listed is treated as
  base or parent.

- envir:

  Where to assign `mod1` and `mod2` \<`xpose_set_item`\>s

## Value

Into environment specified by `envir`, \<`xpose_set_item`\> `mod1` and
`mod2`

## Details

Note that this function does not return valid `xpdb`-like objects
(\<`xpose_data`\> or \<`xp_xtras`\>). The necessary objects for most
functions can be retrieved using `mod1$xpdb` and `mod2$xpdb`.
