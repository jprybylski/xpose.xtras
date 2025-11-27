# Convenience function for ingesting an nlmixr2 model to xpose and xpose.xtras

A wrapper that executes the pipeline:

    obj |>
     xpose.nlmixr2::xpose_data_nlmixr2() |>
     attach_nlmixr2() |>
     as_xp_xtras() |>
     backfill_nlmixr2_props() 
     `if`(.skip_assoc, ., nlmixr2_prm_associations(.))

## Usage

``` r
nlmixr2_as_xtra(obj, ..., .skip_assoc = FALSE)
```

## Arguments

- obj:

  nlmixr2 fit object

- ...:

  Passed to
  [xpose_data_nlmixr2](https://rdrr.io/pkg/xpose.nlmixr2/man/xpose_data_nlmixr2.html)

- .skip_assoc:

  \<`logical`\> If the model is relatively uncomplicated,
  [`nlmixr2_prm_associations()`](https://jprybylski.github.io/xpose.xtras/reference/nlmixr2_prm_associations.md)
  may be able to recognize relationships between random effects and
  fixed effect parameters. If the default (`FALSE`) fails then try to
  rerun with the association step skipped.

## Value

An \<`xp_xtra`\> object with fit attached

## See also

[`attach_nlmixr2()`](https://jprybylski.github.io/xpose.xtras/reference/attach_nlmixr2.md)
