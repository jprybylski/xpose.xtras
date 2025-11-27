# Based on associations baked into nlmixr2, automatically add to xpose data

This function attempts to discern the associations between omegas and
thetas using information about mu referencing within the `nlmixr2` fit
object.

## Usage

``` r
nlmixr2_prm_associations(xpdb, dry_run = FALSE, quiet)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> object

- dry_run:

  \<`logical`\> Return a resulting information to compare against.

- quiet:

  \<`logical`\> Include extra information

## Value

Object with filled `par`

## Details

Back-transformations are not as relevant here as they may seem. Manual
back-transformation with `backTransform()` only affects the display of
the back-transformed theta estimate (and CI), but does not impact the
relationship between EBEs and individual parameter estimates.

## See also

[`rxode2::ini()`](https://nlmixr2.github.io/rxode2/reference/ini.html)

## Examples

``` r
if (FALSE) { # \dontrun{
nlmixr2_warfarin %>%
  # This will add all log-normal and the logitnormal params
  nlmixr2_prm_associations() %>%
  # Make sure theta is in normal scale
  # rxode::expit could be plogis in this case
  mutate_prm(temax~rxode2::expit) %>%
  # Review results
  get_prm()

} # }

```
