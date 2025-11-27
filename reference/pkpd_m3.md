# An `xp_xtras` example of an M3 model

A representative PK/PD model with M3 fitting applied.

## Usage

``` r
pkpd_m3
```

## Format

### `xp_xtras`

An `xp_xtras` object.

## Source

[doi:10.1002/psp4.13219](https://doi.org/10.1002/psp4.13219)

## References

Beal, S.L. Ways to Fit a PK Model with Some Data Below the
Quantification Limit. J Pharmacokinet Pharmacodyn 28, 481-504 (2001).
[doi:10.1023/A:1012299115260](https://doi.org/10.1023/A%3A1012299115260)

Prybylski JP. Indirect modeling of derived outcomes: Are minor
prediction discrepancies a cause for concern? CPT Pharmacometrics Syst
Pharmacol. 2024; 00: 1-9.
[doi:10.1002/psp4.13219](https://doi.org/10.1002/psp4.13219)

## Examples

``` r
# To establish as a complete categorical DV example:
pkpd_m3 <- pkpd_m3 %>%
  # Need to ensure var types are set
  set_var_types(catdv=BLQ,dvprobs=LIKE) %>%
  # Set probs
  set_dv_probs(1, 1~LIKE, .dv_var = BLQ) %>%
  # Optional, but useful to set levels
  set_var_levels(1, BLQ = lvl_bin())

```
