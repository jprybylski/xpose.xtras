# Check for potential parameterization issues

This function can help diagnose potential flip-flop or other issues
related to the parameterization of the model.

## Usage

``` r
diagnose_constants(
  xpdb,
  df = NULL,
  micro_pattern = "^K(\\d+|EL?)$",
  vol_pattern = "^V(C|D|1|2|)$",
  fo_abs = "KA",
  fo_rates = c("alpha_beta", "lambda", "custom"),
  checks = list(flip_flop = NULL, neg_microvol = NULL, units_match = NULL),
  df_units = NULL,
  .problem,
  quiet = xpdb$options$quiet
)
```

## Arguments

- xpdb:

  \<`xpose_data`\> object

- df:

  Optional \<`data.frame`\> of parameter values.

- micro_pattern:

  Regex. Pattern for microconstants

- vol_pattern:

  Regex. Pattern for volume parameter (should only match 1)

- fo_abs:

  First-order absorption parameter (singular, fixed, not regex).

- fo_rates:

  Derived ("macro") exponential rate constants (fixed). See Details

- checks:

  See Details

- df_units:

  Named list of units. If `NULL`, either ignore (`df`) or pull from
  `xpdb` object.

- .problem:

  Used in fetching parameters.

- quiet:

  Should parameter fetching produce output?

## Value

Nothing

## Details

The function prints output directly, not as an object.

A finding from these checks does not necessarily prove the
parameterization is erroneous (indeed, flip-flop PK can exist), but
coupled with other findings would help in diagnosing issues.

For `fo_rates`, `"alpha_beta"` and `"lambda"` are convenience
placeholders meaning literally `c("ALPHA","BETA","GAMMA")` and
`paste0("LAMBA",1:3)`, respectively. If capitalization or competing
names will be an issue, specify a custom set of names (provide a
character vector of names, do not pass `"custom"` to the argument). If
only a subset of `alpha_beta` or `lambda` are available, but these are
the parameterizations used (eg, only `ALPHA`) these options can still be
used. If `LAMBDA` is used alone, it will not match the `"lambda"`
default. If naming conventions are incompatible, it is suggested `xpdb`
or `df` be subject to mutation or renaming to use this function.

The available checks at this time are:

- `flip_flop` Checks if `fo_abs` are slower than the derived `fo_rates`.

- `neg_microvol` Checks if any microconstant or volume is negative. Note
  this check applies to parameterization of microconstants, so only a
  single volume (parameterizations with multiple volumes do not use
  microconstants) should match `vol_pattern`.

- `units_match` For any checks, verifies units are consistent. This
  check requires units are defined by
  [`set_var_units()`](https://uupharmacometrics.github.io/xpose/reference/set_vars.html)
  or `df_units` for parameters applicable to a requested check.

Checks must be requested as a named list of these elements, either
`TRUE` or `FALSE` (truth determines if the test is done). If the default
`NULL` is used, test will be run if the required parameters are present.

## See also

[`backfill_derived()`](https://jprybylski.github.io/xpose.xtras/reference/derive_prm.md)

## Examples

``` r
if (!rlang::is_installed("rxode2") ||
   !exists("rxDerived", envir = rlang::ns_env("rxode2"))) {
nlmixr2_m3 %>%
  backfill_derived() %>%
  diagnose_constants(vol_pattern = "^V$")

nlmixr2_m3 %>%
  backfill_derived() %>%
  diagnose_constants(
    vol_pattern = "^V$",
    df_units = list(KA = "1/hr", ALPHA = "1/hr"),
    checks = list(neg_microvol = FALSE)
  )

# Using df form
derive_prm(nlmixr2_m3) %>%
  diagnose_constants(df = ., vol_pattern = "^V$")

}
```
