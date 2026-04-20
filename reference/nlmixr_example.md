# Generate example `xp_xtras` objects from nlmixr2 fits

Runs an nlmixr2 fit on demand and returns the result as a ready-to-use
`xp_xtras` object. `nlmixr2_example` is an alias.

## Usage

``` r
nlmixr_example(name)

nlmixr2_example(name)
```

## Source

`"xpdb_nlmixr2"` and `"xpdb_nlmixr2_saem"`:
<https://nlmixr2.org/articles/running_nlmixr.html>

`"nlmixr2_warfarin"`:
<https://nlmixr2.org/articles/multiple-endpoints.html>

`"nlmixr2_m3"`:
<https://github.com/nlmixr2/nlmixr2/issues/275#issuecomment-2445469327>

## Arguments

- name:

  \<`character`\> Name of the example to generate. One of
  `"xpdb_nlmixr2"`, `"xpdb_nlmixr2_saem"`, `"nlmixr2_warfarin"`,
  `"nlmixr2_m3"`.

## Value

An `xp_xtras` object with the nlmixr2 fit attached.

## Details

Available examples:

- `"xpdb_nlmixr2"`:

  One-compartment FOCEI fit to the theophylline dataset. The basic
  introductory example from the nlmixr2 documentation. See
  [Theoph](https://rdrr.io/r/datasets/Theoph.html).

- `"xpdb_nlmixr2_saem"`:

  The same one-compartment theophylline model fit with SAEM instead of
  FOCEI.

- `"nlmixr2_warfarin"`:

  Multiple-endpoint warfarin PK/PD model with a four-compartment PK and
  turnover PD. Provides categorical covariate data useful for eta
  diagnostic examples.

- `"nlmixr2_m3"`:

  Theophylline one-compartment model with censoring applied to provoke
  M3 likelihood handling. Includes a `BLQLIKE` output variable for use
  as a categorical DV example with
  [`catdv_vs_dvprobs()`](https://jprybylski.github.io/xpose.xtras/reference/catdv_vs_dvprobs.md).

## References

Fidler M (2025). *nlmixr2: Nonlinear Mixed Effects Models in Population
PK/PD*.
[doi:10.32614/CRAN.package.nlmixr2](https://doi.org/10.32614/CRAN.package.nlmixr2)
, R package version 3.0.2, <https://CRAN.R-project.org/package=nlmixr2>.

Fidler M, Wilkins J, Hooijmaijers R, Post T, Schoemaker R, Trame M,
Xiong Y, Wang W (2019). "Nonlinear Mixed-Effects Model Development and
Simulation Using nlmixr and Related R Open-Source Packages." *CPT:
Pharmacometrics & Systems Pharmacology*, *8*(9), 621-633.
[doi:10.1002/psp4.12445](https://doi.org/10.1002/psp4.12445) .

Schoemaker R, Fidler M, Laveille C, Wilkins J, Hooijmaijers R, Post T,
Trame M, Xiong Y, Wang W (2019). "Performance of the SAEM and FOCEI
Algorithms in the Open-Source, Nonlinear Mixed Effect Modeling Tool
nlmixr." *CPT: Pharmacometrics & Systems Pharmacology*, *8*(12),
923-930. [doi:10.1002/psp4.12471](https://doi.org/10.1002/psp4.12471) .

Beal, S.L. Ways to Fit a PK Model with Some Data Below the
Quantification Limit. J Pharmacokinet Pharmacodyn 28, 481-504 (2001).
[doi:10.1023/A:1012299115260](https://doi.org/10.1023/A%3A1012299115260)

## See also

[`nlmixr2_as_xtra()`](https://jprybylski.github.io/xpose.xtras/reference/nlmixr2_as_xtra.md),
[`catdv_vs_dvprobs()`](https://jprybylski.github.io/xpose.xtras/reference/catdv_vs_dvprobs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
xpdb_nlmixr2 <- nlmixr_example("xpdb_nlmixr2")

nlmixr2_m3 <- nlmixr_example("nlmixr2_m3")
nlmixr2_m3 %>%
  set_var_types(catdv = CENS, dvprobs = BLQLIKE) %>%
  set_dv_probs(1, 1 ~ BLQLIKE, .dv_var = CENS) %>%
  set_var_levels(1, CENS = lvl_bin()) %>%
  catdv_vs_dvprobs(xlab = "basic", quiet = TRUE)
} # }
```
