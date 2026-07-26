# Changelog

## xpose.xtras 0.2.0

CRAN release: 2026-07-26

### New features

- [`xp_from_bbr()`](https://jprybylski.github.io/xpose.xtras/reference/xp_from_bbr.md)
  converts a ‘bbr’ model directly into an `xp_xtras` object.
- New parameter/covariate association system
  ([`add_cov_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_cov_association.md),
  [`prm_cov()`](https://jprybylski.github.io/xpose.xtras/reference/prm_cov.md)
  family) with
  [`xplot_forest()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_forest.md)
  /
  [`cov_forest()`](https://jprybylski.github.io/xpose.xtras/reference/cov_forest.md)
  visualization; see the new parameter-associations vignette.
- [`cormat()`](https://jprybylski.github.io/xpose.xtras/reference/cormat.md)
  visualizes the parameter correlation/covariance matrix, for both
  NONMEM and nlmixr2 models.
- [`catdv_vs_occ()`](https://jprybylski.github.io/xpose.xtras/reference/catdv_vs_occ.md)
  and
  [`catdv_vs_ipred()`](https://jprybylski.github.io/xpose.xtras/reference/catdv_vs_ipred.md)
  add categorical DV diagnostic plots (observed-vs-predicted trend
  across occasions, and binned calibration).
- [`left_join_x()`](https://jprybylski.github.io/xpose.xtras/reference/left_join_x.md)
  backfills missing variables into an xpdb via a coalescing left join.
- Default label overrides, plot watermarking, and a flexible save
  wrapper
  ([`set_default_labs()`](https://jprybylski.github.io/xpose.xtras/reference/set_default_labs.md),
  [`add_watermark()`](https://jprybylski.github.io/xpose.xtras/reference/add_watermark.md),
  [`ggsave_xp()`](https://jprybylski.github.io/xpose.xtras/reference/ggsave_xp.md)),
  configurable via
  [`set_xtras_options()`](https://jprybylski.github.io/xpose.xtras/reference/set_xtras_options.md)
  and auto-applied on print/save. See the new plot-output vignette.
- [`patch_condn()`](https://jprybylski.github.io/xpose.xtras/reference/patch_condn.md)
  corrects
  ‘xpose’`s condition number calculation for multi-method NONMEM runs, applied automatically by`as_xpdb_x()\`.
- [`focus_function()`](https://jprybylski.github.io/xpose.xtras/reference/focus_xpdb.md)
  /
  [`focus_qapply()`](https://jprybylski.github.io/xpose.xtras/reference/focus_xpdb.md)
  now support output-generating functions (plots, tables), not just
  xpdb-transforming ones.
- New [`logLik()`](https://rdrr.io/r/stats/logLik.html),
  [`AIC()`](https://rdrr.io/r/stats/AIC.html), and
  [`BIC()`](https://rdrr.io/r/stats/AIC.html) methods for `xpose_data`
  and `xpose_set` objects.
- [`ind_plots_sample()`](https://jprybylski.github.io/xpose.xtras/reference/ind_plots_sample.md)
  plots a representative (optionally stratified) sample of individuals
  instead of the whole dataset.
- [`plot.xpose_data()`](https://jprybylski.github.io/xpose.xtras/reference/plot.xpose_data.md)
  runs a configurable batch of diagnostic plots in one call.
- [`process_preset()`](https://jprybylski.github.io/xpose.xtras/reference/add_process_preset.md)
  family saves and replays a reusable xpdb processing pipeline by name,
  optionally persisted across sessions.

### Bug fixes

- [`set_dv_probs()`](https://jprybylski.github.io/xpose.xtras/reference/set_dv_probs.md)
  no longer crashes when `.problem` is omitted.
- `conflicted` preferences are now re-applied regardless of package
  attach order, fixing spurious “found in 2 packages” errors.
- Fixed inconsistent ‘cli’ message coloring caused by a color-formatting
  helper being evaluated at install time instead of call time.
- Fixed a title/description typo in `%p%`.
- Fixed misleading default subtitle/caption labels on model-averaged
  plots, which previously reported statistics from just one of the
  averaged models.
- Fixed
  [`as_xpdb_x()`](https://jprybylski.github.io/xpose.xtras/reference/xp_xtras.md)
  /
  [`check_xpdb_x()`](https://jprybylski.github.io/xpose.xtras/reference/xp_xtras.md)
  erroring on `xpose_data` objects with no table data.
- Fixed
  [`xplot_pairs()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_pairs.md)
  silently ignoring a custom `catcont_opts$other_fun`, and removed an
  unreachable leftover duplicate-axis-label block in
  [`roc_by_mod()`](https://jprybylski.github.io/xpose.xtras/reference/roc_by_mod.md).

## xpose.xtras 0.1.4

CRAN release: 2026-04-21

- nlmixr2 example datasets replaced with on-demand
  [`nlmixr_example()`](https://jprybylski.github.io/xpose.xtras/reference/nlmixr_example.md)
  /
  [`nlmixr2_example()`](https://jprybylski.github.io/xpose.xtras/reference/nlmixr_example.md)
  to avoid loading nlmixr2est when not installed.

## xpose.xtras 0.1.3

CRAN release: 2026-04-19

- Fix for nlmixr2 compatibility checks when nlmixr2est is not installed.

## xpose.xtras 0.1.2

CRAN release: 2025-12-01

- Additional changes relevant to 0.1.1.

## xpose.xtras 0.1.1

CRAN release: 2025-11-30

- Minor compatibility changes for nlmixr2 5.0

## xpose.xtras 0.1.0

CRAN release: 2025-09-09

- Bugs regarding covariate plotting functions were resolved.
- nlmixr2 compatibility implemented
- ROC curve plots added
- Updated documentation and pkgdown site

## xpose.xtras 0.0.3

CRAN release: 2025-08-23

- Patch to support ‘ggplot2’ 4.0.0 release.
- Bugfixes for ‘xpose’ related to ‘ggplot2’ also implemented in a
  backwards compatible way.
- Contact email changed.

## xpose.xtras 0.0.2

CRAN release: 2024-11-21

- CRAN release planned

## xpose.xtras 0.0.1

- Early post-development version
- Completed unit tests and ongoing fixes to bugs and documentation.
