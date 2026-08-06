# Changelog

## xpose.xtras 0.2.2

### New features

- Added
  [`xtras_data()`](https://jprybylski.github.io/xpose.xtras/reference/xtras_data.md),
  a wrapper around `xpose::xpose_data() %>% as_xp_xtras()` that silences
  the `rlang_warning`-batched chatter `readr`/`dplyr` can raise while
  parsing tables with `NaN`/`Inf` or otherwise oddly formatted values,
  without hiding warnings that indicate an actual read failure.
  [`xp_from_bbr()`](https://jprybylski.github.io/xpose.xtras/reference/xp_from_bbr.md)
  now uses it internally.
  ([\#77](https://github.com/jprybylski/xpose.xtras/issues/77))
- Added
  [`recalc_shk()`](https://jprybylski.github.io/xpose.xtras/reference/recalc_shk.md),
  which recalculates eta shrinkage directly from individual estimates
  rather than parsing NONMEM’s own reported value, with a `.etastype`
  argument to exclude “true zero” (as opposed to merely near-zero) etas
  from the calculation. Etas are matched to their omega by column name
  where possible (eg `nlmixr2` models), falling back to NONMEM’s
  `ETA<k>`/`OMEGA(k,k)` numbering convention.
  ([\#79](https://github.com/jprybylski/xpose.xtras/issues/79))
- Added a new `shk` variable type, plus
  [`derive_shk()`](https://jprybylski.github.io/xpose.xtras/reference/derive_shk.md)/[`backfill_shk()`](https://jprybylski.github.io/xpose.xtras/reference/derive_shk.md)
  to populate it: a per-individual eta shrinkage contribution diagnostic
  (`log((eta - mean(eta))^2)`).
  ([\#78](https://github.com/jprybylski/xpose.xtras/issues/78))
- Added
  [`shk_grid()`](https://jprybylski.github.io/xpose.xtras/reference/shk_grid_plots.md)/[`shk_vs_cov_grid()`](https://jprybylski.github.io/xpose.xtras/reference/shk_grid_plots.md)/[`shk_vs_contcov()`](https://jprybylski.github.io/xpose.xtras/reference/shk_vs_contcov.md)/
  [`shk_vs_catcov()`](https://jprybylski.github.io/xpose.xtras/reference/shk_vs_catcov.md),
  mirroring
  [`eta_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md)/[`eta_vs_cov_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md)/
  [`eta_vs_contcov()`](https://jprybylski.github.io/xpose.xtras/reference/eta_vs_contcov.md)/[`eta_vs_catcov()`](https://jprybylski.github.io/xpose.xtras/reference/eta_vs_catcov.md)
  for the new `shk` diagnostic.
  ([\#78](https://github.com/jprybylski/xpose.xtras/issues/78))
- Added `list = FALSE` to
  [`eta_vs_contcov()`](https://jprybylski.github.io/xpose.xtras/reference/eta_vs_contcov.md)/[`eta_vs_catcov()`](https://jprybylski.github.io/xpose.xtras/reference/eta_vs_catcov.md)/
  [`shk_vs_contcov()`](https://jprybylski.github.io/xpose.xtras/reference/shk_vs_contcov.md)/[`shk_vs_catcov()`](https://jprybylski.github.io/xpose.xtras/reference/shk_vs_catcov.md),
  combining the per-variable plots onto one shared, faceted plot instead
  of a list, paginating at 9 panels per page.
  ([\#82](https://github.com/jprybylski/xpose.xtras/issues/82))
- Added `covvar` to
  [`eta_vs_contcov()`](https://jprybylski.github.io/xpose.xtras/reference/eta_vs_contcov.md)/[`eta_vs_catcov()`](https://jprybylski.github.io/xpose.xtras/reference/eta_vs_catcov.md)/
  [`shk_vs_contcov()`](https://jprybylski.github.io/xpose.xtras/reference/shk_vs_contcov.md)/[`shk_vs_catcov()`](https://jprybylski.github.io/xpose.xtras/reference/shk_vs_catcov.md)
  to select specific covariates, mirroring `etavar`/`shkvar`; also added
  as a `cols` alias on
  [`eta_vs_cov_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md)/[`shk_vs_cov_grid()`](https://jprybylski.github.io/xpose.xtras/reference/shk_grid_plots.md).
  ([\#82](https://github.com/jprybylski/xpose.xtras/issues/82))
- Added
  [`normalize_etas()`](https://jprybylski.github.io/xpose.xtras/reference/normalize_etas.md)/[`normalise_etas()`](https://jprybylski.github.io/xpose.xtras/reference/normalize_etas.md),
  which set a top-level `xpdb$normalize_etas` slot (like `$covs`, see
  [`add_cov_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_cov_association.md)
  – not an `xpdb$options` entry, and not settable via
  [`set_option()`](https://jprybylski.github.io/xpose.xtras/reference/set_option.md))
  dividing each eta by its omega-implied SD (or, with `.use_sd = TRUE`,
  the empirical SD of its individual estimates) in
  [`eta_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md)/
  [`eta_vs_cov_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md)/[`eta_vs_contcov()`](https://jprybylski.github.io/xpose.xtras/reference/eta_vs_contcov.md)/[`eta_vs_catcov()`](https://jprybylski.github.io/xpose.xtras/reference/eta_vs_catcov.md)
  only – the underlying data is never modified.
  ([\#81](https://github.com/jprybylski/xpose.xtras/issues/81))

### Bug fixes

- [`nlmixr2_as_xtra()`](https://jprybylski.github.io/xpose.xtras/reference/nlmixr2_as_xtra.md)
  now reports the rejected object’s class when
  [`xpose.nlmixr2::xpose_data_nlmixr2()`](https://rdrr.io/pkg/xpose.nlmixr2/man/xpose_data_nlmixr2.html)
  doesn’t recognize it as an `nlmixr2` fit, instead of only the generic
  upstream message.
- [`backfill_iofv()`](https://jprybylski.github.io/xpose.xtras/reference/backfill_iofv.md)
  now gives a clear error when called without `xpdb` instead of a
  cryptic `cli` formatting failure.
  ([\#84](https://github.com/jprybylski/xpose.xtras/issues/84))
- Fixed
  [`print.xpose_data()`](https://jprybylski.github.io/xpose.xtras/reference/print.xpose_data.md)/`print.xp_xtras()`
  erroring instead of printing whenever any `xpdb$options` entry is a
  multi-element list (eg `default_labs`/`default_watermark` with more
  than one key set).
  ([\#81](https://github.com/jprybylski/xpose.xtras/issues/81))
- Fixed a critical bug where any
  `xpdb$foo <- value`/`xpdb[["foo"]] <- value` (including inside
  [`patch_condn()`](https://jprybylski.github.io/xpose.xtras/reference/patch_condn.md),
  run automatically by
  [`as_xpdb_x()`](https://jprybylski.github.io/xpose.xtras/reference/xp_xtras.md))
  could silently strip the `xp_xtras`/`xpose_data` classes on `ggplot2`
  versions before 4.0, due to a class-name collision with `ggplot2`’s
  internal [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html)
  mapping class.
  ([\#74](https://github.com/jprybylski/xpose.xtras/issues/74))
- `conflicted` is now an optional (Suggests) dependency instead of a
  hard requirement, and is only consulted if the user has loaded it
  themselves. Package startup no longer prints a “Registered S3 method
  overwritten” notice for `print.xpose_plot`, and the reminder to attach
  `xpose` now also names any load-order-sensitive bugfix at stake.
  ([\#72](https://github.com/jprybylski/xpose.xtras/issues/72))
- [`irep()`](https://jprybylski.github.io/xpose.xtras/reference/irep.md)
  is no longer deprecated in favor of
  [`xpose::irep()`](https://uupharmacometrics.github.io/xpose/reference/irep.html)
  for `xpose` \>= 0.5.0: `xpose` reverted that upstream fix, so this
  package’s version is needed again regardless of the installed `xpose`
  version.
- [`catdv_vs_ipred()`](https://jprybylski.github.io/xpose.xtras/reference/catdv_vs_ipred.md)
  no longer triggers a spurious `ggplot2` lifecycle warning (incorrectly
  blaming `xpose`) by mapping `size` to its line layer, where it’s
  deprecated in favor of `linewidth`.
  ([\#75](https://github.com/jprybylski/xpose.xtras/issues/75))
- [`set_var_types_x()`](https://jprybylski.github.io/xpose.xtras/reference/set_var_types_x.md)
  no longer mistypes columns when a `tidyselect` expression matches 10+
  columns for one type (e.g. `eta = matches(...)`), or when one type
  name is a prefix of another (e.g. `id`/`idv`).
  ([\#76](https://github.com/jprybylski/xpose.xtras/issues/76))

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
