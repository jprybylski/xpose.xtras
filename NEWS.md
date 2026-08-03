# xpose.xtras

# xpose.xtras (development version)

## New features

* Added `xtras_data()`, a wrapper around `xpose::xpose_data() %>%
  as_xp_xtras()` that silences the `rlang_warning`-batched chatter
  `readr`/`dplyr` can raise while parsing tables with `NaN`/`Inf` or
  otherwise oddly formatted values, without hiding warnings that indicate
  an actual read failure. `xp_from_bbr()` now uses it internally. (#77)
* Added `recalc_shk()`, which recalculates eta shrinkage directly from
  individual estimates rather than parsing NONMEM's own reported value,
  with a `.etastype` argument to exclude "true zero" (as opposed to
  merely near-zero) etas from the calculation. Etas are matched to their
  omega by column name where possible (eg `nlmixr2` models), falling back
  to NONMEM's `ETA<k>`/`OMEGA(k,k)` numbering convention. (#79)
* Added a new `shk` variable type, plus `derive_shk()`/`backfill_shk()` to
  populate it: a per-individual eta shrinkage contribution diagnostic
  (`log((eta - mean(eta))^2)`). (#78)
* Added `shk_grid()`/`shk_vs_cov_grid()`/`shk_vs_contcov()`/
  `shk_vs_catcov()`, mirroring `eta_grid()`/`eta_vs_cov_grid()`/
  `eta_vs_contcov()`/`eta_vs_catcov()` for the new `shk` diagnostic. (#78)
* Added `list = FALSE` to `eta_vs_contcov()`/`eta_vs_catcov()`/
  `shk_vs_contcov()`/`shk_vs_catcov()`, combining the per-variable plots
  onto one shared, faceted plot instead of a list, paginating at 9 panels
  per page. (#82)
* Added `covvar` to `eta_vs_contcov()`/`eta_vs_catcov()`/
  `shk_vs_contcov()`/`shk_vs_catcov()` to select specific covariates,
  mirroring `etavar`/`shkvar`; also added as a `cols` alias on
  `eta_vs_cov_grid()`/`shk_vs_cov_grid()`. (#82)

## Bug fixes

* Fixed a critical bug where any `xpdb$foo <- value`/`xpdb[["foo"]] <- value`
  (including inside `patch_condn()`, run automatically by `as_xpdb_x()`)
  could silently strip the `xp_xtras`/`xpose_data` classes on `ggplot2`
  versions before 4.0, due to a class-name collision with `ggplot2`'s
  internal `aes()` mapping class. (#74)
* `conflicted` is now an optional (Suggests) dependency instead of a hard
  requirement, and is only consulted if the user has loaded it themselves.
  Package startup no longer prints a "Registered S3 method overwritten"
  notice for `print.xpose_plot`, and the reminder to attach `xpose` now
  also names any load-order-sensitive bugfix at stake. (#72)
* `irep()` is no longer deprecated in favor of `xpose::irep()` for
  `xpose` >= 0.5.0: `xpose` reverted that upstream fix, so this package's
  version is needed again regardless of the installed `xpose` version.
* `catdv_vs_ipred()` no longer triggers a spurious `ggplot2` lifecycle
  warning (incorrectly blaming `xpose`) by mapping `size` to its line
  layer, where it's deprecated in favor of `linewidth`. (#75)
* `set_var_types_x()` no longer mistypes columns when a `tidyselect`
  expression matches 10+ columns for one type (e.g. `eta = matches(...)`),
  or when one type name is a prefix of another (e.g. `id`/`idv`). (#76)

# xpose.xtras 0.2.0

## New features

* `xp_from_bbr()` converts a 'bbr' model directly into an `xp_xtras` object.
* New parameter/covariate association system (`add_cov_association()`,
  `prm_cov()` family) with `xplot_forest()` / `cov_forest()` visualization;
  see the new parameter-associations vignette.
* `cormat()` visualizes the parameter correlation/covariance matrix, for
  both NONMEM and nlmixr2 models.
* `catdv_vs_occ()` and `catdv_vs_ipred()` add categorical DV diagnostic
  plots (observed-vs-predicted trend across occasions, and binned
  calibration).
* `left_join_x()` backfills missing variables into an xpdb via a
  coalescing left join.
* Default label overrides, plot watermarking, and a flexible save wrapper
  (`set_default_labs()`, `add_watermark()`, `ggsave_xp()`), configurable via
  `set_xtras_options()` and auto-applied on print/save. See the new
  plot-output vignette.
* `patch_condn()` corrects 'xpose'`s condition number calculation for
  multi-method NONMEM runs, applied automatically by `as_xpdb_x()`.
* `focus_function()` / `focus_qapply()` now support output-generating
  functions (plots, tables), not just xpdb-transforming ones.
* New `logLik()`, `AIC()`, and `BIC()` methods for `xpose_data` and
  `xpose_set` objects.
* `ind_plots_sample()` plots a representative (optionally stratified)
  sample of individuals instead of the whole dataset.
* `plot.xpose_data()` runs a configurable batch of diagnostic plots in one
  call.
* `process_preset()` family saves and replays a reusable xpdb processing
  pipeline by name, optionally persisted across sessions.

## Bug fixes

* `set_dv_probs()` no longer crashes when `.problem` is omitted.
* `conflicted` preferences are now re-applied regardless of package attach
  order, fixing spurious "found in 2 packages" errors.
* Fixed inconsistent 'cli' message coloring caused by a color-formatting
  helper being evaluated at install time instead of call time.
* Fixed a title/description typo in `%p%`.
* Fixed misleading default subtitle/caption labels on model-averaged
  plots, which previously reported statistics from just one of the
  averaged models.
* Fixed `as_xpdb_x()` / `check_xpdb_x()` erroring on `xpose_data` objects
  with no table data.
* Fixed `xplot_pairs()` silently ignoring a custom
  `catcont_opts$other_fun`, and removed an unreachable leftover
  duplicate-axis-label block in `roc_by_mod()`.

# xpose.xtras 0.1.4
* nlmixr2 example datasets replaced with on-demand `nlmixr_example()` /
  `nlmixr2_example()` to avoid loading nlmixr2est when not installed.

# xpose.xtras 0.1.3
* Fix for nlmixr2 compatibility checks when nlmixr2est is not installed.

# xpose.xtras 0.1.2
* Additional changes relevant to 0.1.1.

# xpose.xtras 0.1.1
* Minor compatibility changes for nlmixr2 5.0

# xpose.xtras 0.1.0
* Bugs regarding covariate plotting functions were resolved.
* nlmixr2 compatibility implemented
* ROC curve plots added
* Updated documentation and pkgdown site

# xpose.xtras 0.0.3

* Patch to support 'ggplot2' 4.0.0 release.
* Bugfixes for 'xpose' related to 'ggplot2' also implemented
in a backwards compatible way.
* Contact email changed.

# xpose.xtras 0.0.2

* CRAN release planned

# xpose.xtras 0.0.1

* Early post-development version
* Completed unit tests and ongoing fixes to bugs and documentation.
