# Package index

## xp_xtras objects

Functions for creating, checking and manipulating `xp_xtras` objects.

- [`as_xpdb_x()`](https://jprybylski.github.io/xpose.xtras/reference/xp_xtras.md)
  [`as_xp_xtras()`](https://jprybylski.github.io/xpose.xtras/reference/xp_xtras.md)
  [`check_xpdb_x()`](https://jprybylski.github.io/xpose.xtras/reference/xp_xtras.md)
  [`check_xp_xtras()`](https://jprybylski.github.io/xpose.xtras/reference/xp_xtras.md)
  :

  Convert an object to an `xpose_data` and `xp_xtras` object

- [`check_xpose_set()`](https://jprybylski.github.io/xpose.xtras/reference/check_xpose_set.md)
  [`check_xpose_set_item()`](https://jprybylski.github.io/xpose.xtras/reference/check_xpose_set.md)
  :

  Check an `xpose_set` object

- [`is_xp_xtras()`](https://jprybylski.github.io/xpose.xtras/reference/is_xp_xtras.md)
  :

  Basic class checker for `xp_xtras`

- [`set_var_levels()`](https://jprybylski.github.io/xpose.xtras/reference/set_var_levels.md)
  : Set variable levels

- [`set_var_types()`](https://jprybylski.github.io/xpose.xtras/reference/set_var_types.md)
  **\[experimental\]** : Set variable types

- [`set_var_types(`*`<default>`*`)`](https://jprybylski.github.io/xpose.xtras/reference/set_var_types.default.md)
  **\[experimental\]** : Set variable types

- [`set_var_types(`*`<xp_xtras>`*`)`](https://jprybylski.github.io/xpose.xtras/reference/set_var_types.xp_xtra.md)
  **\[experimental\]** : Set variable types

- [`set_var_types_x()`](https://jprybylski.github.io/xpose.xtras/reference/set_var_types_x.md)
  **\[experimental\]** : Set variable types

- [`as_leveler()`](https://jprybylski.github.io/xpose.xtras/reference/levelers.md)
  [`is_leveler()`](https://jprybylski.github.io/xpose.xtras/reference/levelers.md)
  [`lvl_bin()`](https://jprybylski.github.io/xpose.xtras/reference/levelers.md)
  [`lvl_sex()`](https://jprybylski.github.io/xpose.xtras/reference/levelers.md)
  [`lvl_inord()`](https://jprybylski.github.io/xpose.xtras/reference/levelers.md)
  : Level-defining helper functions

- [`val2lvl()`](https://jprybylski.github.io/xpose.xtras/reference/val2lvl.md)
  : Translate values to levels

- [`backfill_iofv()`](https://jprybylski.github.io/xpose.xtras/reference/backfill_iofv.md)
  : Add individual objective function to data

- [`attach_nlmixr2()`](https://jprybylski.github.io/xpose.xtras/reference/attach_nlmixr2.md)
  : Attach nlmixr2 fit object to xpose data object

- [`backfill_nlmixr2_props()`](https://jprybylski.github.io/xpose.xtras/reference/backfill_nlmixr2_props.md)
  : Populate some properties from nlmixr2 fit

- [`get_prm_nlmixr2()`](https://jprybylski.github.io/xpose.xtras/reference/get_prm_nlmixr2.md)
  : get_prm equivalent for nlmixr2 fits

- [`nlmixr2_as_xtra()`](https://jprybylski.github.io/xpose.xtras/reference/nlmixr2_as_xtra.md)
  : Convenience function for ingesting an nlmixr2 model to xpose and
  xpose.xtras

- [`nlmixr2_prm_associations()`](https://jprybylski.github.io/xpose.xtras/reference/nlmixr2_prm_associations.md)
  : Based on associations baked into nlmixr2, automatically add to xpose
  data

- [`nlmixr_example()`](https://jprybylski.github.io/xpose.xtras/reference/nlmixr_example.md)
  [`nlmixr2_example()`](https://jprybylski.github.io/xpose.xtras/reference/nlmixr_example.md)
  :

  Generate example `xp_xtras` objects from nlmixr2 fits

- [`xp_var()`](https://jprybylski.github.io/xpose.xtras/reference/xp_var.md)
  :

  `xp_var` Method

- [`check_levels()`](https://jprybylski.github.io/xpose.xtras/reference/check_levels.md)
  : Verify validity of level list

## xpose sets

Work with collections of models using `xpose_set`.

- [`xpose_set()`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)
  :

  Generate a set of `xpdb` objects

- [`xset_lineage()`](https://jprybylski.github.io/xpose.xtras/reference/xset_lineage.md)
  : Determine lineage within a set

- [`xset_waterfall()`](https://jprybylski.github.io/xpose.xtras/reference/xset_waterfall.md)
  : Waterfall plot

- [`add_xpdb()`](https://jprybylski.github.io/xpose.xtras/reference/add_xpdb.md)
  :

  Add one or more `xpdb` objects to an `xpose_set`

- [`focus_xpdb()`](https://jprybylski.github.io/xpose.xtras/reference/focus_xpdb.md)
  [`unfocus_xpdb()`](https://jprybylski.github.io/xpose.xtras/reference/focus_xpdb.md)
  [`focused_xpdbs()`](https://jprybylski.github.io/xpose.xtras/reference/focus_xpdb.md)
  [`focus_function()`](https://jprybylski.github.io/xpose.xtras/reference/focus_xpdb.md)
  [`focus_qapply()`](https://jprybylski.github.io/xpose.xtras/reference/focus_xpdb.md)
  : Focus on an xpdb object in an xpose_set

- [`reshape_set()`](https://jprybylski.github.io/xpose.xtras/reference/reshape_set.md)
  [`unreshape_set()`](https://jprybylski.github.io/xpose.xtras/reference/reshape_set.md)
  : Convert xpose_set to a nested list.

- [`diagram_lineage()`](https://jprybylski.github.io/xpose.xtras/reference/diagram_lineage.md)
  **\[experimental\]** :

  Visualize `xpose_set`

- [`add_prm_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_prm_association.md)
  [`drop_prm_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_prm_association.md)
  : Describe parameter associations

- [`mutate_prm()`](https://jprybylski.github.io/xpose.xtras/reference/mutate_prm.md)
  : Transform parameter values in place

- [`add_relationship()`](https://jprybylski.github.io/xpose.xtras/reference/add_relationship.md)
  [`remove_relationship()`](https://jprybylski.github.io/xpose.xtras/reference/add_relationship.md)
  : Add relationship(s) to an xpose_set

- [`` `%p%` ``](https://jprybylski.github.io/xpose.xtras/reference/grapes-p-grapes.md)
  : Binary check if LHS is parent of LHS

## Plotting helpers

Additional plotting functions and themes extending xpose.

- [`xplot_boxplot()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_boxplot.md)
  : Default xpose boxplot function

- [`xplot_pairs()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_pairs.md)
  : Wrapper around ggpairs

- [`xplot_rocplot()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_rocplot.md)
  : Default xpose ROC plot function

- [`iofv_vs_mod()`](https://jprybylski.github.io/xpose.xtras/reference/iofv_vs_mod.md)
  : Objective function changes across models

- [`prm_waterfall()`](https://jprybylski.github.io/xpose.xtras/reference/waterfalls.md)
  [`eta_waterfall()`](https://jprybylski.github.io/xpose.xtras/reference/waterfalls.md)
  [`iofv_waterfall()`](https://jprybylski.github.io/xpose.xtras/reference/waterfalls.md)
  : Specific waterfall plots

- [`shark_plot()`](https://jprybylski.github.io/xpose.xtras/reference/shark_plot.md)
  [`dofv_vs_id()`](https://jprybylski.github.io/xpose.xtras/reference/shark_plot.md)
  : Individual contributions to dOFV

- [`eta_vs_catcov()`](https://jprybylski.github.io/xpose.xtras/reference/eta_vs_catcov.md)
  : Eta categorical covariate plots (typical)

- [`eta_vs_contcov()`](https://jprybylski.github.io/xpose.xtras/reference/eta_vs_contcov.md)
  : Eta continuous covariate plots (typical)

- [`eta_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md)
  [`cov_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md)
  [`eta_vs_cov_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md)
  : Grid plots

- [`dv_vs_ipred_modavg()`](https://jprybylski.github.io/xpose.xtras/reference/modavg_plots.md)
  [`dv_vs_pred_modavg()`](https://jprybylski.github.io/xpose.xtras/reference/modavg_plots.md)
  [`ipred_vs_idv_modavg()`](https://jprybylski.github.io/xpose.xtras/reference/modavg_plots.md)
  [`pred_vs_idv_modavg()`](https://jprybylski.github.io/xpose.xtras/reference/modavg_plots.md)
  [`plotfun_modavg()`](https://jprybylski.github.io/xpose.xtras/reference/modavg_plots.md)
  **\[experimental\]** : Model average plots

- [`ipred_vs_ipred()`](https://jprybylski.github.io/xpose.xtras/reference/pred_vs_pred.md)
  [`pred_vs_pred()`](https://jprybylski.github.io/xpose.xtras/reference/pred_vs_pred.md)
  : Compare model predictions

- [`catdv_vs_dvprobs()`](https://jprybylski.github.io/xpose.xtras/reference/catdv_vs_dvprobs.md)
  : Non-simulation based likelihood model diagnostic

- [`roc_by_mod()`](https://jprybylski.github.io/xpose.xtras/reference/roc_by_mod.md)
  : ROC curve across models

- [`roc_plot()`](https://jprybylski.github.io/xpose.xtras/reference/roc_plot.md)
  : ROC Plot for categorical DVs

- [`shark_colors()`](https://jprybylski.github.io/xpose.xtras/reference/shark_colors.md)
  : Change colors of shark plots

- [`ind_roc()`](https://jprybylski.github.io/xpose.xtras/reference/ind_roc.md)
  : Individual ROC plots

- [`xset_waterfall()`](https://jprybylski.github.io/xpose.xtras/reference/xset_waterfall.md)
  : Waterfall plot

- [`xp_xtra_theme()`](https://jprybylski.github.io/xpose.xtras/reference/xp_xtra_theme.md)
  : Extra theme defaults

- [`xp4_xtra_theme()`](https://jprybylski.github.io/xpose.xtras/reference/xp4_xtra_theme.md)
  : Updated version of the xpose4 theme

- [`grab_xpose_plot()`](https://jprybylski.github.io/xpose.xtras/reference/grab_xpose_plot.md)
  :

  Grab processed `xpose_plot`

- [`wrap_xp_ggally()`](https://jprybylski.github.io/xpose.xtras/reference/wrap_xp_ggally.md)
  :

  Ensure consistent style with `GGally` functions

## Utilities

Miscellaneous helpers and integrations.

- [`modavg_xpdb()`](https://jprybylski.github.io/xpose.xtras/reference/modavg_xpdb.md)
  **\[experimental\]** : Create a model-averaged xpose data object

- [`eta_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md)
  [`cov_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md)
  [`eta_vs_cov_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md)
  : Grid plots

- [`nlmixr2_as_xtra()`](https://jprybylski.github.io/xpose.xtras/reference/nlmixr2_as_xtra.md)
  : Convenience function for ingesting an nlmixr2 model to xpose and
  xpose.xtras

- [`nlmixr2_prm_associations()`](https://jprybylski.github.io/xpose.xtras/reference/nlmixr2_prm_associations.md)
  : Based on associations baked into nlmixr2, automatically add to xpose
  data

- [`nlmixr_example()`](https://jprybylski.github.io/xpose.xtras/reference/nlmixr_example.md)
  [`nlmixr2_example()`](https://jprybylski.github.io/xpose.xtras/reference/nlmixr_example.md)
  :

  Generate example `xp_xtras` objects from nlmixr2 fits

- [`desc_from_comments()`](https://jprybylski.github.io/xpose.xtras/reference/desc_from_comments.md)
  : Backfill utility for descriptions

- [`edit_xpose_data()`](https://jprybylski.github.io/xpose.xtras/reference/edit_xpose_data.md)
  : Master xpdb editing function

- [`expose_param()`](https://jprybylski.github.io/xpose.xtras/reference/expose_param.md)
  : Expose a model parameter of xpdb objects in an xpose_set

- [`expose_property()`](https://jprybylski.github.io/xpose.xtras/reference/expose_property.md)
  : Expose a property of xpdb objects in an xpose_set

- [`get_prop()`](https://jprybylski.github.io/xpose.xtras/reference/get_prop.md)
  : Generic function to extract a property from a model summary

- [`set_prop()`](https://jprybylski.github.io/xpose.xtras/reference/set_prop.md)
  : Set a summary property

- [`get_index()`](https://jprybylski.github.io/xpose.xtras/reference/get_set_index.md)
  [`set_index()`](https://jprybylski.github.io/xpose.xtras/reference/get_set_index.md)
  : Get full index for xpose_data data

- [`set_base_model()`](https://jprybylski.github.io/xpose.xtras/reference/set_base_model.md)
  [`get_base_model()`](https://jprybylski.github.io/xpose.xtras/reference/set_base_model.md)
  [`unset_base_model()`](https://jprybylski.github.io/xpose.xtras/reference/set_base_model.md)
  :

  Base model for `xpose_set`

- [`set_dv_probs()`](https://jprybylski.github.io/xpose.xtras/reference/set_dv_probs.md)
  : Set probability columns for categorical endpoints

- [`list_dv_probs()`](https://jprybylski.github.io/xpose.xtras/reference/list_dv_probs.md)
  : For a categorical DV variable, show associated probabilities

- [`group_by_x()`](https://jprybylski.github.io/xpose.xtras/reference/summarise_xpdb.md)
  [`ungroup_x()`](https://jprybylski.github.io/xpose.xtras/reference/summarise_xpdb.md)
  : Group/ungroup and summarize variables in an xpdb

- [`mutate_x()`](https://jprybylski.github.io/xpose.xtras/reference/modify_xpdb.md)
  [`rename_x()`](https://jprybylski.github.io/xpose.xtras/reference/modify_xpdb.md)
  : Add, remove or rename variables in an xpdb

- [`select_subset()`](https://jprybylski.github.io/xpose.xtras/reference/select_subset.md)
  : Convenience wrapper for tidyselect

- [`set_option()`](https://jprybylski.github.io/xpose.xtras/reference/set_option.md)
  :

  Set an `xpose` option

- [`reportable_digits()`](https://jprybylski.github.io/xpose.xtras/reference/reportable_digits.md)
  : Reportable digits for model fit

- [`test_xpdb()`](https://jprybylski.github.io/xpose.xtras/reference/test_xpdb.md)
  : Logical instead of exception for xpose data check

- [`irep()`](https://jprybylski.github.io/xpose.xtras/reference/irep.md)
  **\[deprecated\]** : Add simulation counter

- [`derive_prm()`](https://jprybylski.github.io/xpose.xtras/reference/derive_prm.md)
  [`backfill_derived()`](https://jprybylski.github.io/xpose.xtras/reference/derive_prm.md)
  : Derive full parameter set for mammillary PK model

- [`diagnose_constants()`](https://jprybylski.github.io/xpose.xtras/reference/diagnose_constants.md)
  : Check for potential parameterization issues

- [`get_prm()`](https://jprybylski.github.io/xpose.xtras/reference/get_prm.md)
  : Access model parameters

- [`get_shk()`](https://jprybylski.github.io/xpose.xtras/reference/get_shk.md)
  : Get shrinkage estimates from model summary

- [`list_vars()`](https://jprybylski.github.io/xpose.xtras/reference/list_vars.md)
  :

  Updates to `list_vars`

## Data

Example objects for documentation.

- [`pheno_base`](https://jprybylski.github.io/xpose.xtras/reference/pheno_base.md)
  :

  An `xp_xtras` example of a base model

- [`pheno_final`](https://jprybylski.github.io/xpose.xtras/reference/pheno_final.md)
  :

  An `xp_xtras` example of a final model

- [`pheno_saem`](https://jprybylski.github.io/xpose.xtras/reference/pheno_saem.md)
  :

  An `xp_xtras` example of a final model

- [`pheno_set`](https://jprybylski.github.io/xpose.xtras/reference/pheno_set.md)
  :

  A more complex example of `xpose_set` object

- [`pkpd_m3`](https://jprybylski.github.io/xpose.xtras/reference/pkpd_m3.md)
  :

  An `xp_xtras` example of an M3 model

- [`pkpd_m3_df`](https://jprybylski.github.io/xpose.xtras/reference/pkpd_m3_df.md)
  :

  An `xp_xtras` example of an M3 model (dataset)

- [`vismo_dtmm`](https://jprybylski.github.io/xpose.xtras/reference/vismo_dtmm.md)
  :

  An `xp_xtras` example of the discrete-time Markov model of categorical
  vismodegib data

- [`vismo_pomod`](https://jprybylski.github.io/xpose.xtras/reference/vismo_pomod.md)
  :

  An `xp_xtras` example of the proportional odds categorical vismodegib
  model

- [`vismodegib`](https://jprybylski.github.io/xpose.xtras/reference/vismodegib.md)
  : A tibble of mock data used for fitting vismodegib models

- [`xpdb_set`](https://jprybylski.github.io/xpose.xtras/reference/xpdb_set.md)
  :

  An example `xpose_set` object

- [`xpdb_x`](https://jprybylski.github.io/xpose.xtras/reference/xpdb_x.md)
  :

  An example `xp_xtras` object
