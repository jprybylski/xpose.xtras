
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xpose.xtras

<!-- badges: start -->
<!-- badges: end -->

The goal of xpose.xtras is to add some at-present missing functionality
(to-be-deprecated if they are added to xpose), or functions unlikely to
be added to the base xpose package.

# Todo

Partially completed items have ~~strikethough~~ to “hide” the finished
part.

- [x] Extend xpose_data class or add a new class with additional
  structures.
  - [x] Add support for defining catcov, dvid, occ and dv levels
    - [ ] Add related plots
  - [ ] Add ~~support for categorical dv~~ (and corresponding
    diagnostics) (catdv added, no support yet)
- [x] Add new class for set of xpdb objects
  - [ ] Read from directory
  - [ ] diff() method
  - [x] Ability to add child-parent relationships
  - [x] Ability to **drop** child-parent relationships
  - [ ] tabulate dofv (and related)
  - [ ] apply transformations to one or multiple xpdbs in a set
  - [ ] model comparison functions from xpose4
  - [ ] model averaging VPC
- [x] set_var_type changes
  - [x] accept vector for continuous and categorical variables (*already
    present*)
  - [x] set_var_type to accept tidyselect syntax
- [x] import irepl fix
- [ ] micro/macro and derived constant convenience functions, as well as
  un-flip flop suggestions for up to 3 compartments
- [ ] Ability to import bbr model (bbr::read_model %\>%
  xpose.xtras::import_bbr())
- [ ] Ability to import/export xpose4 model (xpdb \<-
  xpose.xtras::import_xp4(xpdb4); xpdb4 \<-
  xpose.xtras::export_xp4(xpdb))
- [x] Getter and setter for description (“descr”) in xpose_data (get/set
  any summary property)
- [ ] When joining firstonly, rows are getting duplicated
- [ ] Option to set 0s to NA for select columns
- [ ] Add support to grab xpose_plot objects
- [ ] Built-in support for contcov and catcov plots
- [ ] Built-in implementation of GGally::ggpairs
- [ ] Correct filter() (filter on xpdb_data breaks some columns, eg ETA)
  - [ ] Happens if ETAs are only in firstonly join, which adds them as
    NA in the main data object
- [ ] Beef up DVID support (check for it when doing dv_vs and ind plots
  since it would impact interpretation)
- [ ] Visualize colinearity
- [ ] table of covariates (demographics)
  - [ ] Basic tibble
  - [ ] Nice output version (similar to nice output for prm_table)
- [ ] Nice param table, incorporate pmxcv
  - [ ] Need way to reference etas to fixed effects and state
    transformation
- [ ] unit tests for all functions
- [ ] tidyvpc support?
  - [ ] Want vpc_tte ported over (even though this is not in xpose atm)
- [ ] options-defined default set of diagnostics
- [ ] template-based reporting?
- [ ] patchwork integration?
- [ ] vignette
- [ ] examples
- [ ] pkgdown
