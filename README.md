
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xpose.xtras <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
<!-- badges: end -->

The goal of xpose.xtras is to add some at-present missing functionality
(to-be-deprecated if they are added to xpose), or functions unlikely to
be added to the base xpose package.

# Todo

Partially completed items have ~~strikethough~~ to “hide” the finished
part. VPC-related tasks have been **de-scoped** because I don’t plan to
use xpose-style VPC (ie, vpc::vpc) functionality extensively.

- [x] Extend xpose_data class or add a new class with additional
  structures.
  - [x] Add support for defining catcov, dvid, occ and dv levels
    - [x] Add related plots
  - [ ] Add ~~support for categorical dv~~ (and corresponding
    diagnostics) (catdv added, no support yet)
  - [ ] Binned time var type
- [x] Add new class for set of xpdb objects
  - [ ] Read from directory
  - [x] diff() method
  - [x] Ability to add child-parent relationships
  - [x] Ability to **drop** child-parent relationships
  - [ ] tabulate dofv (and related)
  - [x] apply transformations to one or multiple xpdbs in a set (done
    with `focus`)
  - [x] model comparison functions from xpose4
  - [x] **De-scoped** model averaging VPC
- [x] set_var_type changes
  - [x] accept vector for continuous and categorical variables (*already
    present*)
  - [x] set_var_type to accept tidyselect syntax
- [x] import irepl fix
- [ ] micro/macro and derived constant convenience functions, as well as
  un-flip flop suggestions for up to 3 compartments
- [ ] Ability to import bbr model (bbr::read_model %\>%
  xpose.xtras::import_bbr())
- [x] **De-scoped** Ability to import/export xpose4 model (xpdb \<-
  xpose.xtras::import_xp4(xpdb4); xpdb4 \<-
  xpose.xtras::export_xp4(xpdb))
- [x] Getter and setter for description (“descr”) in xpose_data (get/set
  any summary property)
- [ ] When joining firstonly, rows are getting duplicated
- [ ] Option to set 0s to NA for select columns
- [x] Add support to grab xpose_plot objects
- [x] Built-in support for contcov and catcov plots
- [x] Built-in implementation of GGally::ggpairs
- [ ] Correct filter() (filter on xpdb_data breaks some columns, eg ETA)
  - [ ] Happens if ETAs are only in firstonly join, which adds them as
    NA in the main data object
- [ ] Beef up DVID support (check for it when doing dv_vs and ind plots
  since it would impact interpretation)
- [ ] Visualize colinearity
- [ ] table of covariates (demographics)
  - [ ] Basic tibble
  - [ ] Nice output version (similar to nice output for prm_table)
- [ ] Nice param table, incorporate ~~pmxcv~~
  - [x] Need way to reference etas to fixed effects and state
    transformation
- [ ] unit tests for all functions
- [x] **De-scoped (add any needed features to tidyvpc directly)**
  tidyvpc support?
  - [x] **De-scoped** Want vpc_tte ported over (even though this is not
    in xpose atm)
- [ ] options-defined default set of diagnostics
- [ ] template-based reporting?
- [ ] patchwork integration?
- [ ] vignette
- [ ] examples
- [ ] pkgdown
- [ ] devtools::spell_check
- [ ] Custom examples (eg, phenobarbital model building, xpose_xtra for
  each and together xpose_set)
- [ ] nlmixr2.xpose tests
- [ ] *re-run* data-raw files prior to build
