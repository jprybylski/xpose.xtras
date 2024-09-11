#' An example `xpose_set` object
#'
#' A set of identical `xpdb` objects to demo various features of `xpose.xtras`.
#'
#' @format ## `xpose_set`
#' An `xpose_set` object of length 4 with a single lineage.
#'
#' @source Assembled from the `xpdb_ex_pk` object in the `xpose` package.
"xpdb_set"

#' An example `xp_xtras` object
#'
#' The <[`xpdb_ex_pk`][xpose::xpdb_ex_pk]> object coverted to `xp_xtras`. For examples.
#'
#' @format ## `xp_xtras`
#' An `xp_xtras` object with no extra data filled.
#'
#' @source Assembled from the `xpdb_ex_pk` object in the `xpose` package.
"xpdb_x"

#' A more complex example of `xpose_set` object
#'
#' Model-building set for the phenobarbital in neonates PK data
#' used across multiple packages.
#'
#' This is not a demonstration of high-quality model-building,
#' it is just a typical and simple example.
#'
#' @format ## `xpose_set`
#' An `xpose_set` object of length 14 with a branched lineage.
#'
#' @source https://doi.org/10.1159/000457062 and nlmixr2data::pheno_sd
"pheno_set"

#' An `xp_xtras` example of a base model
#'
#' Base model for phenobarbital in neotates.
#'
#' This is `run6` in <[`pheno_set`]>
#'
#' @format ## `xp_xtras`
#' An `xp_xtras` object.
#'
#' @source https://doi.org/10.1159/000457062 and nlmixr2data::pheno_sd
"pheno_base"

#' An `xp_xtras` example of a final model
#'
#' Final model for phenobarbital in neotates.
#'
#' This is re-parameterized from the covariate-building work, which
#' in this case did not identify a relationship with Apgar score.
#'
#' This is `run16` in <[`pheno_set`]>
#'
#' @format ## `xp_xtras`
#' An `xp_xtras` object.
#'
#' @source https://doi.org/10.1159/000457062 and nlmixr2data::pheno_sd
"pheno_final"
