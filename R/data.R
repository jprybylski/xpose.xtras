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
#' The <[`xpdb_ex_pk`][xpose::xpdb_ex_pk]> object converted to `xp_xtras`. For examples.
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
#' @source <https://doi.org/10.1159/000457062> and nlmixr2data::pheno_sd
"pheno_set"

#' An `xp_xtras` example of a base model
#'
#' Base model for phenobarbital in neonates.
#'
#' This is `run6` in <[`pheno_set`]>
#'
#' @format ## `xp_xtras`
#' An `xp_xtras` object.
#'
#' @source <https://doi.org/10.1159/000457062> and nlmixr2data::pheno_sd
"pheno_base"

#' An `xp_xtras` example of a final model
#'
#' Final model for phenobarbital in neonates.
#'
#' This is re-parameterized from the covariate-building work, which
#' in this case did not identify a relationship with Apgar score.
#'
#' This is `run16` in <[`pheno_set`]>
#'
#' @format ## `xp_xtras`
#' An `xp_xtras` object.
#'
#' @source <https://doi.org/10.1159/000457062> and nlmixr2data::pheno_sd
"pheno_final"

#' An `xp_xtras` example of a final model
#'
#' Final model for phenobarbital in neonates.
#'
#' This is the same as [`pheno_final`] but fitted with SAEM/IMP.
#'
#' Not a part of <[`pheno_set`]>
#'
#' @format ## `xp_xtras`
#' An `xp_xtras` object.
#'
#' @source <https://doi.org/10.1159/000457062> and nlmixr2data::pheno_sd
"pheno_saem"

#' A tibble of mock data used for fitting vismodegib models
#'
#' The referenced work presents two alternative modeling approaches
#' for muscle spasm response to vismodegib. There is a mock
#' dataset for one person, and using the provided model a 50 participant
#' mock dataset could be generated.
#'
#'
#' @format ## `tibble`
#' An `tibble`.
#'
#' @references
#' Lu, T., Yang, Y., Jin, J.Y. and Kågedal, M. (2020),
#' Analysis of Longitudinal-Ordered Categorical Data for
#' Muscle Spasm Adverse Event of Vismodegib: Comparison
#' Between Different Pharmacometric Models. CPT
#' Pharmacometrics Syst. Pharmacol., 9: 96-105.
#' <https://doi.org/10.1002/psp4.12487>
#'
#' @source Generated using sup-0009 and sup-0010 from the reference.
#'
"vismodegib"

#' An `xp_xtras` example of the proportional odds categorical vismodegib model
#'
#' The referenced work presents two alternative modeling approaches
#' for muscle spasm response to vismodegib. This is a fit of the
#' provided proportional odds model to the 50 participant mock data.
#'
#'
#' @format ## `xp_xtras`
#' An `xp_xtras` object.
#'
#' @references
#' Lu, T., Yang, Y., Jin, J.Y. and Kågedal, M. (2020),
#' Analysis of Longitudinal-Ordered Categorical Data for
#' Muscle Spasm Adverse Event of Vismodegib: Comparison
#' Between Different Pharmacometric Models. CPT
#' Pharmacometrics Syst. Pharmacol., 9: 96-105.
#' <https://doi.org/10.1002/psp4.12487>
#'
#' @source Derived from sup-0009 and sup-0010 from the reference.
#'
"vismo_pomod"

#' An `xp_xtras` example of the discrete-time Markov model of categorical vismodegib data
#'
#' The referenced work presents two alternative modeling approaches
#' for muscle spasm response to vismodegib. This is a fit of the
#' provided discrete-time Markov model to the 50 participant mock data.
#'
#'
#' @format ## `xp_xtras`
#' An `xp_xtras` object.
#'
#' @references
#' Lu, T., Yang, Y., Jin, J.Y. and Kågedal, M. (2020),
#' Analysis of Longitudinal-Ordered Categorical Data for
#' Muscle Spasm Adverse Event of Vismodegib: Comparison
#' Between Different Pharmacometric Models. CPT
#' Pharmacometrics Syst. Pharmacol., 9: 96-105.
#' <https://doi.org/10.1002/psp4.12487>
#'
#' @source Derived from sup-0009 and sup-0010 from the reference.
#'
"vismo_dtmm"

#' An `xp_xtras` example of an M3 model
#'
#' A representative PK/PD model with M3 fitting applied.
#'
#' @format ## `xp_xtras`
#' An `xp_xtras` object.
#'
#' @references
#' Beal, S.L. Ways to Fit a PK Model with Some Data
#' Below the Quantification Limit. J Pharmacokinet
#' Pharmacodyn 28, 481-504 (2001).
#' https://doi.org/10.1023/A:1012299115260
#'
#' Prybylski JP. Indirect modeling of derived outcomes:
#' Are minor prediction discrepancies a cause for concern?
#' CPT Pharmacometrics Syst Pharmacol. 2024; 00: 1-9.
#' doi:10.1002/psp4.13219
#'
#' @source <https://doi.org/10.1002/psp4.13219>
"pkpd_m3"

#' An `xp_xtras` example of an M3 model (dataset)
#'
#' The dataset used to fit the [`pkpd_m3`] model.
#'
#' @format ## `xp_xtras`
#' An `xp_xtras` object.
#'
#' @references
#' Prybylski JP. Indirect modeling of derived outcomes:
#' Are minor prediction discrepancies a cause for concern?
#' CPT Pharmacometrics Syst Pharmacol. 2024; 00: 1-9.
#' doi:10.1002/psp4.13219
#'
#' @source <https://doi.org/10.1002/psp4.13219>
"pkpd_m3_df"



#' An `xp_xtra` example based on an nlmixr2 fit
#'
#' This is the most basic one compartment example used in
#' nlmixr2 example documentation. It is a fit to the popular theophylline
#' dataset.
#'
#' @rdname xpdb_nlmixr2
#'
#' @seealso [Theoph][datasets::Theoph]
#'
#' @references
#' Fidler M (2025). _nlmixr2: Nonlinear Mixed Effects Models in Population PK/PD_.
#' doi:10.32614/CRAN.package.nlmixr2 <https://doi.org/10.32614/CRAN.package.nlmixr2>,
#' R package version 3.0.2, <https://CRAN.R-project.org/package=nlmixr2>.
#'
#' Fidler M, Wilkins J, Hooijmaijers R, Post T, Schoemaker R, Trame M, Xiong Y, Wang W
#' (2019). “Nonlinear Mixed-Effects Model Development and Simulation Using nlmixr and
#' Related R Open-Source Packages.” _CPT: Pharmacometrics & Systems Pharmacology_,
#' *8*(9), 621-633. <https://doi.org/10.1002/psp4.12445>.
#'
#' Schoemaker R, Fidler M, Laveille C, Wilkins J, Hooijmaijers R, Post T, Trame M,
#' Xiong Y, Wang W (2019). “Performance of the SAEM and FOCEI Algorithms in the
#' Open-Source, Nonlinear Mixed Effect Modeling Tool nlmixr.” _CPT: Pharmacometrics &
#'   Systems Pharmacology_, *8*(12), 923-930. <https://doi.org/10.1002/psp4.12471>.
#'
#' @source <https://nlmixr2.org/articles/running_nlmixr.html>
"xpdb_nlmixr2"

#' @rdname xpdb_nlmixr2
"xpdb_nlmixr2_saem"

#' An `xp_xtra` example based on a complex nlmixr2 fit
#'
#' Based on the multiple endpoint warfarin PK/PD example.
#'
#'
#' @seealso [xpdb_nlmixr2]
#'
#'
#' @source <https://nlmixr2.org/articles/multiple-endpoints.html>
"nlmixr2_warfarin"

#' An `xp_xtra` example based on a nlmixr2 fit with M3 censoring
#'
#' A modified version of the theophylline model fit with censoring
#' added in to provoke M3 censoring. An additional output variable
#' is added to use an an example in categorical DVs.
#'
#'
#' @seealso [xpdb_nlmixr2] [catdv_vs_dvprobs()]
#'
#'
#' @source <https://github.com/nlmixr2/nlmixr2/issues/275#issuecomment-2445469327>
#'
#' @examples
#' \dontrun{
#' # This not-run block is to show how the dataset was generated
#' # This is also available in data-raw of the github repo
#' one.cmt <- function() {
#'   ini({
#'     ## You may label each parameter with a comment
#'     tka <- 0.45 # Ka
#'     tcl <- log(c(0, 2.7, 100)) # Log Cl
#'     ## This works with interactive models
#'     ## You may also label the preceding line with label("label text")
#'     tv <- 3.45; label("log V")
#'     ## the label("Label name") works with all models
#'     eta.ka ~ 0.6
#'     eta.cl ~ 0.3
#'     eta.v ~ 0.1
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- exp(tka + eta.ka)
#'     cl <- exp(tcl + eta.cl)
#'     v <- exp(tv + eta.v)
#'     # Not sure how one does this with linCmt(), if that has to be posthoc
#'     d/dt(depot) = -ka*depot
#'     d/dt(cent) = ka*depot - cl*cent/v
#'     cp = cent/v
#'     blqlike = pnorm( (LLOQ - cp)/add.sd  ) # blq likelihood for diagnostics
#'     cp ~ add(add.sd)
#'   })
#' }
#' theo_sdcens=nlmixr2data::theo_sd
#' good_lloq <- quantile(theo_sdcens[theo_sdcens$EVID==0,]$DV, 0.15)
#' theo_sdcens$CENS=ifelse(theo_sdcens$DV<good_lloq & theo_sdcens$EVID==0,1,0)
#' theo_sdcens$DV=ifelse(theo_sdcens$CENS==1,good_lloq,theo_sdcens$DV)
#' theo_sdcens$LLOQ=good_lloq # add lloq column
#' fitcens <- nlmixr2est::nlmixr2(one.cmt, theo_sdcens, "focei",
#'            control=nlmixr2est::foceiControl(print=0))
#' nlmixr2_m3 <- nlmixr2_as_xtra(obj = fitcens, .skip_assoc = TRUE)
#' }
#'
#'
#' nlmixr2_m3 %>% # modified from catdv_vs_dvprobs example
#'   set_var_types(catdv=CENS,dvprobs=BLQLIKE) %>%
#'   set_dv_probs(1, 1~BLQLIKE, .dv_var = CENS) %>%
#'   set_var_levels(1, CENS = lvl_bin()) %>%
#'   catdv_vs_dvprobs(xlab = "basic", quiet = TRUE)
"nlmixr2_m3"
