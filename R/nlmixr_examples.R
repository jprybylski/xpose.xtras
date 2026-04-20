#' Generate example `xp_xtras` objects from nlmixr2 fits
#'
#' Runs an nlmixr2 fit on demand and returns the result as a ready-to-use
#' `xp_xtras` object. `nlmixr2_example` is an alias.
#'
#' Available examples:
#' \describe{
#'   \item{`"xpdb_nlmixr2"`}{One-compartment FOCEI fit to the theophylline
#'     dataset. The basic introductory example from the nlmixr2 documentation.
#'     See [Theoph][datasets::Theoph].}
#'   \item{`"xpdb_nlmixr2_saem"`}{The same one-compartment theophylline model
#'     fit with SAEM instead of FOCEI.}
#'   \item{`"nlmixr2_warfarin"`}{Multiple-endpoint warfarin PK/PD model with
#'     a four-compartment PK and turnover PD. Provides categorical covariate
#'     data useful for eta diagnostic examples.}
#'   \item{`"nlmixr2_m3"`}{Theophylline one-compartment model with censoring
#'     applied to provoke M3 likelihood handling. Includes a `BLQLIKE` output
#'     variable for use as a categorical DV example with [catdv_vs_dvprobs()].}
#' }
#'
#' @param name <`character`> Name of the example to generate. One of
#'   `"xpdb_nlmixr2"`, `"xpdb_nlmixr2_saem"`, `"nlmixr2_warfarin"`,
#'   `"nlmixr2_m3"`.
#'
#' @return An `xp_xtras` object with the nlmixr2 fit attached.
#' @export
#' @rdname nlmixr_example
#'
#' @seealso [nlmixr2_as_xtra()], [catdv_vs_dvprobs()]
#'
#' @references
#' Fidler M (2025). _nlmixr2: Nonlinear Mixed Effects Models in Population PK/PD_.
#' \doi{doi:10.32614/CRAN.package.nlmixr2},
#' R package version 3.0.2, <https://CRAN.R-project.org/package=nlmixr2>.
#'
#' Fidler M, Wilkins J, Hooijmaijers R, Post T, Schoemaker R, Trame M, Xiong Y, Wang W
#' (2019). "Nonlinear Mixed-Effects Model Development and Simulation Using nlmixr and
#' Related R Open-Source Packages." _CPT: Pharmacometrics & Systems Pharmacology_,
#' *8*(9), 621-633. \doi{doi:10.1002/psp4.12445}.
#'
#' Schoemaker R, Fidler M, Laveille C, Wilkins J, Hooijmaijers R, Post T, Trame M,
#' Xiong Y, Wang W (2019). "Performance of the SAEM and FOCEI Algorithms in the
#' Open-Source, Nonlinear Mixed Effect Modeling Tool nlmixr." _CPT: Pharmacometrics &
#'   Systems Pharmacology_, *8*(12), 923-930. \doi{doi:10.1002/psp4.12471}.
#'
#' Beal, S.L. Ways to Fit a PK Model with Some Data Below the Quantification
#' Limit. J Pharmacokinet Pharmacodyn 28, 481-504 (2001).
#' \doi{doi:10.1023/A:1012299115260}
#'
#' @source
#' `"xpdb_nlmixr2"` and `"xpdb_nlmixr2_saem"`:
#' <https://nlmixr2.org/articles/running_nlmixr.html>
#'
#' `"nlmixr2_warfarin"`:
#' <https://nlmixr2.org/articles/multiple-endpoints.html>
#'
#' `"nlmixr2_m3"`:
#' <https://github.com/nlmixr2/nlmixr2/issues/275#issuecomment-2445469327>
#'
#' @examples
#' \dontrun{
#' xpdb_nlmixr2 <- nlmixr_example("xpdb_nlmixr2")
#'
#' nlmixr2_m3 <- nlmixr_example("nlmixr2_m3")
#' nlmixr2_m3 %>%
#'   set_var_types(catdv = CENS, dvprobs = BLQLIKE) %>%
#'   set_dv_probs(1, 1 ~ BLQLIKE, .dv_var = CENS) %>%
#'   set_var_levels(1, CENS = lvl_bin()) %>%
#'   catdv_vs_dvprobs(xlab = "basic", quiet = TRUE)
#' }
nlmixr_example <- function(name) {
  rlang::check_installed(
    c("nlmixr2est", "nlmixr2data", "xpose.nlmixr2"),
    reason = "to generate nlmixr2 examples"
  )
  name <- rlang::arg_match(name, .nlmixr_example_names())
  switch(
    name,
    xpdb_nlmixr2      = .nlmixr_example_xpdb_nlmixr2(),
    xpdb_nlmixr2_saem = .nlmixr_example_xpdb_nlmixr2_saem(),
    nlmixr2_warfarin  = .nlmixr_example_nlmixr2_warfarin(),
    nlmixr2_m3        = .nlmixr_example_nlmixr2_m3()
  )
}

#' @rdname nlmixr_example
#' @export
nlmixr2_example <- nlmixr_example

.nlmixr_example_names <- function() {
  c("xpdb_nlmixr2", "xpdb_nlmixr2_saem", "nlmixr2_warfarin", "nlmixr2_m3")
}

# --- unexported helpers (code adapted from data-raw/) ------------------------

.nlmixr_example_xpdb_nlmixr2 <- function() {
  one.cmt <- function() {
    ini({
      tka <- 0.45
      tcl <- log(c(0, 2.7, 100))
      tv <- 3.45; label("log V")
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd)
    })
  }
  theo_sd_fit <- nlmixr2est::nlmixr2(
    one.cmt, nlmixr2data::theo_sd, "focei",
    control = nlmixr2est::foceiControl(print = 0)
  )
  nlmixr2_as_xtra(theo_sd_fit, .skip_assoc = TRUE) %>%
    set_option(dir = "~") %>%
    set_prop(dir = "~")
}

.nlmixr_example_xpdb_nlmixr2_saem <- function() {
  one.cmt <- function() {
    ini({
      tka <- 0.45
      tcl <- log(c(0, 2.7, 100))
      tv <- 3.45; label("log V")
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd)
    })
  }
  theo_sd_saem <- nlmixr2est::nlmixr2(
    one.cmt, nlmixr2data::theo_sd, "saem",
    control = nlmixr2est::saemControl(print = 0)
  )
  nlmixr2_as_xtra(theo_sd_saem, .skip_assoc = TRUE) %>%
    set_option(dir = "~") %>%
    set_prop(dir = "~")
}

.nlmixr_example_nlmixr2_warfarin <- function() {
  # eval(parse()) prevents R CMD check from statically analyzing rxode2 DSL
  # syntax (effect(0) = e0 initial condition) that is not valid plain R.
  pk.turnover.emax3 <- eval(parse(text = '
    function() {
      ini({
        tktr <- log(1)
        tka <- log(1)
        tcl <- log(0.1)
        tv <- log(10)
        eta.ktr ~ 1
        eta.ka ~ 1
        eta.cl ~ 2
        eta.v ~ 1
        prop.err <- 0.1
        pkadd.err <- 0.1
        temax <- logit(0.8)
        tec50 <- log(0.5)
        tkout <- log(0.05)
        te0 <- log(100)
        eta.emax ~ .5
        eta.ec50  ~ .5
        eta.kout ~ .5
        eta.e0 ~ .5
        pdadd.err <- 10
      })
      model({
        ktr <- exp(tktr + eta.ktr)
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        emax = expit(temax + eta.emax)
        ec50 = exp(tec50 + eta.ec50)
        kout = exp(tkout + eta.kout)
        e0 = exp(te0 + eta.e0)
        DCP = center / v
        PD = 1 - emax * DCP / (ec50 + DCP)
        effect(0) = e0
        kin = e0 * kout
        d/dt(depot) = -ktr * depot
        d/dt(gut) = ktr * depot - ka * gut
        d/dt(center) = ka * gut - cl / v * center
        d/dt(effect) = kin * PD - kout * effect
        cp = center / v
        cp ~ prop(prop.err) + add(pkadd.err)
        effect ~ add(pdadd.err) | pca
      })
    }
  '))
  fit.TOS <- nlmixr2est::nlmixr2(
    pk.turnover.emax3, nlmixr2data::warfarin, "focei",
    control = list(print = 0),
    table = list(cwres = TRUE, npde = TRUE)
  )
  nlmixr2_as_xtra(fit.TOS, .skip_assoc = TRUE) %>%
    set_option(dir = "~") %>%
    set_prop(dir = "~")
}

.nlmixr_example_nlmixr2_m3 <- function() {
  # eval(parse()) prevents R CMD check from statically analyzing rxode2 DSL
  # syntax (d/dt, pnorm as DSL token) that triggers false-positive NOTEs.
  one.cmt <- eval(parse(text = '
    function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45; label("log V")
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d/dt(depot) = -ka * depot
        d/dt(cent) = ka * depot - cl * cent / v
        cp = cent / v
        blqlike = pnorm((LLOQ - cp) / add.sd)
        cp ~ add(add.sd)
      })
    }
  '))
  theo_sdcens <- nlmixr2data::theo_sd
  good_lloq <- quantile(theo_sdcens[theo_sdcens$EVID == 0, ]$DV, 0.15)
  theo_sdcens$CENS <- ifelse(
    theo_sdcens$DV < good_lloq & theo_sdcens$EVID == 0, 1, 0
  )
  theo_sdcens$DV <- ifelse(theo_sdcens$CENS == 1, good_lloq, theo_sdcens$DV)
  theo_sdcens$LLOQ <- good_lloq
  fitcens <- nlmixr2est::nlmixr2(
    one.cmt, theo_sdcens, "focei",
    control = nlmixr2est::foceiControl(print = 0)
  )
  nlmixr2_as_xtra(fitcens, .skip_assoc = TRUE) %>%
    set_option(dir = "~") %>%
    set_prop(dir = "~")
}
