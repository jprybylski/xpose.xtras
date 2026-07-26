## Log-likelihood/AIC/BIC for xpose_data and xpose_set objects

#' Log-likelihood, AIC and BIC for `xpose_data` objects
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' NONMEM (and nlmixr2) objective function values (OFV) are `-2*log-likelihood`,
#' so the log-likelihood of a model is derived as `-ofv/2`. The number of estimated
#' parameters (thetas, and unfixed omegas/sigmas) is used as the degrees of freedom.
#'
#' Because <[`stats::AIC`]> and <[`stats::BIC`]> dispatch through <[`stats::logLik`]>
#' by default, only `logLik.xpose_data` needs to be defined here for `AIC()`/`BIC()`
#' to work as expected on `xpose_data`/`xp_xtras` objects; no `AIC`/`BIC` methods
#' are defined for a single model.
#'
#' Not calculable for a model-averaged ("franken") `xpose_data` object (eg, the
#' output of <[`modavg_xpdb`]>), since such an object does not correspond to a
#' single fitted model; an error is thrown instead.
#'
#' @param object <`xpose_data`> or <`xp_xtras`> object
#' @param .problem <`numeric`> Problem number to use. Uses the xpdb default if not provided.
#' @param .subprob <`numeric`> Subproblem number to use. Uses the xpdb default if not provided.
#' @param .method <`numeric`> Method to use. Uses the xpdb default if not provided.
#' @param ... Not used.
#'
#' @return <`logLik`> object, as documented in <[`stats::logLik`]>, with `"df"`
#' (number of estimated parameters) and `"nobs"` (number of observations) attributes set.
#' @exportS3Method stats::logLik
#'
#' @examples
#'
#' logLik(xpdb_x)
#' AIC(xpdb_x)
#' BIC(xpdb_x)
#'
logLik.xpose_data <- function(object, .problem = NULL, .subprob = NULL, .method = NULL, ...) {
  if (is_franken_xpdb(object)) {
    cli::cli_abort(
      "Log-likelihood is not calculable for a model-averaged (combined) {.cls xpose_data} object."
    )
  }

  ofv  <- as.numeric(get_prop(object, "ofv", .problem = .problem, .subprob = .subprob, .method = .method))
  nobs <- as.numeric(get_prop(object, "nobs", .problem = .problem, .subprob = .subprob, .method = .method))
  npar <- hot_swap_base_get_prm(object, .problem = .problem, .subprob = .subprob, .method = .method, quiet = TRUE) %>%
    dplyr::pull(fixed) %>%
    magrittr::not() %>%
    sum()

  val <- -ofv / 2
  attr(val, "df")   <- npar
  attr(val, "nobs") <- nobs
  class(val) <- "logLik"
  val
}

#' Log-likelihood, AIC and BIC across an `xpose_set`
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' If no base model is provided, and if lineage is unclear, the first model in
#' the `xpose_set` is used as the base model, exactly as in <[`diff.xpose_set`]>.
#' Unlike `diff()`, values are not differenced, so a straightforward
#' model-to-model comparison is possible.
#'
#' As with the `xpose_data` methods, a component model that is itself a
#' model-averaged ("franken") object will cause an error.
#'
#' @param object <`xpose_set`> object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Passed to <[`xset_lineage`]>.
#' `.spinner=FALSE` can also be set here.
#'
#' @returns <`numeric`> vector, or list thereof, following <[`xset_lineage`]>.
#' @exportS3Method stats::logLik
#' @rdname xset_fitstats
#'
logLik.xpose_set <- function(object, ...) {
  xset_fitstat(object, function(x) as.numeric(stats::logLik(x)), ...)
}

#' @param k <`numeric`> Penalty per parameter, as in <[`stats::AIC`]>.
#' @exportS3Method stats::AIC
#' @rdname xset_fitstats
AIC.xpose_set <- function(object, ..., k = 2) {
  xset_fitstat(object, function(x) stats::AIC(x, k = k), ...)
}

#' @exportS3Method stats::BIC
#' @rdname xset_fitstats
BIC.xpose_set <- function(object, ...) {
  xset_fitstat(object, stats::BIC, ...)
}

# Internal: apply a per-model fit-statistic function across an xpose_set's
# lineage, following the same lineage-guessing rules as diff.xpose_set().
xset_fitstat <- function(xpdb_s, .fn, ...) {
  lineage <- xset_lineage(xpdb_s, ...)

  fitstat_fun <- function(line) {
    purrr::map_dbl(line, ~ .fn(xpdb_s[[.x]]$xpdb))
  }

  if (is.list(lineage)) return(purrr::map(lineage, fitstat_fun))

  fitstat_fun(lineage)
}
