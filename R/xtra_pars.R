## Functions for extra parameter handling functions

#' Describe parameter associations
#'
#' @description
#' The relationship between structural parameters and
#' omega parameters can be described. This is useful if
#' it deviates from the typical log-normal.
#'
#' Default transformations are those that are built into
#' `pmxcv`, but see examples for how associations can be
#' described for other relationships.
#'
#'
#' @param xpdb_x h
#' @param ... h
#' @param .problem h
#' @param .subprob h
#' @param .method h
#' @param quiet h
#'
#' @details
#' At time of writing, the built-in distributions for `pmxcv` are below.
#' Those marked with an asterisk require a fixed effect parameter to calculate CV.
#' \itemize{
#'   \item `log` typical log-normal. Optional `exact` parameter.
#'   \item `logexp`* modified log-normal `log(1+X)`
#'   \item `logit`* logit-normal
#'   \item `arcsin`* arcsine-transform
#'   \item `nmboxcox`* Box-Cox transform as typically implemented in pharmacometrics.
#' Requires a `lambda` parameter.
#' }
#'
#' To pass a custom parameter, use `custom` transform, and pass `pdist` and `qdist` to
#' that transform. See Examples.
#'
#'
#' @references
#' Prybylski, J.P. Reporting Coefficient of Variation for Logit, Box-Cox and
#' Other Non-log-normal Parameters. Clin Pharmacokinet 63, 133–135 (2024).
#' https://doi.org/10.1007/s40262-023-01343-2
#'
#' @seealso [pmxcv::dist.intcv()]
#'
#' @export
#'
#' @examples
#' # example codes
#'
#'
add_prm_association <- function(
    xpdb_x,
    ...,
    .problem = NULL,
    .subprob = NULL,
    .method = NULL,
    quiet
) {

}




########
# Method
########

#' Access model parameters
#'
#' @rdname get_prm
#'
#' @description
#' Access model parameter estimates from an xpdb object.
#'
#' Methods have been added to implement extensions. See Details.
#'
#' @inheritParams xpose::get_prm
#'
#' @return
#' A tibble for single problem/subprob or a named list for multiple problem|subprob.
#' @export
#'
#' @seealso [add_prm_association()]
#'
#' @details
#' When using an <`xp_xtra`> object, this function will add a column to the output
#' where CV% for each diagonal element of omega is calculated. This CV% is with
#' respect to the resulting structural parameter, so unless the default log-normal
#' association is applicable update with [`add_prm_association`].
#'
#' For log-normal, users may prefer to use the first-order CV% (\eqn{\sqrt{\omega^2}})
#' instead of the exact. In such case, `xpdb <- set_option(xpdb, cvtype="sqrt")` will
#' get that preferred form.
#'
#' @references
#' Prybylski, J.P. Reporting Coefficient of Variation for Logit, Box-Cox and
#' Other Non-log-normal Parameters. Clin Pharmacokinet 63, 133–135 (2024).
#' https://doi.org/10.1007/s40262-023-01343-2
#'
#'
#' @examples
#'
#' # xpose parameter table
#' get_prm(xpose::xpdb_ex_pk, .problem = 1)
#'
#' # xpose.xtra parameter table (basically the same)
#' get_prm(pheno_final, .problem = 1)
#'
#' # For the sake of example, even though these were all lognormal:
#' pheno_final %>%
#'   add_prm_association(CLpkg~logit(IIVCL)) %>%
#'   add_prm_association(Vpkg~nmboxcox(IIVV, lambda = 0.01)) %>%
#'   get_prm(.problem = 1)
#'
#'
get_prm <- function(
  xpdb,
  .problem = NULL,
  .subprob = NULL,
  .method = NULL,
  digits = 4,
  transform = TRUE,
  show_all = FALSE,
  quiet
) {
  UseMethod("get_prm")
}

#' @method get_prm default
#' @export
get_prm.default <- function(
    xpdb,
    .problem = NULL,
    .subprob = NULL,
    .method = NULL,
    digits = 4,
    transform = TRUE,
    show_all = FALSE,
    quiet
) {
  if (check_xpdb_x(xpdb, .warn = FALSE)) {
    return(get_prm.xp_xtras(xpdb=xpdb, .problem=.problem, .subprob = .subprob,
                            .method = .method, transform = transform,
                            show_all = show_all, quiet=quiet))
  }

  xpose::get_prm(xpdb=xpdb, .problem=.problem, .subprob = .subprob,
                 .method = .method, digits = digits, transform = transform,
                 show_all = show_all, quiet=quiet)
}

#' @method get_prm default
#' @export
get_prm.xp_xtras <- function(
    xpdb,
    .problem = NULL,
    .subprob = NULL,
    .method = NULL,
    digits = reportable_digits(xpdb),
    transform = TRUE,
    show_all = FALSE,
    quiet
) {
  # Use sqrt for lognormal? (change this later, it's just here as a reminder)
  sqrt_lnorm <- xpdb$options$cvtype=="sqrt"
}
