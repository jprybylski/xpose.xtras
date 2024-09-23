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
#' @param xpdb <`xp_xtras`> object
#' @param ... ... <[`dynamic-dots`][rlang::dyn-dots]> One or more formulas that
#' define associations between parameters.
#' One list of formulas can also be used, but a warning is generated.
#' @param .problem <`numeric`> Problem number to apply this relationship.
#' @param .subprob <`numeric`> Problem number to apply this relationship.
#' @param .method <`numeric`> Problem number to apply this relationship.
#' @param quiet Silence extra output.
#'
#' @details
#' At time of writing, the built-in distributions for `pmxcv` are below.
#' Those marked with an asterisk require a fixed effect parameter to calculate CV.
#' \itemize{
#'   \item `log` typical log-normal. Optional `exact` parameter (if `TRUE`, will not
#'   calculate with integration); this is unrelated to the `cvtype` option.
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
#' Note that the functions used in describing associations are not real functions,
#' it is just the syntax for this application. Based on examples, be mindful of
#' where positional arguments would acceptable and where named arguments are
#' required. Care has been given to provide a modest amount of flexibility
#' with informative errors for fragile points, but not every error can be anticipated.
#' If this function or downstream results from it seem wrong, the association syntax
#' should be scrutinized.
#'
#' Format for associations is:
#' `LHS~fun(OMEGA, args...)`
#'
#' \itemize{
#'   \item LHS: Selector for a fixed effect parameter. Can be `the{m}` (eg, the1),
#'   `{name}` (eg, THETA1) or `{label}` (eg, TVCL). These should *not* be quoted.
#'   Multiple associations can be defined at once with `+`. Cannot be empty.
#'   \item RHS: Should be a simple call to only one function, which should be
#'   custom or one of the built-in distributions or `custom(...)`. A lot of things
#'   can look like simple calls, so may not break immediately; keep to the described
#'   format and everything should be fine.
#'   \item RHS OMEGA: Selector for omega variable. Similar rules to the fixed
#'   effect selector. Can be `ome{m}`, `{name}` or `{label}`, limited to diagonal
#'   elements. Should *not* be quoted. `OMEGA` is not a named argument (`OMEGA={selector}`
#'   should **not** be considered valid); whatever is used as the first argument to the
#'   "function" will be considered an OMEGA selector.
#'   \item RHS args: Applies when the distribution has extra arguments. If these
#'   are limited to 1, can be passed by position (eg, `lambda` for `nmboxcox` and
#'   `exact` for `log`). For `custom()`, `qdist`, `pdist` and any arguments needed to
#'   pass to them should be named.
#' }
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
#'
#' pheno_base %>%
#'    add_prm_association(the1~log(IIVCL),V~log(IIVV))
#'
#'
add_prm_association <- function( # TODO: add more examples of parameters, including custom
  xpdb,
  ...,
  .problem,
  .subprob,
  .method,
  quiet
) {
  if (!check_xpdb_x(xpdb, .warn = TRUE))
    cli::cli_abort("{cli::col_blue('xp_xtras')} object required.")

  fill_prob_subprob_method(xpdb, .problem=.problem, .subprob=.subprob,.method=.method)

  assc_list <- rlang::list2(...) # List of formulas (hopefully)
  # Allow a list to be passed to ... given add_relationship behavior
  if (length(assc_list)>=1 && is.list(assc_list[[1]])) {
    if (.warn) rlang::warn("List should not be used in dots, but is allowed; instead pass as arguments or pass list with !!!list.")
    assc_list <- assc_list[[1]]
  }

  # Validate input
  ## Return base object if no associations are provided
  if (rlang::dots_n(...)==0) return(xpdb)
  ## Check that formulas are valid
  check_associations(assc_list=assc_list, xpdb=xpdb, .problem=.problem, .subprob=.subprob,.method=.method)

  # Process
  assoc_proc <- proc_assc(assc_list, .problem=.problem, .subprob=.subprob,.method=.method)
  got_prm <- xpose::get_prm(xpdb=xpdb, .problem=.problem, .subprob=.subprob,.method=.method, quiet=TRUE)

  ## Make sure any existing param associations that would be overwritten are overwritten
  if (nrow(xpdb$pars)>0) {
    subpars <- xpdb$pars %>% dplyr::filter(problem==.problem,subprob==.subprob,method==.method)
    existing_sels <- param_selector(subpars$param, got_prm)
    new_sels <- param_selector(assoc_proc$param, got_prm)
    # Use existing selector for rows_update
    if (any(new_sels %in% existing_sels))
      assoc_proc$param[new_sels %in% existing_sels] <-
        subpars$param[
          match(
            new_sels[new_sels %in% existing_sels],
            existing_sels
            )
          ]
  }

  ### Set pars
  xpdb$pars <- xpdb$pars %>%
    dplyr::rows_upsert(
      assoc_proc,
      by = c("param","problem","subprob","method")
    )
  as_xpdb_x(xpdb)
}


#'
#' @inheritParams add_prm_association
#' @param ... <`character`> One or more selectors for associations to drop.
#'
#' @export
drop_prm_association <- function(
  xpdb,
  ...,
  .problem,
  .subprob,
  .method,
  quiet
) {
  if (!check_xpdb_x(xpdb, .warn = TRUE))
    cli::cli_abort("{cli::col_blue('xp_xtras')} object required.")

  fill_prob_subprob_method(xpdb, .problem=.problem, .subprob=.subprob,.method=.method)

}

builtin_asscs <- c("log","logexp","logit","arcsin","nmboxcox")
check_associations <- function(
    assc_list,
    xpdb,
    ...,
    .problem,
    .subprob,
    .method
    ) {
  # Check that associations are valid

  # Confirm list of formulas
  if (
    length(assc_list)==0 ||
    !is_formula_list(assc_list)
  ) {
    rlang::abort("Associations must be a list of formulas.")
  }

  # Basic checks that should not be done in processing
  for (fmla in assc_list) {

    # Check for non-empty lhs
    if (!rlang::is_formula(fmla, lhs=TRUE))
      cli::cli_abort("LHS of formula cannot be empty in `{cli::code_highlight(deparse(fmla))}`")

    # Check that rhs is a function call
    if (class(fmla[[3]])!="call")
      cli::cli_abort("RHS of formula must be a function call, not a {.strong {class(fmla[[3]])}}")

    # Check that rhs is a simple* function call
    valid_funs <- c(builtin_asscs, "custom")
    if (!deparse(fmla[[3]][[1]]) %in% valid_funs)
      cli::cli_abort("RHS of formula must be a call to one of {valid_funs}, not `{deparse(fmla[[3]][[1]])}`")

    # Check that nmboxcox has more than 1 args
    if (deparse(fmla[[3]][[1]])=="nmboxcox" && length(rlang::call_args(fmla[[3]]))<2)
      cli::cli_abort("The `nmboxcox` distribution requires a second argument (for lambda) not seen here `{deparse(fmla[[3]])}`")

    # Check that custom has at least 3 args
    if (deparse(fmla[[3]][[1]])=="custom" && length(rlang::call_args(fmla[[3]]))<3)
      cli::cli_abort("The `custom` distribution requires more arguments (for `qdist` and `pdist`) not seen here `{deparse(fmla[[3]])}`")
  }

  # All symbols
  sym_tab <- proc_assc(assc_list, .problem=.problem, .subprob=.subprob,.method=.method)
  par_tbl <- xpose::get_prm(xpdb, .problem=.problem, .subprob=.subprob,.method=.method, quiet = TRUE)
  rlang::try_fetch({
    fepars <- sym_tab$param %>% param_selector(prm_tbl = par_tbl)
    repars <- sym_tab$omega %>% param_selector(prm_tbl = par_tbl)
  },
    error = function(s)
      rlang::abort("Non-valid selectors in association.", parent=s)
  )
  # Check for fepars repeats (repars is fine to repeat)
  if (any(duplicated(fepars)))
    cli::cli_abort("Cannot have multiple associations for the same (fixed effect) parameters. ({sym_tab$param[duplicated(fepars)]})")

  return()
}

# Process associations list
# This is used in as_xp_xtras, so changes to behavior should be made with caution
proc_assc <- function(assc_list,.problem,.subprob,.method) {
  purrr::map_dfr(
    assc_list,
    ~ {
      # Extract symbol(s) in lhs
      lhs <- all.vars(.x[[2]])
      # Parts of the rhs function call
      rhfun <- deparse(.x[[3]][[1]])
      rhargs <- rlang::call_args(.x[[3]])
      omearg <- deparse(rhargs[[1]])


      # Create a tibble
      tibble::tibble(
        param = lhs,
        assoc = rhfun,
        omega = omearg,
        argus = list(tail(rhargs, -1)),
        problem = .problem,
        subprob = .subprob,
        method = .method
      )
    }
  )
}

#' Select parameter row number
#'
#' @description
#' The selection rules are described in <[`add_prm_association`]>.
#'
#' @param sel <`character`> Selector of any parameter
#' @param prm_tbl <`tibble`> Like the output of `get_prm()`
#'
#' @return <`integer`> of selected row number. Can be used to
#' get value and other elements of information from `prm_tbl`.
#'
#' @keywords internal
param_selector <- function(
  sel,
  prm_tbl
) {
  if (missing(prm_tbl) || !is.data.frame(prm_tbl)) {
    rlang::abort("This function requires a parameter table like that from `get_prm()")
  }
  if (length(sel)>1) return(purrr::map_int(sel, param_selector, prm_tbl=prm_tbl))
  if (!is.character(sel) || sel=="")
    cli::cli_abort("Selector must be a non-empty character vector.")

  candidates <- c()
  # First check by type-index
  typem_names <- paste0(prm_tbl$type,prm_tbl$m)
  if (sel %in% typem_names)
    candidates <- which(typem_names==sel)
  # Check name
  if (sel %in% prm_tbl$name)
    candidates <- which(prm_tbl$name==sel)
  # Check label
  if (sel %in% prm_tbl$label)
    candidates <- which(prm_tbl$label==sel)
  if (length(candidates)==0)
    cli::cli_abort("Selector {.strong {sel}} does not match any valid rows.")

  # only diagonal, where it applies
  ret_val <- candidates[is.na(prm_tbl$diagonal[candidates]) | prm_tbl$diagonal[candidates]==TRUE]
  if (length(ret_val)==0)
    cli::cli_abort("Selector {.strong {sel}} does not match any valid, non-diagonal rows.")
  if (length(ret_val)>1)
    cli::cli_abort("Selector {.strong {sel}} does not unambiguously match any one row (matches {length(ret_val)}).")
  ret_val
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
  if (!check_xpdb_x(xpdb, .warn = TRUE))
    cli::cli_abort("{cli::col_blue('xp_xtras')} object required.")

  fill_prob_subprob_method(xpdb, .problem=.problem, .subprob=.subprob,.method=.method)

  def_prm <- xpose::get_prm(xpdb=xpdb, .problem=.problem, .subprob = .subprob,
                   .method = .method, digits = digits, transform = transform,
                   show_all = show_all, quiet=quiet)


  # Use sqrt for lognormal? (change this later, it's just here as a reminder)
  sqrt_lnorm <- xpdb$options$cvtype=="sqrt"

  # Get basic
}
