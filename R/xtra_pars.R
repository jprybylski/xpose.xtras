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
#' **_Note:_** When these associations are used to calculate CV%, it is assumed
#' that the value for the theta parameter is _untransformed_. So, if a parameter is
#' fitted in the logit scale, the value should be transformed back to normal scale
#' with [`mutate_prm()`] (eg, `mutate_prm(the~plogis`) before declaring `the~logit(ome)`.
#'
#' @rdname add_prm_association
#'
#' @param xpdb <`xp_xtras`> object
#' @param ... ... <[`dynamic-dots`][rlang::dyn-dots]> One or more formulas that
#' define associations between parameters. One list of formulas can also be used,
#' but a warning is generated.
#'
#' For `drop_prm_association`, these dots should be selectors for which associations
#' will be dropped (`the2, the3,...`). Fixed effect selectors only will work.
#' @param .problem <`numeric`> Problem number to apply this relationship.
#' @param .subprob <`numeric`> Problem number to apply this relationship.
#' @param .method <`numeric`> Problem number to apply this relationship.
#' @param quiet Silence extra output.
#'
#' @details
#' At time of writing, the built-in distributions for `pmxcv` are below.
#' Those marked with an asterisk require a fixed effect parameter to calculate CV.
#' \itemize{
#'   \item `log` typical log-normal. Optional `exact` parameter (if `TRUE`, default, will not
#'   calculate with integration); this is unrelated to the `cvtype` option. **Note**,
#'   if `cvtype` option is set to `"sqrt"`, log-normal `get_prm` CVs will use the square root,
#'   not any integration or analytical estimate, regardless of how this association is specified.
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
#' Reminder about `qdist` and `pdist`: Consider that `qlogis` transforms a proportion
#' to a continuous, unbounded number; it is the `logit` transform. The `plogis` function
#' converts a continuous, unbounded number to a proportion; it is the *inverse* `logit`
#' transform. Other R `stats` functions work similarly, and as such functions used as
#' `qdist` and `pdist` values are expected to act similarly.
#'
#' Note that the functions used in describing associations are not real functions,
#' it is just the syntax for this application. Based on examples, be mindful of
#' where positional arguments would acceptable and where named arguments are
#' required. Care has been given to provide a modest amount of flexibility
#' with informative errors for fragile points, but not every error can be anticipated.
#' If this function or downstream results from it seem wrong, the association syntax
#' should be scrutinized. These "functions" are not processed like in [`mutate_prm`],
#' so (eg) `the2` will not be substituted for the value of `the2`; if
#' `lambda` is a fitted value (like `the2`), in that edge case the value of `the2` should be written
#' explicitly in the association formula, and if any `mutate_prm` changes `the2` then users
#' should be mindful of the new association needed. This may be updated in the future.
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
#'   "function" will be considered an OMEGA selector. **NOTE**, if selecting an OMEGA parameter
#'   by name (eg, `OMEGA(2,2)`), backticks (eg \code{`OMEGA(2,2)`}) must be used or else the selection will throw
#'   an error.
#'   \item RHS args: Applies when the distribution has extra arguments. If these
#'   are limited to 1, can be passed by position (eg, `lambda` for `nmboxcox` and
#'   `exact` for `log`). For `custom()`, `qdist`, `pdist` and any arguments needed to
#'   pass to them should be named.
#' }
#'
#' For the `nmboxcox` transformation, a lambda value (especially negative ones) may
#' not work well with the integration-based CV estimation. This may occur even if
#' the lambda is fitted and stable in that fitting, but it cannot be predicted which
#' ones will be affected. This note is intended to forewarn that this might happen.
#'
#'
#'
#' @references
#' Prybylski, J.P. Reporting Coefficient of Variation for Logit, Box-Cox and
#' Other Non-log-normal Parameters. Clin Pharmacokinet 63, 133-135 (2024).
#' \doi{doi:10.1007/s40262-023-01343-2}
#'
#' @seealso [`dist.intcv`][pmxcv::dist.intcv()]
#'
#' @export
#'
#' @returns An updated `xp_xtras` object
#'
#' @examples
#'
#' pheno_base %>%
#'    add_prm_association(the1~log(IIVCL),V~log(IIVV)) %>%
#'    get_prm() # get_prm is the only way to see the effect of associations
#'
#' # These values are not fitted as logit-normal, but
#' # just to illustrate:
#' pheno_final %>%
#'    add_prm_association(the1~logit(IIVCL),Vpkg~logit(IIVV)) %>%
#'    get_prm()
#'
#' # ... same for Box-Cox
#' pheno_base %>%
#'    add_prm_association(V~nmboxcox(IIVV, lambda=0.5)) %>%
#'    # Naming the argument is optional
#'    add_prm_association(CL~nmboxcox(IIVCL, -0.1)) %>%
#'    get_prm()
#'
#' # A 'custom' use-case is when logexp, log(1+X), is
#' # desired but 1 is too large.
#' # Again, for this example, treating this like it applies here.
#' pheno_base %>%
#'   add_prm_association(V~custom(IIVV, qdist=function(x) log(0.001+x),
#'         pdist=function(x) exp(x)-0.001)) %>%
#'    get_prm()
#'
#' # Dropping association is easy
#' bad_assoc <- pheno_final %>%
#'    add_prm_association(the1~logit(IIVCL),Vpkg~logit(IIVV))
#' bad_assoc %>% get_prm()
#' bad_assoc %>%
#'   drop_prm_association(the1) %>%
#'   get_prm()
#'
add_prm_association <- function(
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
    rlang::warn("List should not be used in dots, but is allowed; instead pass as arguments or pass list with !!!list.")
    assc_list <- assc_list[[1]]
  }

  # Validate input
  ## Return base object if no associations are provided
  if (rlang::dots_n(...)==0) return(xpdb)
  ## Check that formulas are valid
  check_associations(assc_list=assc_list, xpdb=xpdb, .problem=.problem, .subprob=.subprob,.method=.method)

  # Process
  assoc_proc <- proc_assc(assc_list, .problem=.problem, .subprob=.subprob,.method=.method)
  rlang::try_fetch(
    got_prm <- hot_swap_base_get_prm(xpdb=xpdb, .problem=.problem, .subprob=.subprob,.method=.method, quiet=TRUE),
    error = function(s)
      rlang::abort(
        paste0("Error getting current available parameters. If using SAEM or Monte Carlo methods, ",
        "this is a known issue in the base `xpose` package."),
        parent = s
      )
  )
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


#' @rdname add_prm_association
#'
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

  rlang::check_dots_unnamed()

  remove_selectors <- rlang::exprs(...) %>%
    purrr::map_chr(~{
      out <- try(eval(.x), silent=TRUE)
      if ("try-error" %in% class(out)) out <- deparse(.x)
      out
    })
  current_selectors <- xpdb$pars %>%
    dplyr::filter(
      problem == .problem,
      subprob == .subprob,
      method == .method
    )
  if (nrow(current_selectors)==0) return(xpdb)
  par_tbl <- hot_swap_base_get_prm(xpdb, .problem=.problem, .subprob=.subprob,.method=.method, transform = FALSE, quiet = TRUE)

  current_pars <- param_selector(current_selectors$param, prm_tbl = par_tbl)
  rlang::try_fetch({
    todrop <- param_selector(remove_selectors, prm_tbl = par_tbl)
  },
  error = function(s)
    rlang::abort("Non-valid selectors in association.", parent=s)
  )
  if (!any(todrop %in% current_pars)) {
    return(xpdb)
  }

  new_xpdb <- xpdb
  new_xpdb$pars <- new_xpdb$pars %>%
    dplyr::filter(
      magrittr::not(
        current_pars %in% todrop &
          problem == .problem &
          subprob == .subprob &
          method == .method

      )
    )
  as_xpdb_x(new_xpdb)
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
    if (!inherits(fmla[[3]],"call"))
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
  rlang::try_fetch(
    par_tbl <- hot_swap_base_get_prm(xpdb, .problem=.problem, .subprob=.subprob,.method=.method, transform = FALSE, quiet = TRUE),
    error = function(s)
      rlang::abort(
        paste0("Error getting current available parameters. If using SAEM or Monte Carlo methods, ",
               "this is a known issue in the base `xpose` package."),
        parent = s
      )
  )
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

# Empty `xpdb$covs` tibble, defining the storage schema for parameter/covariate
# associations (see add_cov_association()). Used in as_xpdb_x(), so changes to
# the schema should be made with caution -- anything reading xpdb$covs should
# tolerate these columns/types.
empty_covs_tbl <- function() {
  tibble::tibble(
    param = character(),     # LHS selector, as typed (e.g. "the1")
    covariate = character(), # RHS covariate column selector (e.g. "WT")
    covtype = character(),   # "cont" or "cat"
    assoc = character(),     # one of builtin_cov_asscs or "custom"
    thetas = list(),         # list<character>: 1+ theta selectors (one per non-ref level for multi-level catshift)
    ref = list(),            # required reference covariate value/level (no implicit default)
    argus = list(),          # extra named args (e.g. custom()'s `fun`)
    problem = numeric(),
    subprob = numeric(),
    method = character()
  )
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
      rhargus <- tail(rhargs, -1) %>% purrr::map(eval)


      # Create a tibble
      tibble::tibble(
        param = lhs,
        assoc = rhfun,
        omega = omearg,
        argus = list(rhargus),
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
  if (length(sel)==0) return(integer())
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
  as.integer(ret_val)
}

########################
# Covariate associations
########################

#' Describe parameter/covariate associations
#'
#' @description
#' The relationship between a structural parameter and a covariate can be
#' described, so that the covariate's effect on that parameter -- and the
#' uncertainty of that effect -- can later be visualized with
#' [`xplot_forest()`] (via [`prm_cov()`]/[`prm_contcov()`]/[`prm_catcov()`]).
#'
#' This is deliberately parallel to [`add_prm_association()`]: the same
#' formula-based declaration style, the same two-stage
#' check-then-process validation, and the same "redeclare to replace"
#' upsert behavior. It is a separate mechanism (own storage, own getters)
#' because a covariate association needs more shape than an omega
#' association -- a covariate column, a *required* reference value, and
#' (for categorical covariates or `custom()`) more than one theta -- and
#' it produces a *range* of effect sizes rather than a single scalar CV.
#'
#' @rdname add_cov_association
#'
#' @param xpdb <`xp_xtras`> object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more formulas that
#' define associations between a parameter and a covariate. One list of
#' formulas can also be used, but a warning is generated.
#'
#' For `drop_cov_association`, these should be formulas of the form
#' `param ~ covariate` (both bare, unquoted selectors; `covariate` must be
#' the literal covariate column name as declared).
#' @param .problem <`numeric`> Problem number to apply this relationship.
#' @param .subprob <`numeric`> Subprob number to apply this relationship.
#' @param .method <`numeric`> Method to apply this relationship.
#' @param quiet Silence extra output.
#'
#' @details
#' Format for associations is:
#'
#' `LHS ~ fun(COVARIATE, THETA..., ref = ..., ...)`
#'
#' \itemize{
#'   \item LHS: Selector for a fixed-effect (theta) parameter, exactly as
#'   in [`add_prm_association()`] (`the{m}`, `{name}` or `{label}`, unquoted).
#'   Multiple parameters can share one association with `+` (eg, a
#'   covariate that affects both `CL` and `Q` through the same theta).
#'   \item RHS `COVARIATE`: The first (positional) argument. The bare,
#'   unquoted name of a `contcov`/`catcov` column (see [`xpose::xp_var()`]) --
#'   *not* a fixed-effect or omega selector.
#'   \item RHS `THETA...`: One or more further positional arguments,
#'   selecting the fixed-effect parameter(s) that carry the covariate
#'   effect magnitude (same selector rules as LHS -- `the{m}`/`{name}`/
#'   `{label}`, unquoted). How many are expected depends on `fun`; see the
#'   built-in list below.
#'   \item RHS `ref`: **Required, named, no default**, for every
#'   association regardless of `fun`. This is the covariate value (for
#'   continuous covariates) or raw level (for categorical covariates)
#'   that the effect is normalized against -- the point at which the
#'   reported effect ratio is exactly `1`. There is no way to safely infer
#'   this from the `xpdb` alone (NONMEM control streams commonly
#'   normalize a covariate against a hardcoded constant baked into the
#'   code, which is invisible to `xpose`), so it must always be stated
#'   explicitly, the same way [`add_prm_association()`]'s `nmboxcox`
#'   requires an explicit `lambda`.
#' }
#'
#' All built-ins express the covariate's effect as a multiplicative
#' `effect_ratio` on the parameter's typical value, and are constructed so
#' that `effect_ratio == 1` whenever the covariate equals `ref`,
#' *regardless of the theta value*. Available built-ins:
#'
#' \itemize{
#'   \item `linear(COV, THETA, ref=)`: \eqn{1 + \theta (COV - ref)}
#'   \item `power(COV, THETA, ref=)`: \eqn{(COV / ref)^\theta} (allometric)
#'   \item `exponential(COV, THETA, ref=)`: \eqn{e^{\theta (COV - ref)}}
#'   \item `additive(COV, THETA, ref=)`: \eqn{(\theta + COV) / (\theta + ref)}.
#'   An uncommon but simple form where the covariate is added directly (with
#'   an implicit coefficient of `1`, unlike `linear`'s explicit slope) to an
#'   intercept-like `THETA`, eg `CL = THETA(n) + WT` in the underlying model.
#'   \item `hockey(COV, THETA_LO, THETA_HI, ref=, brk=ref)`: PsN's
#'   "hockey-stick" two-slope piecewise-linear model --
#'   \eqn{1 + \theta_{lo} (COV - ref)} when `COV <= brk`,
#'   \eqn{1 + \theta_{hi} (COV - ref)} when `COV > brk`. `brk` (the
#'   breakpoint) defaults to `ref` (PsN's usual default: breakpoint =
#'   normalization reference), but can be given separately, eg for a
#'   covariate normalized to its observed median while the clinically
#'   meaningful cutpoint is a round number (`ref = 90, brk = 60` for an
#'   eGFR-like covariate).
#'   \item `catshift(COV, THETA..., ref=)`: One theta per non-reference
#'   raw level of a categorical covariate, `effect_ratio = 1 + THETA_i`
#'   for level `i` (`1` at `ref`). Assumes the typical NONMEM pattern of
#'   one theta per non-reference category (eg
#'   `IF (RACE.EQ.2) CLCOV = THETA(9)`). Thetas are matched to
#'   non-reference levels in ascending raw-value order -- if that order
#'   is ambiguous or wrong for a given model, use `custom()` instead.
#' }
#'
#' For anything else, `custom(COV, THETA..., ref=, fun=)` is the escape
#' hatch: `fun` is a function of `(cov, ref, theta)` (`theta` is always a
#' numeric vector, even when only one `THETA` selector is given) returning
#' the effect ratio. Because `custom()` can't be verified by
#' construction the way the built-ins can, `add_cov_association()`
#' validates it at declaration time by evaluating `fun(ref, ref, theta)`
#' for a few probe values of `theta` (not the currently-fitted value,
#' which could coincidentally pass while `fun` is still wrong for other
#' theta values) and requires each to equal `1`; if it doesn't, the error
#' states the required invariant and shows what `fun` actually returned,
#' rather than failing silently or only much later during plotting.
#'
#' @seealso [`add_prm_association()`], [`prm_cov()`]
#'
#' @export
#'
#' @returns An updated `xp_xtras` object
#'
#' @examples
#'
#' # xpdb_x's THETA7 ("CRCL on CL") is a genuine covariate effect already
#' # in the model, so this is a faithful (if allometric-flavored, for
#' # illustration) description of it:
#' xpdb_x %>%
#'   add_cov_association(TVCL ~ power(CLCR, THETA7, ref = 64)) %>%
#'   prm_cov()
#'
#' # hockey-stick (PsN-style): different slope above/below the reference
#' xpdb_x %>%
#'   add_cov_association(TVCL ~ hockey(CLCR, THETA7, THETA4, ref = 64)) %>%
#'   prm_cov()
#'
#' # Categorical: one theta per non-reference level. SEX has 2 levels
#' # (1, 2), so catshift needs exactly one theta for the non-reference
#' # level; THETA4 is reused here purely for illustration.
#' xpdb_x %>%
#'   add_cov_association(TVCL ~ catshift(SEX, THETA4, ref = 1)) %>%
#'   prm_cov()
#'
#' # custom(): fun(cov, ref, theta) must equal 1 when cov == ref
#' xpdb_x %>%
#'   add_cov_association(
#'     TVCL ~ custom(CLCR, THETA7, ref = 64,
#'                 fun = function(cov, ref, theta) (cov/ref)^theta)
#'   ) %>%
#'   prm_cov()
#'
#' # Dropping an association is easy
#' bad_assoc <- xpdb_x %>%
#'   add_cov_association(TVCL ~ power(CLCR, THETA7, ref = 64))
#' bad_assoc %>%
#'   drop_cov_association(TVCL ~ CLCR) %>%
#'   prm_cov()
#'
add_cov_association <- function(
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
    rlang::warn("List should not be used in dots, but is allowed; instead pass as arguments or pass list with !!!list.")
    assc_list <- assc_list[[1]]
  }

  # Validate input
  ## Return base object if no associations are provided
  if (rlang::dots_n(...)==0) return(xpdb)
  ## Check that formulas are valid
  check_cov_associations(assc_list=assc_list, xpdb=xpdb, .problem=.problem, .subprob=.subprob,.method=.method)

  # Process
  cov_proc <- proc_cov_assc(assc_list, .problem=.problem, .subprob=.subprob,.method=.method)
  cov_proc$covtype <- cov_covtype(cov_proc$covariate, xpdb=xpdb, .problem=.problem)

  rlang::try_fetch(
    got_prm <- hot_swap_base_get_prm(xpdb=xpdb, .problem=.problem, .subprob=.subprob,.method=.method, quiet=TRUE),
    error = function(s)
      rlang::abort(
        paste0("Error getting current available parameters. If using SAEM or Monte Carlo methods, ",
        "this is a known issue in the base `xpose` package."),
        parent = s
      )
  )
  ## Make sure any existing (param, covariate) associations that would be
  ## overwritten are overwritten regardless of which valid selector form
  ## was used to name the parameter (mirrors add_prm_association's logic)
  if (nrow(xpdb$covs)>0) {
    subcovs <- xpdb$covs %>% dplyr::filter(problem==.problem,subprob==.subprob,method==.method)
    existing_sels <- param_selector(subcovs$param, got_prm)
    new_sels <- param_selector(cov_proc$param, got_prm)
    existing_key <- paste(existing_sels, subcovs$covariate)
    new_key <- paste(new_sels, cov_proc$covariate)
    if (any(new_key %in% existing_key)) {
      match_i <- match(new_key[new_key %in% existing_key], existing_key)
      cov_proc$param[new_key %in% existing_key] <- subcovs$param[match_i]
    }
  }

  ### Set covs
  xpdb$covs <- xpdb$covs %>%
    dplyr::rows_upsert(
      cov_proc,
      by = c("param","covariate","problem","subprob","method")
    )
  as_xpdb_x(xpdb)
}


#' @rdname add_cov_association
#'
#' @export
drop_cov_association <- function(
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

  rlang::check_dots_unnamed()

  if (rlang::dots_n(...)==0) return(xpdb)

  drop_list <- rlang::list2(...)
  for (fmla in drop_list) {
    if (!rlang::is_bare_formula(fmla) || !rlang::is_formula(fmla, lhs=TRUE))
      cli::cli_abort("`drop_cov_association` selectors must be formulas of the form `param ~ covariate`, not `{deparse(fmla)}`")
    if (!inherits(fmla[[3]], "name"))
      cli::cli_abort("RHS of `drop_cov_association` selector must be a bare covariate column name, not `{deparse(fmla[[3]])}`")
  }

  current <- xpdb$covs %>%
    dplyr::filter(problem==.problem, subprob==.subprob, method==.method)
  if (nrow(current)==0) return(xpdb)

  par_tbl <- hot_swap_base_get_prm(xpdb, .problem=.problem, .subprob=.subprob,.method=.method, transform = FALSE, quiet = TRUE)

  current_idx <- param_selector(current$param, par_tbl)
  rlang::try_fetch({
    drop_tbl <- purrr::map_dfr(drop_list, ~ tibble::tibble(
      par_i = param_selector(deparse(.x[[2]]), par_tbl),
      covariate = deparse(.x[[3]])
    ))
  },
  error = function(s)
    rlang::abort("Non-valid selectors in association.", parent=s)
  )

  current_key <- paste(current_idx, current$covariate)
  drop_key <- paste(drop_tbl$par_i, drop_tbl$covariate)
  keep <- !current_key %in% drop_key
  if (all(keep)) return(xpdb)

  new_xpdb <- xpdb
  new_xpdb$covs <- dplyr::bind_rows(
    xpdb$covs %>% dplyr::filter(!(problem==.problem & subprob==.subprob & method==.method)),
    current[keep, ]
  )
  as_xpdb_x(new_xpdb)
}

builtin_cov_asscs <- c("linear","power","exponential","hockey","additive","catshift")

# Resolve each covariate selector to "cont"/"cat"/NA (NA meaning it does
# not match any declared contcov/catcov column for this problem)
cov_covtype <- function(covariate, xpdb, .problem) {
  valid_cont <- xpose::xp_var(xpdb, .problem, type = "contcov", silent = TRUE)$col
  valid_cat  <- xpose::xp_var(xpdb, .problem, type = "catcov", silent = TRUE)$col
  dplyr::case_when(
    covariate %in% valid_cont ~ "cont",
    covariate %in% valid_cat  ~ "cat",
    TRUE ~ NA_character_
  )
}

check_cov_associations <- function(
    assc_list,
    xpdb,
    ...,
    .problem,
    .subprob,
    .method
) {
  # Confirm list of formulas
  if (
    length(assc_list)==0 ||
    !is_formula_list(assc_list)
  ) {
    rlang::abort("Associations must be a list of formulas.")
  }

  valid_funs <- c(builtin_cov_asscs, "custom")

  # Basic per-formula shape checks that don't need xpdb state
  for (fmla in assc_list) {

    if (!rlang::is_formula(fmla, lhs=TRUE))
      cli::cli_abort("LHS of formula cannot be empty in `{cli::code_highlight(deparse(fmla))}`")

    if (!inherits(fmla[[3]],"call"))
      cli::cli_abort("RHS of formula must be a function call, not a {.strong {class(fmla[[3]])}}")

    fun_name <- deparse(fmla[[3]][[1]])
    if (!fun_name %in% valid_funs)
      cli::cli_abort("RHS of formula must be a call to one of {valid_funs}, not `{fun_name}`")

    call_args <- rlang::call_args(fmla[[3]])
    arg_names <- rlang::names2(call_args)
    positional <- call_args[arg_names==""]

    if (length(positional)<1)
      cli::cli_abort("RHS of `{fun_name}` must have a covariate as its first (positional) argument in `{deparse(fmla[[3]])}`")

    if (!"ref" %in% arg_names)
      cli::cli_abort("`ref` is a required named argument for every covariate association (no implicit default) in `{deparse(fmla[[3]])}`")

    n_thetas <- length(positional) - 1L # first positional is the covariate
    if (fun_name %in% c("linear","power","exponential","additive") && n_thetas != 1)
      cli::cli_abort("`{fun_name}` requires exactly one theta selector, found {n_thetas} in `{deparse(fmla[[3]])}`")
    if (fun_name == "hockey" && n_thetas != 2)
      cli::cli_abort("`hockey` requires exactly two theta selectors (low-side, high-side, in that order), found {n_thetas} in `{deparse(fmla[[3]])}`")
    if (fun_name == "catshift" && n_thetas < 1)
      cli::cli_abort("`catshift` requires at least one theta selector (one per non-reference level), found {n_thetas} in `{deparse(fmla[[3]])}`")
    if (fun_name == "custom") {
      if (n_thetas < 1)
        cli::cli_abort("`custom` requires at least one theta selector in `{deparse(fmla[[3]])}`")
      if (!"fun" %in% arg_names)
        cli::cli_abort("`custom` requires a named `fun` argument (a function of `(cov, ref, theta)`) in `{deparse(fmla[[3]])}`")
    }
  }

  # All symbols
  cov_tab <- proc_cov_assc(assc_list, .problem=.problem, .subprob=.subprob,.method=.method)

  rlang::try_fetch(
    par_tbl <- hot_swap_base_get_prm(xpdb, .problem=.problem, .subprob=.subprob,.method=.method, transform = FALSE, quiet = TRUE),
    error = function(s)
      rlang::abort(
        paste0("Error getting current available parameters. If using SAEM or Monte Carlo methods, ",
               "this is a known issue in the base `xpose` package."),
        parent = s
      )
  )

  rlang::try_fetch({
    fepars <- param_selector(cov_tab$param, prm_tbl = par_tbl)
  },
  error = function(s)
    rlang::abort("Non-valid parameter (LHS) selector(s) in association.", parent=s)
  )
  if (any(par_tbl$type[fepars]!="the"))
    cli::cli_abort("LHS of a covariate association must select a fixed-effect (theta) parameter.")

  all_thetas <- unlist(cov_tab$thetas)
  rlang::try_fetch({
    theta_idx <- param_selector(all_thetas, prm_tbl = par_tbl)
  },
  error = function(s)
    rlang::abort("Non-valid theta selector(s) in association.", parent=s)
  )
  if (length(theta_idx)>0 && any(par_tbl$type[theta_idx] != "the"))
    cli::cli_abort("Covariate-effect selectors must be fixed-effect (theta) parameters, not omega/sigma.")

  # Duplicate (parameter, covariate) pairs within this call
  fe_cov_pairs <- paste(fepars, cov_tab$covariate)
  if (any(duplicated(fe_cov_pairs)))
    cli::cli_abort("Cannot have multiple associations for the same (parameter, covariate) pair in a single call. ({cov_tab$covariate[duplicated(fe_cov_pairs)]})")

  # Covariate columns must be declared contcov/catcov columns
  cov_tab$covtype <- cov_covtype(cov_tab$covariate, xpdb=xpdb, .problem=.problem)
  if (any(is.na(cov_tab$covtype)))
    cli::cli_abort("Covariate selector(s) not found among declared `contcov`/`catcov` columns: {unique(cov_tab$covariate[is.na(cov_tab$covtype)])}")

  # Covtype/assoc compatibility + covtype-specific checks
  purrr::pwalk(cov_tab, function(param, covariate, covtype, assoc, thetas, ref, argus, ...) {
    if (covtype=="cat" && !assoc %in% c("catshift","custom"))
      cli::cli_abort("Covariate {.strong {covariate}} is categorical; only `catshift`/`custom` associations are valid for it, not `{assoc}`.")
    if (covtype=="cont" && assoc == "catshift")
      cli::cli_abort("Covariate {.strong {covariate}} is continuous; `catshift` is only valid for categorical covariates.")

    if (assoc=="catshift") {
      obs_levels <- xpose::get_data(xpdb, .problem=.problem, quiet=TRUE) %>%
        dplyr::pull(covariate) %>% unique() %>% sort()
      nonref_levels <- setdiff(obs_levels, ref)
      if (length(thetas) != length(nonref_levels))
        cli::cli_abort("`catshift` for {.strong {covariate}} needs one theta per non-reference level ({length(nonref_levels)} found: {nonref_levels}), but {length(thetas)} given.")
    }

    if (assoc=="custom") {
      fun <- argus$fun
      if (!is.function(fun))
        cli::cli_abort("`custom`'s `fun` argument must be a function.")
      probe_ok <- purrr::map_lgl(c(0,1,-1), function(tp) {
        theta_probe <- rep(tp, length(thetas))
        val <- tryCatch(fun(ref, ref, theta_probe), error = function(e) NA_real_)
        isTRUE(all.equal(val, 1))
      })
      if (!all(probe_ok))
        cli::cli_abort(c(
          "`custom` association for {.strong {covariate}} must satisfy `fun(ref, ref, theta) == 1` for any `theta` (this is what makes the effect ratio equal 1 at the reference covariate value).",
          "x" = "It did not for at least one probe theta value; double check the formula used in `fun`."
        ))
    }
  })

  return()
}

# Process covariate associations list into xpdb$covs-shaped rows (minus
# covtype, which needs xpdb access to resolve -- see cov_covtype()).
# Kept xpdb-free, mirroring proc_assc()'s design.
proc_cov_assc <- function(assc_list,.problem,.subprob,.method) {
  purrr::map_dfr(
    assc_list,
    ~ {
      lhs <- all.vars(.x[[2]])
      rhs_call <- .x[[3]]
      fun_name <- deparse(rhs_call[[1]])
      call_args <- rlang::call_args(rhs_call)
      arg_names <- rlang::names2(call_args)

      positional <- call_args[arg_names==""]
      named <- call_args[arg_names!=""]

      covariate <- deparse(positional[[1]])
      thetas <- purrr::map_chr(tail(positional, -1), deparse)

      ref <- eval(named[["ref"]])
      argus <- purrr::map(named[setdiff(names(named), "ref")], eval)

      tibble::tibble(
        param = lhs,
        covariate = covariate,
        covtype = NA_character_,
        assoc = fun_name,
        thetas = list(thetas),
        ref = list(ref),
        argus = list(argus),
        problem = .problem,
        subprob = .subprob,
        method = .method
      )
    }
  )
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
#' If a single omega parameter is associated with multiple fixed effect parameters,
#' the `cv` column will be a list. For the `omega` row associated with multiple
#' fixed effect parameters, there will be multiple CV values. This will be the case
#' even if the transformation is log-normal and therefore scale-invariant, given
#' the need for generality.
#'
#' **Note** the approach used to calculate CV% assumes an untransformed scale for the
#' fitted parameter value (unrelated to `transform`=TRUE). That means, for example,
#' that for a logit-normal fitted parameter value, it is expected the value will be
#' something constrained between 0 and 1, not the unbounded, continuous transformed value.
#' The function <[`mutate_prm`]> is intended to help where that might be an issue.
#'
#' @references
#' Prybylski, J.P. Reporting Coefficient of Variation for Logit, Box-Cox and
#' Other Non-log-normal Parameters. Clin Pharmacokinet 63, 133-135 (2024).
#' \doi{doi:10.1007/s40262-023-01343-2}
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
    quiet) {
  if (check_xpdb_x(xpdb, .warn = FALSE)) {
    return(get_prm.xp_xtras(
      xpdb = xpdb, .problem = .problem, .subprob = .subprob,
      .method = .method, transform = transform,
      show_all = show_all, quiet = quiet
    ))
  }

  implemented_prm_software <- c("nlmixr2")
  if (xpose::software(xpdb) %in% implemented_prm_software) {
    cli::cli_abort("For {.strong {xpose::software(xpdb)}} models, convert to {package_flex()} object before doing this action.")
  } else if (xpose::software(xpdb) != "nonmem") {
    cli::cli_abort("For {.strong {xpose::software(xpdb)}} models, {.emph extra} parameter functionality is not implemented.")
  }
  xpose::get_prm(
    xpdb = xpdb, .problem = .problem, .subprob = .subprob,
    .method = .method, digits = digits, transform = transform,
    show_all = show_all, quiet = quiet
  )
}

#' @method get_prm xp_xtras
#' @export
get_prm.xp_xtras <- function(
    xpdb,
    .problem = NULL,
    .subprob = NULL,
    .method = NULL,
    digits = NULL,
    transform = TRUE,
    show_all = FALSE,
    quiet
) {
  if (!check_xpdb_x(xpdb, .warn = TRUE))
    cli::cli_abort("{cli::col_blue('xp_xtras')} object required.")
  if (missing(quiet)) quiet <- xpdb$options$quiet
  fill_prob_subprob_method(xpdb, .problem=.problem, .subprob=.subprob,.method=.method)

  # TODO: Add a means to get equivalent table for nlmixr2 xpose data objects so associations can work

  if (is.null(digits)) digits <- reportable_digits(xpdb,.problem=.problem, .subprob=.subprob, .method=.method)

  if (xpose::software(xpdb)=="nonmem") {

    # Get basic param table
    def_prm <- xpose::get_prm(xpdb=xpdb, .problem=.problem, .subprob = .subprob,
                              .method = .method, digits = digits, transform = transform,
                              show_all = show_all, quiet=quiet)
    # Get untransformed param table
    untr_prm <- suppressWarnings(xpose::get_prm(xpdb=xpdb, .problem=.problem, .subprob = .subprob,
                                                .method = .method, digits = digits, transform = FALSE,
                                                show_all = show_all, quiet=TRUE))

  } else if (xpose::software(xpdb)=="nlmixr2") {
    assert_nlmixr2fit(xpdb)

    # Get basic param table
    def_prm <- get_prm_nlmixr2(xpdb, transform = transform, show_all = show_all, quiet=quiet)
    # Get untransformed param table
    untr_prm <- suppressWarnings(get_prm_nlmixr2(xpdb, transform = FALSE, show_all = show_all, quiet=TRUE))
  } else {
    cli::cli_abort("`get_prm` currently only works for nonmem and nlmixr2 models. Sorry.")
  }


  # Parameter associations defined for object
  par_asscs <- xpdb$pars %>%
    # add selector numbers for param and omega
    dplyr::mutate(
      thnums = param_selector(param, def_prm),
      omnums = param_selector(omega, def_prm)
    ) %>%
    dplyr::arrange(thnums)
  impacted_omegas <- par_asscs$omnums
  # Default function for missing (also overrides log)
  v_distinv <- Vectorize(function(v,exact) pmxcv::dist.intcv("log", v = v, exact=exact)) # outside function for speed
  lnorm_transform <- function(x, pmxcv.exact=TRUE) `if`(
    xpdb$options$cvtype=="sqrt",
    sqrt(x)*100,
    # sqrt(exp(x) - 1) # < this is all pmxcv is doing if exact is true, but for consistency with other methods...
    v_distinv(x,pmxcv.exact)
  )

  # Indexes of diagonal omega parameters to show cv
  rel_ome_i <- which(!is.na(def_prm$diagonal) & def_prm$diagonal==TRUE & def_prm$type=="ome")

  # prepopulated CV%
  new_prm <- def_prm %>%
    dplyr::mutate(
      cv = ifelse(
        any(duplicated(impacted_omegas)),
        list(NA_real_),
        NA_real_
      )
    )
  for (om2cv in rel_ome_i) {
    if (!om2cv %in% impacted_omegas) {
      new_cv <- lnorm_transform(untr_prm$value[om2cv])
    } else {
      # Do this generally so multiple thetas per omega can be covered
      par_rows <- which(impacted_omegas==om2cv)
      new_cv <- purrr::map_dbl(par_rows, function(row) {
        assc_row <- par_asscs %>% dplyr::slice(row)
        thnum <- assc_row$thnums
        thval <- new_prm$value[thnum]
        omval <- untr_prm$value[om2cv]
        argus <- assc_row$argus[[1]]
        if (assc_row$assoc=="log") return(
          lnorm_transform(omval,
                          pmxcv.exact = length(argus)==0 || argus[[1]]==TRUE)
        )
        if (par_asscs$assoc[row]=="nmboxcox") return(
          pmxcv::dist.intcv("nmboxcox", u = thval, v = omval,
                            lambda = eval(argus[[1]]))
        )
        if (par_asscs$assoc[row]=="custom") return(
          pmxcv::intcv(u = thval, v = omval, pdist = argus$pdist,
                       qdist = argus$qdist)
        )
        pmxcv::dist.intcv(assc_row$assoc, u = thval, v = omval)
      })
    }
    new_prm <- new_prm %>%
      dplyr::mutate(
        cv = ifelse(
          dplyr::row_number()==om2cv,
          .env$new_cv %>%
            .[!purrr::map_lgl(.,is.na)] %>%
            ifelse(is.list(cv), list(.), .),
          cv
        )
      )
  }

  # Also add shrinkages while we're here
  shk_wrap <- function(wh, n) rlang::try_fetch(
    get_shk(xpdb, wh, .problem=.problem, .subprob=.subprob, .method=.method),
    error = function(s) {
      if (n==1) cli::cli_warn("Shrinkage missing for {cli::col_magenta(ifelse(wh=='eta','omega','sigma'))} estimates, if any are modeled. Using NA in this table.")
      return(NA_real_)
    })
  eta_shk <- shk_wrap("eta", max(new_prm$m[new_prm$type=="ome"]))
  eps_shk <- shk_wrap("eps", max(new_prm$m[new_prm$type=="sig"]))

  new_prm<-new_prm %>%
    dplyr::mutate(
      shk = purrr::map2_dbl(type,purrr::map2(m,n,function(x,y) c(x,y)), ~{
        if (is.na(.y[2]) || .y[1]!=.y[2]) return(NA_real_)
        if (.x=="ome") return(eta_shk[.y[1]])
        if (.x=="sig") return(eps_shk[.y[1]])
      })
    ) %>%
    # Apply digits completely
    dplyr::mutate(dplyr::across(c(m,n), as.integer)) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.double),
        function(.x) tibble::num(.x, sigfig=digits)
      )
    )

  store_associations <- NULL
  if (length(impacted_omegas)>0) {
    # For nlmixr2, use name instead of label
    lhs_assoc <- def_prm$label[par_asscs$thnums]
    if (xpose::software(xpdb)=="nlmixr2")
      lhs_assoc <- def_prm$name[par_asscs$thnums]
    store_associations <- stringr::str_c(lhs_assoc,'~',
                   par_asscs$assoc,'(',def_prm$label[par_asscs$omnums],')')
  }

  as_prm_tbl(new_prm, associations=store_associations)
}

#' @noRd
as_prm_tbl <- function(tbl, ...) {
  if (inherits(tbl, "prm_tbl")) return(tbl)
  structure(
    tbl,
    class = c("prm_tbl", class(tbl)),
    ...
  )
}

#' @method print prm_tbl
#' @export
print.prm_tbl <- function(x, ...) {
  NextMethod()
  if (length(attr(x, "associations"))>0) {
    cli::cli_inform(cli::col_grey("# Parameter table includes the following associations:
                    {.strong {attr(x, 'associations')}}"))
  }
}

################################
# Covariate-effect computation
################################

# Return `label` where non-empty/non-NA, else fall back to `name`
# (nlmixr2 fits often have no label; NONMEM ones usually do)
label_or_name <- function(par_tbl, idx) {
  lbl <- par_tbl$label[idx]
  nm <- par_tbl$name[idx]
  ifelse(!is.na(lbl) & lbl!="", lbl, nm)
}

# Builtin effect_ratio functions as function(cov, ref, theta), theta always
# a numeric vector. `catshift` is intentionally excluded -- its per-level,
# index-based evaluation is handled directly in prm_catcov() rather than as
# a continuous function of `cov`.
cov_effect_fun <- function(assoc, argus) {
  switch(
    assoc,
    linear = function(cov, ref, theta) 1 + theta[1]*(cov-ref),
    power = function(cov, ref, theta) (cov/ref)^theta[1],
    exponential = function(cov, ref, theta) exp(theta[1]*(cov-ref)),
    additive = function(cov, ref, theta) (theta[1]+cov)/(theta[1]+ref),
    hockey = function(cov, ref, theta) {
      brk <- if (!is.null(argus$brk)) argus$brk else ref
      ifelse(cov <= brk, 1 + theta[1]*(cov-ref), 1 + theta[2]*(cov-ref))
    },
    custom = argus$fun,
    cli::cli_abort("Unsupported association type for continuous covariate computation: {.strong {assoc}}")
  )
}

# Propagate theta uncertainty (SE) through `fun` at fixed evaluation point(s)
# `cov` (may be a vector), treating each theta independently (no cross-theta
# covariance -- see dev-notes for why). Returns list(low=, high=), same
# length as `cov`.
cov_effect_ci <- function(fun, cov, ref, theta, se, ci_method, level, nsim) {
  names(cov) <- NULL # keep results unnamed regardless of a named `cov` (eg low/ref/high)
  alpha <- 1 - level
  n_theta <- length(theta)

  if (any(is.na(se))) {
    if (ci_method=="delta") {
      return(list(low = rep(NA_real_, length(cov)), high = rep(NA_real_, length(cov))))
    }
  }

  if (ci_method == "simulation") {
    set.seed(2323) # reproducible, mirrors mutate_prm()'s `.autose` simulation
    theta_draws <- matrix(
      stats::rnorm(nsim*n_theta, mean = rep(theta, each = nsim), sd = rep(se, each = nsim)),
      nrow = nsim, ncol = n_theta
    )
    sim_effects <- apply(theta_draws, 1, function(th) fun(cov, ref, th))
    if (is.null(dim(sim_effects))) sim_effects <- matrix(sim_effects, nrow = 1)
    ci <- apply(sim_effects, 1, stats::quantile, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
    return(list(low = unname(ci[1, ]), high = unname(ci[2, ])))
  }

  # Delta method: numerical gradient of log(effect) w.r.t. each theta,
  # first-order log-scale SE, independent thetas (variances add).
  h <- 1e-4 * pmax(abs(theta), 1)
  base_val <- fun(cov, ref, theta)
  log_var <- 0
  for (i in seq_len(n_theta)) {
    theta_up <- theta
    theta_up[i] <- theta[i] + h[i]
    grad_i <- (fun(cov, ref, theta_up) - base_val) / h[i]
    dlog_i <- grad_i / base_val
    log_var <- log_var + (dlog_i * se[i])^2
  }
  z <- stats::qnorm(1 - alpha/2)
  log_se <- sqrt(log_var)
  list(low = unname(base_val * exp(-z*log_se)), high = unname(base_val * exp(z*log_se)))
}

# Shared selector filtering for prm_contcov()/prm_catcov()/prm_cov(): dots
# are `param ~ covariate` formulas (bare selectors), same style as
# drop_cov_association(). Empty dots means "all declared associations".
filter_cov_selectors <- function(covs, dots, par_tbl) {
  if (length(dots)==0 || nrow(covs)==0) return(covs)
  covs_idx <- param_selector(covs$param, par_tbl)
  rlang::try_fetch({
    sel_tbl <- purrr::map_dfr(dots, function(f) {
      if (!rlang::is_formula(f, lhs=TRUE) || !inherits(f[[3]], "name"))
        cli::cli_abort("Selectors must be formulas of the form `param ~ covariate` (bare, unquoted).")
      tibble::tibble(par_i = param_selector(deparse(f[[2]]), par_tbl), covariate = deparse(f[[3]]))
    })
  },
  error = function(s) rlang::abort("Non-valid selector(s).", parent = s)
  )
  keep <- paste(covs_idx, covs$covariate) %in% paste(sel_tbl$par_i, sel_tbl$covariate)
  covs[keep, ]
}

#' Continuous/categorical covariate effect tables
#'
#' @description
#' Computes, for each covariate association declared with
#' [`add_cov_association()`], the covariate's effect on the associated
#' parameter (as a ratio to the parameter's typical value, `1` at the
#' reference covariate value/level) at a handful of representative
#' evaluation points, with an uncertainty interval propagated from the
#' effect-size theta's standard error. This is what [`xplot_forest()`]
#' plots; calling these directly is mostly useful for inspecting the
#' numbers before/without plotting.
#'
#' `prm_contcov()` handles continuous covariates (`linear`, `power`,
#' `exponential`, `hockey`, or `custom` associations), evaluated at the
#' low/reference/high points. `prm_catcov()` handles categorical
#' covariates (`catshift` or `custom` associations), evaluated at every
#' observed level. `prm_cov()` combines both.
#'
#' @rdname prm_cov
#'
#' @param xpdb <`xp_xtras`> object with associations declared via
#' [`add_cov_association()`]
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Optional `param ~ covariate`
#' selectors (bare, unquoted, same style as [`drop_cov_association()`]) to
#' restrict which declared associations are computed. Defaults to all of
#' them.
#' @param .problem <`numeric`> Problem number.
#' @param .subprob <`numeric`> Subprob number.
#' @param .method <`numeric`> Method.
#' @param ci_method <`character`> `"simulation"` (default) draws `nsim`
#' samples of each theta from `N(theta_hat, se)` (mirrors
#' [`mutate_prm()`]'s `.autose` approach) and propagates them through the
#' (possibly nonlinear) effect_ratio function, taking the resulting sample
#' quantiles as the interval; most accurate for strongly nonlinear forms
#' (`power`, `exponential`, `hockey`). `"delta"` is a first-order analytic
#' (numerical-gradient) log-scale approximation -- cheap and
#' deterministic, but less accurate the more nonlinear the association is.
#' Both treat multiple thetas (eg `hockey`, multi-level `catshift`) as
#' independent, ignoring any covariance between them.
#' @param probs <`numeric(2)`> For `prm_contcov()`: quantiles of the
#' covariate's observed data used as the "low"/"high" evaluation points.
#' @param level <`numeric`> Confidence level for the effect interval.
#' @param nsim <`numeric`> Number of simulation draws, when
#' `ci_method = "simulation"`.
#' @param quiet Silence extra output.
#'
#' @returns A `prm_cov_tbl` tibble (a plain tibble, classed for a future
#' print method) with one row per (parameter, covariate, evaluation point):
#' `param`, `covariate`, `covtype`, `level` (`"low"`/`"ref"`/`"high"` for
#' continuous, the raw category value for categorical), `value` (the
#' covariate value/level backing that row), `effect`, `ci_low`, `ci_high`,
#' `ci_method`.
#'
#' @seealso [`add_cov_association()`], [`xplot_forest()`]
#'
#' @export
#'
#' @examples
#'
#' xpdb_x %>%
#'   add_cov_association(TVCL ~ power(CLCR, THETA7, ref = 64)) %>%
#'   prm_contcov()
#'
#' xpdb_x %>%
#'   add_cov_association(TVCL ~ catshift(SEX, THETA4, ref = 1)) %>%
#'   prm_catcov()
#'
#' xpdb_x %>%
#'   add_cov_association(
#'     TVCL ~ power(CLCR, THETA7, ref = 64),
#'     TVCL ~ catshift(SEX, THETA4, ref = 1)
#'   ) %>%
#'   prm_cov()
#'
#' # Restrict to one association, and use the analytic delta-method CI
#' xpdb_x %>%
#'   add_cov_association(
#'     TVCL ~ power(CLCR, THETA7, ref = 64),
#'     TVCL ~ catshift(SEX, THETA4, ref = 1)
#'   ) %>%
#'   prm_cov(TVCL ~ CLCR, ci_method = "delta")
#'
prm_contcov <- function(
    xpdb,
    ...,
    .problem = NULL,
    .subprob = NULL,
    .method = NULL,
    ci_method = c("simulation", "delta"),
    probs = c(0.05, 0.95),
    level = 0.95,
    nsim = 1000,
    quiet
) {
  if (!check_xpdb_x(xpdb, .warn = TRUE))
    cli::cli_abort("{cli::col_blue('xp_xtras')} object required.")
  if (missing(quiet)) quiet <- xpdb$options$quiet
  fill_prob_subprob_method(xpdb, .problem=.problem, .subprob=.subprob,.method=.method)
  ci_method <- rlang::arg_match(ci_method)

  covs <- xpdb$covs %>%
    dplyr::filter(problem==.problem, subprob==.subprob, method==.method, covtype=="cont")
  if (nrow(covs)==0) return(as_prm_cov_tbl(empty_prm_cov_tbl()))

  par_tbl <- hot_swap_base_get_prm(xpdb, .problem=.problem, .subprob=.subprob,.method=.method, transform = FALSE, quiet = TRUE)
  covs <- filter_cov_selectors(covs, rlang::list2(...), par_tbl)
  if (nrow(covs)==0) return(as_prm_cov_tbl(empty_prm_cov_tbl()))

  data <- xpose::get_data(xpdb, .problem=.problem, quiet=TRUE)

  out <- purrr::pmap_dfr(covs, function(param, covariate, covtype, assoc, thetas, ref, argus, ...) {
    par_idx <- param_selector(param, par_tbl)
    theta_idx <- param_selector(thetas, par_tbl)
    theta_val <- par_tbl$value[theta_idx]
    theta_se <- par_tbl$se[theta_idx]

    lohi <- stats::quantile(data[[covariate]], probs = probs, na.rm = TRUE, names = FALSE)
    eval_pts <- c(low = lohi[1], ref = ref, high = lohi[2])

    fun <- cov_effect_fun(assoc, argus)
    point_effect <- fun(eval_pts, ref, theta_val)
    ci <- cov_effect_ci(fun=fun, cov=eval_pts, ref=ref, theta=theta_val, se=theta_se,
                         ci_method=ci_method, level=level, nsim=nsim)

    tibble::tibble(
      param = label_or_name(par_tbl, par_idx),
      covariate = covariate,
      covtype = "cont",
      level = names(eval_pts),
      value = as.character(signif(eval_pts, 4)),
      effect = as.numeric(point_effect),
      ci_low = ci$low,
      ci_high = ci$high,
      ci_method = ci_method
    )
  })
  as_prm_cov_tbl(out)
}

#' @rdname prm_cov
#' @export
prm_catcov <- function(
    xpdb,
    ...,
    .problem = NULL,
    .subprob = NULL,
    .method = NULL,
    ci_method = c("simulation", "delta"),
    level = 0.95,
    nsim = 1000,
    quiet
) {
  if (!check_xpdb_x(xpdb, .warn = TRUE))
    cli::cli_abort("{cli::col_blue('xp_xtras')} object required.")
  if (missing(quiet)) quiet <- xpdb$options$quiet
  fill_prob_subprob_method(xpdb, .problem=.problem, .subprob=.subprob,.method=.method)
  ci_method <- rlang::arg_match(ci_method)

  covs <- xpdb$covs %>%
    dplyr::filter(problem==.problem, subprob==.subprob, method==.method, covtype=="cat")
  if (nrow(covs)==0) return(as_prm_cov_tbl(empty_prm_cov_tbl()))

  par_tbl <- hot_swap_base_get_prm(xpdb, .problem=.problem, .subprob=.subprob,.method=.method, transform = FALSE, quiet = TRUE)
  covs <- filter_cov_selectors(covs, rlang::list2(...), par_tbl)
  if (nrow(covs)==0) return(as_prm_cov_tbl(empty_prm_cov_tbl()))

  data <- xpose::get_data(xpdb, .problem=.problem, quiet=TRUE)

  out <- purrr::pmap_dfr(covs, function(param, covariate, covtype, assoc, thetas, ref, argus, ...) {
    par_idx <- param_selector(param, par_tbl)
    label <- label_or_name(par_tbl, par_idx)
    # Compare/match on character throughout: `data[[covariate]]` may already
    # be a factor (xpose auto-factors catcov columns), while `ref` as typed
    # in the association formula is whatever raw type the user wrote (eg
    # numeric `1`); factor-vs-numeric equality is not reliable, but the
    # factor's levels and `as.character(ref)` are directly comparable.
    obs_levels <- sort(unique(stats::na.omit(data[[covariate]])))
    obs_chr <- as.character(obs_levels)
    ref_chr <- as.character(ref)

    if (assoc == "custom") {
      theta_idx <- param_selector(thetas, par_tbl)
      theta_val <- par_tbl$value[theta_idx]
      theta_se <- par_tbl$se[theta_idx]
      fun <- argus$fun

      rows <- purrr::map_dfr(seq_along(obs_levels), function(i) {
        lv <- obs_levels[i]
        point_effect <- fun(lv, ref, theta_val)
        ci <- cov_effect_ci(fun=fun, cov=lv, ref=ref, theta=theta_val, se=theta_se,
                             ci_method=ci_method, level=level, nsim=nsim)
        tibble::tibble(level = obs_chr[i], value = obs_chr[i],
                       effect = as.numeric(point_effect), ci_low = ci$low, ci_high = ci$high)
      })
    } else { # catshift
      nonref_chr <- setdiff(obs_chr, ref_chr)
      theta_idx <- param_selector(thetas, par_tbl)
      theta_val <- par_tbl$value[theta_idx]
      theta_se <- par_tbl$se[theta_idx]

      rows <- purrr::map_dfr(seq_along(obs_levels), function(i) {
        lv <- obs_levels[i]
        if (identical(obs_chr[i], ref_chr)) {
          return(tibble::tibble(level = obs_chr[i], value = obs_chr[i], effect = 1, ci_low = 1, ci_high = 1))
        }
        j <- match(obs_chr[i], nonref_chr)
        shift_fun <- function(cov, r, theta) 1 + theta[1]
        ci <- cov_effect_ci(fun=shift_fun, cov=lv, ref=ref, theta=theta_val[j], se=theta_se[j],
                             ci_method=ci_method, level=level, nsim=nsim)
        tibble::tibble(level = obs_chr[i], value = obs_chr[i],
                       effect = 1 + theta_val[j], ci_low = ci$low, ci_high = ci$high)
      })
    }

    rows$param <- label
    rows$covariate <- covariate
    rows$covtype <- "cat"
    rows$ci_method <- ci_method
    dplyr::relocate(rows, param, covariate, covtype, level, value, effect, ci_low, ci_high, ci_method)
  })
  as_prm_cov_tbl(out)
}

#' @rdname prm_cov
#' @export
prm_cov <- function(
    xpdb,
    ...,
    .problem = NULL,
    .subprob = NULL,
    .method = NULL,
    ci_method = c("simulation", "delta"),
    probs = c(0.05, 0.95),
    level = 0.95,
    nsim = 1000,
    quiet
) {
  if (!check_xpdb_x(xpdb, .warn = TRUE))
    cli::cli_abort("{cli::col_blue('xp_xtras')} object required.")
  if (missing(quiet)) quiet <- xpdb$options$quiet
  ci_method <- rlang::arg_match(ci_method)

  as_prm_cov_tbl(dplyr::bind_rows(
    prm_contcov(xpdb, ..., .problem=.problem, .subprob=.subprob, .method=.method,
                ci_method=ci_method, probs=probs, level=level, nsim=nsim, quiet=quiet),
    prm_catcov(xpdb, ..., .problem=.problem, .subprob=.subprob, .method=.method,
               ci_method=ci_method, level=level, nsim=nsim, quiet=quiet)
  ))
}

#' @noRd
empty_prm_cov_tbl <- function() {
  tibble::tibble(
    param = character(), covariate = character(), covtype = character(),
    level = character(), value = character(), effect = double(),
    ci_low = double(), ci_high = double(), ci_method = character()
  )
}

#' @noRd
as_prm_cov_tbl <- function(tbl) {
  if (inherits(tbl, "prm_cov_tbl")) return(tbl)
  structure(tbl, class = c("prm_cov_tbl", class(tbl)))
}


# Transform parameter values
#' Transform parameter values in place
#'
#' @description
#' Apply transformations to fitted parameter values.
#'
#' As fitted, sometimes parameter values are not as easy to communicate, but
#' to transform them outside of the `xpose` ecosystem limits some available
#' features. To have the best experience, this function can update the
#' parameter values that are used by `xpose` `get_prm` functions. At this
#' time these transformations are not applied to `param` vars ([`list_vars`]), but that can
#' already be done with the `mutate` method.
#'
#' **This only works for theta parameters.**
#'
#' All valid mutations are applied sequentially, so a double call to `the2~the2^3`
#' will result in effectively `the2~the2^9`, for example.
#'
#' RSE values are calculated at runtime within `get_prm`, so they are not updated (or
#' updatable) with this function.
#'
#'
#' @param xpdb <`xp_xtras`> object
#' @param ... ... <[`dynamic-dots`][rlang::dyn-dots]> One or more formulae that
#' define transformations to parameters. RHS of formulas can be function or a
#' value. That value can be a function call like in `mutate()` (`the1~exp(the1)`).
#' @param .autose <`logical`> If a function is used for the transform then simulation
#' is used to transform the current SE to a new SE. Precision of this transformation
#' is dependent on `.sesim`. If parameter values are not assigned with a function,
#' this option will simply scale SE to maintain the same RSE. See Details.
#' @param .problem <`numeric`> Problem number to apply this relationship.
#' @param .subprob <`numeric`> Problem number to apply this relationship.
#' @param .method <`numeric`> Problem number to apply this relationship.
#' @param .sesim <`numeric`> Length of simulated `rnorm` vector for `.autose`.
#' @param quiet Silence extra output.
#'
#' @export
#'
#' @details
#' ### Important points about covariance and correlation (for NONMEM only)
#'
#' Covariance and correlation parameters are adjusted when standard error (SE)
#' values are changed directly or with `.autose`. When a transformation is applied
#' as a function for the fixed effect parameter (eg, `~plogis`), the resulting SE may have
#' an unexpected scale; this is because it is now reporting the standard deviation
#' of a transformed and potentially non-normal distribution. If the parameter were fit
#' in the transformed scale (constrained to any appropriate bounds), it would likely have a
#' different SE given that most covariance estimation methods (excluding non-parametric and
#' resampling-based) will treat the constrained parameter as continuous and unconstrained.
#'
#' The updates to variance-covariance values (and the correlation values, though that is mostly
#' invariant) are applied to the entire matrices. When piped directly into
#' `get_prm`, only the SE estimate is shown, but <[`get_file`][`xpose::get_file`]> can be used
#' to see the complete updated variance-covariance values. This could be useful if those
#' matrices are being used to define priors for a Bayesian model fitting, as the re-scaling
#' of off-diagonal elements is handled automatically.
#'
#' For all software: A function to transform parameters will result in a more accurate `autose`
#' result. If a call (`the1~exp(the)`) or a value (`the1~2`) are used, the standard error
#' will be simply scaled.
#'
#' @returns An updated `xp_xtras` object with mutated parameters
#'
#' @examples
#'
#' vismo_pomod %>%
#'   # Function
#'   mutate_prm(THETA11~exp) %>%
#'   # Value (se will not be scaled); plogis = inverse logit
#'   mutate_prm(THETA12~plogis(THETA12)) %>%
#'   get_prm()
#'
#'
mutate_prm <- function(
  xpdb,
  ...,
  .autose = TRUE, # simulation-based SE approximation
  .problem = NULL,
  .subprob = NULL,
  .method = NULL,
  .sesim = 100000,
  quiet) {
  # Make sure users did not do `=`
  rlang::try_fetch(
    rlang::check_dots_unnamed(),
    error = function(s)
      rlang::abort(paste("Only formula(e) are expected in the dots, not assignment.",
                   "Was `=` used instead of `~`?"), parent=s)
  )

  ### Top part here is similar in behavior to xpose::get_prm
  # Fill empty
  if (missing(quiet)) quiet <- xpdb$options$quiet
  fill_prob_subprob_method(xpdb, .problem=.problem, .subprob=.subprob,.method=.method)
  # Use xpose::get_prm for other checks and to get untransformed values
  base_df <- suppressWarnings(hot_swap_base_get_prm(xpdb=xpdb, .problem=.problem, .subprob = .subprob,
                            .method = .method, digits = 12, # absurd digits so no rounding
                            transform = FALSE, show_all = TRUE, quiet=TRUE)) # Need show_all so cor and cov columns are same length as this
  if (all(is.na(base_df$se))) {
    rlang::warn("Covariance matrix is not available. Changes affecting SEs will not be implementable.")
  }

  ### Now different boilerplate
  mutp_list <- rlang::list2(...) # List of formulas (hopefully)
  # Allow a list to be passed to ... given add_relationship behavior
  if (length(mutp_list)>=1 && is.list(mutp_list[[1]])) {
    rlang::warn("List should not be used in dots, but is allowed; instead pass as arguments or pass list with !!!list.")
    mutp_list <- mutp_list[[1]]
  }

  # Validate input
  ## Return base object if no associations are provided
  if (rlang::dots_n(...)==0) return(xpdb)
  ## Check that formulas are valid
  mutp_tab <- mutate_prm_check(mutp_list=mutp_list, xpdb=xpdb, .problem=.problem, .subprob=.subprob,.method=.method)

  # Make environment in which to run the calls
  calls_env <- new.env()
  # Add all possible selectors as variables in the environment
  only_theta <- base_df$type=="the"
  type_sels <- paste0(base_df$type,base_df$m)[only_theta] # only theta parameters allowed
  name_sels <- base_df$name[only_theta]
  labl_sels <- base_df$label[only_theta]
  sel_vals <- base_df$value[only_theta]
  calls_env$base_df <- base_df
  calls_env$se <- function(selval) base_df$se[param_selector(deparse(substitute(selval)), base_df)] # get se when function calls (se(sel))
  for (toassign in 1:sum(only_theta)) { # for loop because assigning to environment
    assign(type_sels[toassign], sel_vals[toassign], envir = calls_env)
    assign(name_sels[toassign], sel_vals[toassign], envir = calls_env)
    if (labl_sels[toassign]!="") rlang::try_fetch(
      assign(labl_sels[toassign], sel_vals[toassign], envir = calls_env),
      error = function(s) {
        if (labl_sels[toassign] %in% mutp_tab$param) rlang::abort(paste("Label selector '",labl_sels[toassign],"' cannot be used as a selector in this function."), parent=s)
        if (!quiet) rlang::warn(paste("Error downgraded to warning; label '",labl_sels[toassign],"' is not a valid selector, if used."), parent=s)
      }
    )
  }

  # Iterate through mutations
  new_xpdb <- xpdb
  new_val <- function(call_or) {
    if (!class(call_or) %in% c("name","call")) return(as.numeric(call_or)) # it is the new value
    f <- rlang::try_fetch(eval(call_or, envir = calls_env),
                          error = function(s)
                            rlang::abort(paste0("Cannot evaluate RHS expression `",deparse(call_or),"`. Was an omega or sigma selector used?"), parent=s)
    )

    f # either function or a value
  }
  refresh_calls_prms <- function(xpdb) # Same as call in beginning of function. Get updated parameters
    calls_env$base_df <- suppressWarnings(hot_swap_base_get_prm(xpdb=xpdb, .problem=.problem, .subprob = .subprob,
                                                         .method = .method, digits = 12,
                                                         transform = FALSE, show_all = TRUE, quiet=TRUE)) # show all is for off-diagonal of cor cov
  for (mn in 1:nrow(mutp_tab)) { # For loop to apply changes initiated from mutp_tab to running updates in new_xpdb
      new_value <- new_val(mutp_tab$argus[mn][[1]])
      change_to <- new_value
      impacted_prm <- param_selector(mutp_tab$param[mn],base_df)
      existing_value <- calls_env$base_df$value[impacted_prm]
      # For fixed effects, implement
      if (!mutp_tab$is_se[mn]) {
        # Change to value in files
        if (is.function(new_value)) {
          change_to <- rlang::try_fetch(evalq(new_value(existing_value),
                                 envir = calls_env),
                           error = function(s)
                             rlang::abort(paste0("Cannot evaluate RHS expression `",
                                                 stringr::str_trunc(paste(deparse(new_value), collapse=""), 18),
                                                 "`. Was an omega or sigma selector used?"), parent=s)
          )
        }
        if (length(change_to)!=1)
          cli::cli_abort("RHS must resolve to a length of {.strong 1}, not {.strong {length(change_to)}}")
        # Change value in ext
        if (xpose::software(xpdb)=="nonmem") {
          new_xpdb$files <- mutate_in_file(
            xpdb = new_xpdb,
            val = change_to,
            col = 1 + impacted_prm,
            row = quote(ITERATION==-1000000000),
            ext = "ext",
            problem = .problem,
            subprob = .subprob,
            method = .method
          )
        } else if (xpose::software(xpdb)=="nlmixr2") {
          new_xpdb <- mutate_mask(
            new_xpdb, fortheta = base_df$name[impacted_prm], newval = change_to
          )
        } else {
          cli::cli_abort("This functionality is not yet implemented for {.strong {xpose::software(xpdb)}}")
        }
        new_xpdb <- as_xp_xtras(new_xpdb)
        # Update base_df in calls_env
        refresh_calls_prms(new_xpdb)
      }
      if (mutp_tab$is_se[mn] || .autose==TRUE) {
        # If autose and *not* is_se, then update new_value before processing
        exisiting_se <- calls_env$base_df$se[impacted_prm]
        if (is.na(exisiting_se) || exisiting_se==0) next # no point in doing the rest if se is 0 or unknown
        if (!mutp_tab$is_se[mn] && .autose==TRUE) {
          if (is.function(new_value)) {
            set.seed(2323) # ensure reproducible
            nv_fun <- new_value
            new_value <- function(x,y=exisiting_se)
              sd(
                nv_fun(
                  rnorm(.sesim, mean = x, sd = abs(y)) # distribution implied by current estimate and SE
                  ),
                na.rm = TRUE # in case original estimate was bounded and for some reason this function would still be used.
                )
          } else {
            if (!quiet) cli::cli_warn("Since a function was not provided, `autose` will be scaled to maintain the same RSE.")
            new_value <- exisiting_se*abs(new_value/existing_value)
          }
        }

        change_to <- new_value
        if (is.function(new_value)) {
          change_to <- rlang::try_fetch(evalq(new_value(existing_value),
                                              envir = calls_env),
                                        error = function(s)
                                          rlang::abort(paste0("Cannot evaluate RHS expression `",
                                                              stringr::str_trunc(paste(deparse(new_value), collapse=""), 18),
                                                              "`. Was an omega or sigma selector used?"), parent=s)
          )
        }
        if (length(change_to)!=1)
          cli::cli_abort("RHS must resolve to a length of {.strong 1}, not {.strong {length(change_to)}}")
        # Change value in ext (most times get_prm actually pulls from cov, but can happen here, too)
        if (xpose::software(xpdb)=="nonmem") {
          new_xpdb$files <- mutate_in_file(
            xpdb = new_xpdb,
            val = abs(change_to),
            col = 1 + impacted_prm,
            row = quote(ITERATION==-1000000001),
            ext = "ext",
            problem = .problem,
            subprob = .subprob,
            method = .method
          )
        } else if (xpose::software(xpdb)=="nlmixr2") {
          new_xpdb <- mutate_mask(
            new_xpdb, fortheta = base_df$name[impacted_prm], newval = abs(change_to),
            se = TRUE
          )
        } else {
          # Since other software would have files processed differently, make SE correction here
          return(new_xpdb)
        }
        new_xpdb <- as_xp_xtras(new_xpdb)
        # If Iteration -1...1 exists, there should be cor/cov, but just in case...
        if (xpose::software(xpdb)=="nonmem" &&
            "cov" %in% new_xpdb$files$extension &&
            "cor" %in% new_xpdb$files$extension) {
          # Change value in cor (off-diagonal is invariant to scale; only need to change reporting diagonal)
          new_xpdb$files <- mutate_in_file(
            xpdb = new_xpdb,
            val = abs(change_to), # cor diagonal is SE, in NONMEM and others
            col = 1 + impacted_prm,
            row = parse(text=paste0("dplyr::row_number()==",impacted_prm)),
            ext = "cor",
            problem = .problem,
            subprob = .subprob,
            method = .method
          )
          # Change value in cov
          new_variance <- change_to^2
          orig_variance <- exisiting_se^2
          new_xpdb <- as_xp_xtras(new_xpdb)
          # Change values in cov off-diagonal + diagonal
          cur_off_diag <- xpose::get_file(new_xpdb, ext="cov", .problem=.problem, .subprob = .subprob,
                                          .method = .method, quiet=TRUE) %>%
            dplyr::pull(1 + impacted_prm)
          new_off_diag <- cur_off_diag*sqrt(new_variance/orig_variance)
          new_off_diag[impacted_prm] <- new_variance
          new_xpdb$files <- mutate_in_file( # change off-diag col
            xpdb = new_xpdb,
            val = new_off_diag,
            col = 1 + impacted_prm,
            row = quote(dplyr::row_number()),
            ext = "cov",
            problem = .problem,
            subprob = .subprob,
            method = .method
          )
          new_xpdb <- as_xp_xtras(new_xpdb)
          new_xpdb$files <- mutate_in_file( # change off-diag row
            xpdb = new_xpdb,
            # This is clunky but seems to work
            val = rlang::quo( # special handling for quosures now in mutate_in_file
              new_off_diag[ # limit only to the value in this vector that matches...
                which( # ... the TRUE values for...
                  # when names for the current data frame is the current column
                  names(dplyr::pick(everything()))==dplyr::cur_column()
                  ) # This whole which(...) call is effectively just getting the current column index
                -1] # Skip NAME column, since it is not counted in new_off_diag
              ),
            col = 2:(dplyr::last_col()),
            row = parse(text=paste0("dplyr::row_number()==",impacted_prm)),
            ext = "cov",
            problem = .problem,
            subprob = .subprob,
            method = .method
          )
          new_xpdb <- as_xp_xtras(new_xpdb)
        }
        # Update base_df in calls_env
        refresh_calls_prms(new_xpdb)
      }
      # for whatever the current theta and se are, RSE are updated by nature of the xpose::get_prm function
  }
  new_xpdb
}

# functions to check and process
mutate_prm_check <- function(
    mutp_list,
    xpdb,
    ...,
    .problem,
    .subprob,
    .method
) {
  # Check that associations are valid

  # Confirm list of formulas
  if (
    length(mutp_list)==0 ||
    !is_formula_list(mutp_list)
  ) {
    rlang::abort("Mutations must be formulas.")
  }

  # Basic checks that should not be done in processing
  for (fmla in mutp_list) {

    # Check for non-empty lhs
    if (!rlang::is_formula(fmla, lhs=TRUE))
      cli::cli_abort("LHS of formula cannot be empty in `{cli::code_highlight(deparse(fmla))}`")

    # Check that if lhs is a call, it is
    if (inherits(fmla[[2]],"call") && !deparse(fmla[[2]][[1]])=="se") {
      if (deparse(fmla[[2]][[1]])=="+")
        cli::cli_abort("Because of parsing complexities, RHSs cannot be shared in this function with `+` on LHS: `{deparse(fmla[[2]])}`")

      cli::cli_abort("LHS of formula must be a selector or in the form `se(selector)`, not `{deparse(fmla[[2]])}`")
    }

  }

  # All symbols
  sym_tab <- mutate_prm_proc(mutp_list, .problem=.problem, .subprob=.subprob,.method=.method)
  par_tbl <- suppressWarnings(hot_swap_base_get_prm(xpdb, .problem=.problem, .subprob=.subprob,.method=.method, transform = FALSE, quiet = TRUE))
  rlang::try_fetch({
    pars <- sym_tab$param %>% param_selector(prm_tbl = par_tbl)
  },
  error = function(s)
    rlang::abort("Non-valid selectors in association.", parent=s)
  )
  if (any(pars>sum(par_tbl$type=="the"))) {
    cli::cli_abort("Inappropriate selector {.strong {unique(sym_tab$param[pars>sum(par_tbl$type==\"the\")])}}. Only theta parameters can be used in this function.")
  }

  return(sym_tab) # return the checked, processed list for this
}


mutate_prm_proc <- function(mutp_list,.problem,.subprob,.method) {
  purrr::map_dfr(
    mutp_list,
    ~ {
      # Extract symbol(s) in lhs
      lhs <- all.vars(.x[[2]])
      is_se <- !identical(lhs, all.vars(.x[[2]], functions = TRUE))
      # RHS may be
      # value
      # selector
      # selector used as variable in call
      # function name
      # function declaration
      # ... So best to just save quoted call
      rhs <- .x[[3]]



      # Create a tibble
      tibble::tibble(
        param = lhs,
        is_se = is_se,
        argus = list(rhs),
        problem = .problem,
        subprob = .subprob,
        method = .method
      )
    }
  )
}

# Pulled out to isolate this logic.
# Surgical update of enclosed file values.
# Not intended to be used by end users, so can avoid some boilerplate
# See mutate_files for a generic application
mutate_in_file <- function(
    xpdb,
    val,
    col,
    row,
    ext,
    problem,
    subprob,
    method
) {
  xpdb$files %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      data = if (
        extension == ext &
        problem==.env[["problem"]] & subprob==.env[["subprob"]] & method==.env[["method"]]
      ) {
        data %>%
          dplyr::mutate(across(
            {{ col}},
            function(v) {
              row_picker <- eval(row, envir = environment())
              if (rlang::is_quosure(val)) val <- eval(val, envir = environment())
              v[row_picker] <- val
              v
            }
          )) %>%
          list()
      } else {
        data %>%
          list()
      }
    ) %>%
    dplyr::ungroup()
}


