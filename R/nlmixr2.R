#' Attach nlmixr2 fit object to xpose data object
#'
#' @param xpdb <`xpose_data`> The object upon which to attach the fit
#' @param obj <`nlmixr2FitData`> Result of the nlmixr2 fit
#'
#' @return An object of the same class as `xpdb` with an additional element.
#' @export
#'
#' @examples
#' \dontrun{
#' # Based on an example from nlmixr2 documentation
#' if (rlang::is_installed("nlmixr2") &&
#'     rlang::is_installed("nlmixr2data")) {
#'   one.cmt <- function() {
#'     ini({
#'       tka <- 0.45 # Ka
#'       tcl <- log(c(0, 2.7, 100)) # Log Cl
#'       tv <- 3.45; label("log V")
#'       eta.ka ~ 0.6
#'       eta.cl ~ 0.3
#'       eta.v ~ 0.1
#'       add.sd <- 0.7
#'     })
#'     model({
#'       ka <- exp(tka + eta.ka)
#'       cl <- exp(tcl + eta.cl)
#'       v <- exp(tv + eta.v)
#'       linCmt() ~ add(add.sd)
#'     })
#'   }
#'
#'   theo_sd_fit <- nlmixr2::nlmixr2(one.cmt, nlmixr2data::theo_sd,
#'       "focei", control=nlmixr2::foceiControl(print=0))
#'
#'   attach_nlmixr2(
#'     xpdb_nlmixr2, theo_sd_fit
#'   ) %>%
#'   as_xpdb_x() %>%
#'   print() # fit will be mentioned in print() method
#' }
#'}
attach_nlmixr2 <- function(
    xpdb,
    obj
  ) {
  rlang::check_installed("nlmixr2", reason = "to interact with attached object.")
  xpose::check_xpdb(xpdb)
  nlmixr2est::assertNlmixrFit(obj)

  xpdb_new <- xpdb
  xpdb_new$fit <- obj

  class(xpdb_new) <- class(xpdb)
  xpdb_new
}


#' Convenience function for ingesting an nlmixr2 model to xpose and xpose.xtras
#'
#' @description
#' A wrapper that executes the pipeline:
#'
#' \preformatted{
#' obj |>
#'  xpose.nlmixr2::xpose_data_nlmixr2() |>
#'  attach_nlmixr2() |>
#'  as_xp_xtras() |>
#'  backfill_nlmixr2_props() %>%
#'  `if`(.skip_assoc, ., nlmixr2_prm_associations(.))
#' }
#'
#'
#' @param obj nlmixr2 fit object
#' @param ... Passed to [xpose_data_nlmixr2][xpose.nlmixr2::xpose_data_nlmixr2()]
#' @param .skip_assoc <`logical`> If the model is relatively uncomplicated, [`nlmixr2_prm_associations(`]
#' may be able to recognize relationships between random effects and fixed effect parameters. If the default
#' (`FALSE`) fails then try to rerun with the association step skipped.
#'
#' @return An <`xp_xtra`> object with fit attached
#' @export
#'
#' @seealso [attach_nlmixr2()]
#'
nlmixr2_as_xtra <- function(
  obj,
  ...,
  .skip_assoc = FALSE
) {
  mod_name <- deparse(substitute(obj))
  rlang::check_installed("xpose.nlmixr2")
  rlang::check_installed("nlmixr2")

  xpose.nlmixr2::xpose_data_nlmixr2(
    obj = obj, ...
  ) %>%
    attach_nlmixr2(obj) %>%
    as_xp_xtras() %>%
    set_prop(file = mod_name) %>%
    mutate_files(name = dplyr::case_when(
      # In current version, this is the only case
      length(name)==1 & all(name=="obj") ~ mod_name,
      # If the situation changes, fall back to default
      TRUE ~ name
    )) %>%
    backfill_nlmixr2_props() %>%
    `if`(.skip_assoc, ., nlmixr2_prm_associations(.))
}

#' Populate some properties from nlmixr2 fit
#'
#' @param xpdb <`xpose_data`> object
#'
#'
#' @details
#' This function will currently backfill:
#'
#' * condn
#' * nsig
#'
#' @export
#'
#' @examples
#'
#' xpdb_nlmixr2 %>%
#'   set_prop(condn = "not implemented") %>%
#'   get_prop("condn")
#'
#' xpdb_nlmixr2 %>%
#'   set_prop(condn = "not implemented") %>%
#'   backfill_nlmixr2_props() %>%
#'   get_prop("condn")
#'
backfill_nlmixr2_props <- function(xpdb) {
  checkmate::assert_true(test_nlmixr2_has_fit(xpdb))
  rlang::check_installed("rxode2") # This would be installed

  xpdb %>%
  # Condition number
  set_prop(
    condn = dplyr::coalesce(paste(xpdb$fit$conditionNumberCov), "na")
  ) %>%
  # Significant digits
  set_prop(
    nsig = rxode2::rxGetControl(xpdb$fit$ui, "sigdig", 3L)
  )
}

#' Test if xpose data object has a fit object
#'
#' @param xpdb <`xpose_data`> object
#'
#' @keywords internal
#' @export
#'
test_nlmixr2_has_fit <- function(xpdb) {
  xpose::check_xpdb(xpdb)
  if (xpose::software(xpdb)!="nlmixr2") return(FALSE)
  "fit" %in% names(xpdb) && inherits(xpdb$fit, "nlmixr2FitData")
}


#' @title get_prm equivalent for nlmixr2 fits
#'
#' @description
#' This is intended to match the <`xpose::get_prm`> rather than the
#' updated [`get_prm()`].
#'
#' @param fit <`nlmixr2FitData`>
#' @param transform  <`logical`> as in [get_prm()]
#' @param show_all   <`logical`> as in [get_prm()]
#' @param quiet <`logical`> as in [get_prm()]
#'
#' @return a tibble with expected columns
get_prm_nlmixr2 <- function(
    fit,
    transform = formals(get_prm)$transform,
    show_all = formals(get_prm)$show_all,
    quiet = FALSE
) {
  if (!quiet) cli::cli_alert_info("{.strong nlmixr2} does not provide SE values for random effect parameters (this includes error parameters).")

  # Template
  templater <- xpose::get_prm(xpdb_x,quiet = TRUE) %>%
    dplyr::slice(0)

  # Additional parameter data
  extra_info <- (fit$finalUi$iniDf) %>%
    # Common rename
    dplyr::rename(
      value = est, # for finalIU, est is the fitted estimate
      fixed = fix,
      n = neta2
    )

  # Fixed effects
  fx_tbl <- fit$parFixedDf %>%
    tibble::rownames_to_column("name") %>%
    dplyr::mutate(
      rse = `%RSE`/100
    ) %>%
    dplyr::rename(
      se = SE,
    ) %>%
    dplyr::select(name,se,rse)
  the_tbl <- extra_info %>%
    # Drop omegas (keep error parameters)
    dplyr::filter(!is.na(ntheta)) %>%
    # analogous columns
    dplyr::mutate(
      type = "the",
      m = ntheta,
    ) %>%
    # Join results
    dplyr::left_join(
      fx_tbl, by = "name"
    )

  # Omegas
  all_nxm_combos <- nrow(fit$omega)
  ome_tbl <- templater
  if (all_nxm_combos>0) {
    show_all_om <- expand.grid(
      neta1 = 1:all_nxm_combos,
      neta2 = 1:all_nxm_combos
    ) %>%
      dplyr::filter(neta2<=neta1) %>%
      # get transformed or not estimates
      dplyr::rowwise() %>%
      dplyr::mutate(
        value = `if`(TRUE==transform,
                     fit$omegaR,
                     fit$omega)[neta1,neta2],
      ) %>%
      dplyr::ungroup() %>%
      # Mete info
      dplyr::mutate(
        diagonal = neta1==neta2,
        m = neta1,
        n = neta2,
        type = "ome"
      ) %>%
      dplyr::left_join(
        dplyr::filter(extra_info, is.na(ntheta)) %>%
          # Remove value
          dplyr::select(-value),
        by=c("neta1","n"),
        keep =TRUE # Keep off-diagonals not in extra_info
      ) %>%
      # Because of keep=TRUE, n is split into n.x and n.y
      dplyr::rename(
        n = n.x
      ) %>%
      # Ensure fixed is not NA, and assume true if no value
      dplyr::mutate(
        fixed = ifelse(is.na(fixed), TRUE, fixed)
      ) %>%
      # Fill name and label if missing
      dplyr::mutate(
        label = ifelse(
          is.na(label) & !is.na(name), name,
          label
        ),
        name = ifelse(is.na(name),sprintf("omega(%s,%s)",m,n), name)
      )
    ome_tbl <- show_all_om %>%
      dplyr::select(dplyr::any_of(names(templater)))
    if (!show_all) ome_tbl <- dplyr::filter(ome_tbl,diagonal | !fixed)
  }

  # Sigmas
  # nlmixr2 doesn't use SIGMAs in the same way as nonmem, and effectively
  # uses a single epsilon even for multi-endpoint models.
  sig_tbl <- templater %>%
    dplyr::add_row(
      type = "sig",
      name = "sigma(1,1)",
      label = "err",
      value = 1,
      fixed = TRUE,
      diagonal = TRUE,
      m = 1,
      n = 1
    )

  dplyr::bind_rows(
    templater,
    the_tbl,
    ome_tbl,
    sig_tbl
  ) %>%
    dplyr::select(dplyr::all_of(names(templater))) %>%
    # For any remaining empty labels, sub in "" to be consistent with xpose::get_prm for nonmem
    dplyr::mutate(
      label = ifelse(is.na(label), "", label)
    )
}

# Based on the xpdb software, use xpose::get_prm or nlmixr2 equivalent
hot_swap_base_get_prm <- function(xpdb, ...) {
  all_dots <- rlang::dots_list(..., .named = TRUE)
  if (xpose::software(xpdb)=="nonmem") {
    use_function <- xpose::get_prm
    use_dots <- all_dots[names(all_dots) %in% names(formals(use_function))]
    par_tbl <- rlang::exec(
      use_function, xpdb, !!!use_dots
    )
  } else if (xpose::software(xpdb)=="nlmixr2") {
    checkmate::assert_true(test_nlmixr2_has_fit(xpdb))
    use_function <- get_prm_nlmixr2
    use_dots <- all_dots[names(all_dots) %in% names(formals(use_function))]
    par_tbl <- rlang::exec(
      use_function, xpdb$fit, !!!use_dots
    )
  }
  par_tbl
}


# TODO: mutate_in_fit for equivalent of mutate_in_file used by mutate_prm
# Using that, can have a backfill to apply backtransform to

#' Based on associations baked into nlmixr2, automatically add to xpose data
#'
#' @param xpdb <`xp_xtras`> object
#' @param dry_run <`logical`> Return a resulting information to compare against.
#'
#' @details
#' This function attempts to discern the associations between omegas and thetas
#' using information about mu referencing within the `nlmixr2` fit object. Please
#'
#'
#' Notably, back-transformations are not as relevant here as they may seem. Manual
#' back-transformation with `backTransform()` only affects the display of the
#' back-transformed theta estimate (and CI), but does not impact the
#' relationship between EBEs and individual parameter estimates.
#'
#' @seealso [rxode2::ini()]
#' @return Object with filled `par`
#' @export
#'
nlmixr2_prm_associations <- function(xpdb, dry_run = FALSE) {
  checkmate::assert_true(test_nlmixr2_has_fit(xpdb))

  # Several parts to this that end up pretty complex

  # Notably, if add_cov_association is ever implemented, this function
  # can be modified so the dry_run also returns covariate info (probably as a nested tibble)

  # Backtransform (for record-keeping)
  inidf <- xpdb$fit$iniUi$iniDf

  # Get muRefCurEval and muRefTable
  muref_cureval <- xpdb$fit$ui$muRefCurEval # Captures transformation
  muref_tbl <- xpdb$fit$ui$muRefTable # Captures what params are trans_f(theta + eta)

  # Get eta and theta lhs
  eta_lhs <- xpdb$fit$ui$etaLhs
  theta_lhs <- xpdb$fit$ui$thetaLhs

  # Now we process
  # We convert the LHS vectors to tibbles for joining on which param they estimate
  v2t <- function(v, what) v %>% t() %>% t() %>%
    as.data.frame() %>%
    rlang::set_names("param") %>%
    tibble::rownames_to_column(what)
  eta_lhs_tbl <- v2t(eta_lhs, "eta")
  theta_lhs_tbl <- v2t(theta_lhs, "theta")
  # Join and...
  lhs_tbl <- dplyr::full_join(
    theta_lhs_tbl, eta_lhs_tbl,
    by = "param"
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # flag as mu referenced or not
      muref = paste(theta,eta) %in% paste(muref_tbl$theta,muref_tbl$eta),
      # determine transformation of eta parameter
      etatrans = muref_cureval$curEval[muref_cureval$parameter==eta],
      # FYI determine transformation of theta parameter
      thetatrans = muref_cureval$curEval[muref_cureval$parameter==theta],
      # record manual backtransform, if any
      thetabt = inidf$backTransform[inidf$name==theta]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(param, .before = dplyr::everything())

  # Check if all etatrans have a known pmxcv equivalent
  etatrans_pmxcv <- dplyr::bind_rows(
    dplyr::tibble(
      etatrans = "exp",
      # Define these as quosures so we can check for globally evaluable custom functions
      pdist = list(rlang::quo(exp)), # get original with rlang::as_label
      qdist = list(rlang::quo(log)), # Probably won't use these quos, but using this approach for flexibility
      dist = "log"
    ),
    dplyr::tibble(
      etatrans = "expm1",
      pdist = list(rlang::quo(function(x) exp(x)-1)),
      qdist = list(rlang::quo(function(x) log(x+1))),
      dist = "logexp"
    ),
    dplyr::tibble(
      etatrans = "expit",
      pdist = list(rlang::quo(plogis)),
      qdist = list(rlang::quo(qlogis)),
      dist = "logit"
    ),
    # Add more as needed
  )

  # Create a list of transformations supported by rxode2 that need additional parameters
  # These _may_ be feasible to automatically add, but honestly the juice isn't worth
  # the squeeze. Some transformations can technically take additional parameters (like expit),
  # but it's safe to assume that in the vast majority of cases the default is used.
  need_extra_params <- c("boxCox", "yeoJohnson")
  if (any(lhs_tbl$etatrans %in% need_extra_params)) {
    cli::cli_alert_info(
      paste("Transformations {.strong {dplyr::intersect(need_extra_params,lhs_tbl$etatrans)}} need",
      "additional parameters that are not captured in the current version of this function.","\n",
      "\U00A0 Add manually with {.code add_prm_association(...)}.")
    )
  }

  # For each row, add prm association using appropriate pmxcv or custom association
  # In each custom case, test if pdist function can be applied to (0.1) without error when
  # evaluated in the global environment. If not, see if the function exists in the rxode2
  # namespace, then change custom to reflect that. Then check if in rxode2::rxSupportedFuns(),
  # and if so (supported by not in rxode2 namespace) set a flag. If it is not evaluable,
  # let the user know they need to write a custom for it; if the rxSupportedFuns flag is up,
  # note that the function is used internal to rxode2, and if not ask if it was a custom function
  # from the model fitting script?
  fmla_builder <- function(lhs,rhs_fun,rhs_inner) paste0(lhs,"~",rhs_fun,"(",rhs_inner,")")
  pdist_tester <- function(pdist_quo) {

  }
  transforms_to_apply <- lhs_tbl %>%
    # etatrans_pmxcv
    dplyr::rowwise() %>%
    dplyr::mutate(
      prm_assoc_formula = dplyr::case_when(
        # Predefined etatrans
        etatrans %in% etatrans_pmxcv$etatrans &&
          # With non-custom dist
          etatrans_pmxcv[etatrans_pmxcv$etatrans == etatrans]$dist!="custom" ~
          fmla_builder()
      )
    ) %>%
    dplyr::ungroup()

  if (dry_run) return(lhs_tbl)
  purrr::pwalk(transforms_to_apply, ~ {

  })




  xpdb
}
