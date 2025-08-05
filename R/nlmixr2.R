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
#' @param .skip_assoc <`logical`> If the model is relatively uncomplicated, [nlmixr2_prm_associations()]
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
  mod_name <- deparse(substitute(obj))[1]
  rlang::check_installed("xpose.nlmixr2")
  rlang::check_installed("nlmixr2")

  nlm_xpd <- xpose.nlmixr2::xpose_data_nlmixr2(
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
    backfill_nlmixr2_props()
  if (.skip_assoc) return(nlm_xpd)
  nlmixr2_prm_associations(nlm_xpd, quiet = nlm_xpd$options$quiet)
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
  assert_nlmixr2fit(xpdb)
  rlang::check_installed("rxode2") # This would be installed

  xpdb %>%
  # Condition number
  set_prop(
    condn = ifelse(is.null(xpdb$fit$conditionNumberCov), "na", paste(xpdb$fit$conditionNumberCov))
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

assert_nlmixr2fit <- function(xpdb, caller = parent.frame()) {
  xpa("true", test_nlmixr2_has_fit(xpdb), "No `nlmixr2` fit object found.", caller = caller)
}


#' @title get_prm equivalent for nlmixr2 fits
#'
#' @description
#' This is intended to match the <`xpose::get_prm`> rather than the
#' updated [`get_prm()`].
#'
#' @param xpdb <`xp_xtras`> With nlmixr2 fit
#' @param transform  <`logical`> as in [get_prm()]
#' @param show_all   <`logical`> as in [get_prm()]
#' @param quiet <`logical`> as in [get_prm()]
#'
#' @return a tibble with expected columns
get_prm_nlmixr2 <- function(
    xpdb,
    transform = formals(get_prm)$transform,
    show_all = formals(get_prm)$show_all,
    quiet = FALSE
) {
  if (!quiet) cli::cli_alert_info("{.strong nlmixr2} does not provide SE values for random effect parameters (this includes error parameters).")

  assert_nlmixr2fit(xpdb)
  fit <- xpdb$fit

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
    show_all_om <- tidyr::expand_grid(
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


  out <- dplyr::bind_rows(
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

  # Mask (change) any parameter values
  mask_df <- dplyr::tibble(name=character(0))
  if ("prm_mask" %in% names(xpdb)) mask_df <- xpdb$prm_mask %>%
    # Ensure no NAs
    dplyr::rows_patch(
      dplyr::select(out,name,value,se),
      by = "name",
      unmatched = "ignore"
    )
  out %>%
    # apply and transformations to data
    dplyr::rows_update(mask_df, by = "name") %>%
    dplyr::mutate(
      rse = ifelse(name %in% mask_df$name & !is.na(rse), se/value, rse)
    )
}

# Based on the xpdb software, use xpose::get_prm or nlmixr2 equivalent
hot_swap_base_get_prm <- function(xpdb, ...) {
  all_dots <- rlang::dots_list(..., .named = TRUE)
  if (xpose::software(xpdb)=="nonmem") {
    use_function <- xpose::get_prm
    use_dots <- all_dots[names(all_dots) %in% names(formals(use_function))]
  } else if (xpose::software(xpdb)=="nlmixr2") {
    assert_nlmixr2fit(xpdb)
    use_function <- get_prm_nlmixr2
    use_dots <- all_dots[names(all_dots) %in% names(formals(use_function))]
  }
  rlang::exec(
    use_function, xpdb, !!!use_dots
  )
}



#' Mutate parameter value without changing in source
#'
#' @description
#' Unexported function to provide the same mutate_prm api for nlmixr2
#' fits. Since the fit cannot be directly edited, this will add an
#' undocumented and unchecked of the xpose_data object with the updated
#' value and SE for the parameter.
#'
#'
#' @param xpdb <`xp_xtras`>
#' @param fortheta name of theta to be changed
#' @param newval new value to store for theta
#' @param se is new value to SE?
#'
#' @noRd
#'
mutate_mask <- function(
    xpdb, fortheta, newval, se=FALSE
    ) {
  # Build tibble of new values to upsert to mask table
  upsert_df <- dplyr::tibble(
    name = fortheta,
    value = newval
  )
  if (se) upsert_df <- dplyr::rename(upsert_df, se = value)
  if (!"prm_mask" %in% names(xpdb))
    xpdb$prm_mask = dplyr::tibble(name = character(0), value = numeric(0), se = numeric(0))
  xpdb$prm_mask <- dplyr::rows_upsert(
    xpdb$prm_mask,
    upsert_df,
    by = "name"
  )
  as_xp_xtras(xpdb)
}

#' Based on associations baked into nlmixr2, automatically add to xpose data
#'
#' @description
#' This function attempts to discern the associations between omegas and thetas
#' using information about mu referencing within the `nlmixr2` fit object.
#'
#'
#' @param xpdb <`xp_xtras`> object
#' @param dry_run <`logical`> Return a resulting information to compare against.
#' @param quiet <`logical`> Include extra information
#'
#' @details
#' Back-transformations are not as relevant here as they may seem. Manual
#' back-transformation with `backTransform()` only affects the display of the
#' back-transformed theta estimate (and CI), but does not impact the
#' relationship between EBEs and individual parameter estimates.
#'
#' @seealso [rxode2::ini()]
#' @return Object with filled `par`
#' @export
#'
#' @examples
#' \dontrun{
#' nlmixr2_warfarin %>%
#'   # This will add all log-normal and the logitnormal params
#'   nlmixr2_prm_associations() %>%
#'   # Make sure theta is in normal scale
#'   # rxode::expit could be plogis in this case
#'   mutate_prm(temax~rxode2::expit) %>%
#'   # Review results
#'   get_prm()
#'
#' }
#'
#'
nlmixr2_prm_associations <- function(xpdb, dry_run = FALSE, quiet) {
  assert_nlmixr2fit(xpdb)

  if (rlang::is_missing(quiet)) quiet <- xpdb$options$quiet

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
    #  Ignore parameters with no theta or eta association
    filter(!is.na(eta) & !is.na(theta)) %>%
    # Add transformations
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
    dplyr::relocate(param, .before = dplyr::everything()) %>%
    # Flag to ignore for inclusion (updated further down)
    dplyr::mutate(ignore = FALSE)

  # Check if all etatrans have a known pmxcv equivalent or build custom
  etatrans_pmxcv <- dplyr::bind_rows(
    dplyr::tibble(
      etatrans = "exp",
      # Define these as quosures so we can check for globally evaluable custom functions
      # Not very relevant for built-ins, but helpful for testing more custom situations
      pdist = list(rlang::quo(exp)), # get original with rlang::as_label
      qdist = list(rlang::quo(log)),
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
    dplyr::tibble(
      etatrans = "probitInv",
      pdist = list(rlang::quo(probitInv)),
      qdist = list(rlang::quo(probit)),
      dist = "custom"
    ),
    dplyr::tibble(
      etatrans = "", # no apparent transformation
      pdist = list(rlang::quo(c)),
      qdist = list(rlang::quo(c)),
      dist = "custom"
    ),
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
    lhs_tbl <- dplyr::mutate(lhs_tbl, ignore = etatrans %in% need_extra_params)
  }

  # This is a bit overengineered
  # Identify optimal q/pdist environment
  test_envs <- list(
    global = .GlobalEnv,
    rxode2 = rlang::ns_env("rxode2")
  )
  pdist_qdist_env <- function(xdist_quo) {
    # global gets piority in case user has masked paackage funs
    for (tenv in names(test_envs)) {
      env_quo <- rlang::quo_set_env(xdist_quo, env = test_envs[[tenv]])
      # Wrap the tidy_eval for the function in safely()
      test_fn <- purrr::safely(function() rlang::eval_tidy(env_quo))
      test_preres <- test_fn()
      if (is.null(test_preres$error))
        return(tenv)
    }
    NULL # return null if no known envinment
  }
  pdist_qdist_tester <- function(pdist_quo, qdist_quo) {
    # Determine if environment is identifiable
    pdist_envname <- pdist_qdist_env(pdist_quo)
    qdist_envname <- pdist_qdist_env(qdist_quo)
    xdist_exprs <- purrr::map_chr(list(pdist_quo, qdist_quo), rlang::as_label)
    if (is.null(pdist_envname) ||
        is.null(qdist_envname)) {
      null_env <- xdist_exprs[c(is.null(pdist_envname), is.null(qdist_envname))]
      if (!quiet)
        cli::cli_warn("Normal \U2192 Transformed or inverse function not in global or rxode2 environment. ({.code {null_env}})")
      return(FALSE)
    }
    # Ensure results are numeric and revere eachother
    pdist_fn <- purrr::safely(rlang::eval_tidy(rlang::quo_set_env(pdist_quo, test_envs[[pdist_envname]])))
    qdist_fn <- purrr::safely(rlang::eval_tidy(rlang::quo_set_env(qdist_quo, test_envs[[qdist_envname]])))
    probe_num <- 0.1
    pdist_test <- pdist_fn(probe_num)
    if (!is.null(pdist_test$error)) {
      if (!quiet)
        cli::cli_warn("Normal \U2192 Transformed function cannot be evaluated without error with input value {.code {probe_num}}. ({.code {xdist_exprs[1]}})")
      return(FALSE)
    }
    if (!is.numeric(pdist_test$result)) {
      if (!quiet)
        cli::cli_warn("Normal \U2192 Transformed function does not return numeric values with input value {.code {probe_num}}. ({.code {xdist_exprs[1]}})")
      return(FALSE)
    }
    qdist_test <- qdist_fn(pdist_test$result)
    if (!is.null(qdist_test$error)) {
      if (!quiet)
        cli::cli_warn("Transformed \U2192 Normal function cannot be evaluated without error with input value {.code {pdist_test$result}}. ({.code {xdist_exprs[2]}})")
      return(FALSE)
    }
    if (!isTRUE(all.equal(qdist_test$result,probe_num))) {
      if (!quiet)
        cli::cli_warn("Normal \U2192 Transformed is not reversible by {.code {xdist_exprs[2]}}. ({.code {xdist_exprs[1]}})")
      return(FALSE)
    }
    return(TRUE)
  }
  fmla_builder <- function(lhs,rhs_fun,rhs_inner) formula(paste0(lhs,"~",rhs_fun,"(",rhs_inner,")"))
  tidy_eval_builder <- function(label,env) sprintf(
    "rlang::eval_tidy(rlang::quo_set_env(rlang::quo(%s),%s))",
    label, env
  )
  transforms_to_apply <- lhs_tbl %>%
  # etatrans_pmxcv
  dplyr::rowwise() %>%
  dplyr::mutate(
    prm_assoc_formula = if (ignore == TRUE) {
      list(formula(".~."))
    } else if (
      etatrans %in% etatrans_pmxcv$etatrans &&
        # With non-custom dist
        etatrans_pmxcv[etatrans_pmxcv$etatrans == etatrans, ]$dist != "custom"
    ) {
      # Predefined etatrans
      list(fmla_builder(
        theta, etatrans_pmxcv[etatrans_pmxcv$etatrans == etatrans, ]$dist, eta
      ))
    } else if (
      # Custom etatrans
      etatrans %in% etatrans_pmxcv$etatrans
    ) {
      pdist_quo <- etatrans_pmxcv[etatrans_pmxcv$etatrans == etatrans, ]$pdist[[1]]
      qdist_quo <- etatrans_pmxcv[etatrans_pmxcv$etatrans == etatrans, ]$qdist[[1]]
      # Test that the quos are valid
      if (!pdist_qdist_tester(pdist_quo, qdist_quo)) {
        list(formula(".~."))
      } else {
        # Determine what to put as the environment
        pdist_env_char <- pdist_qdist_env(pdist_quo) %>%
          {
            ifelse(. == "global", ".GlobalEnv", paste0("rlang::ns_env('", ., "')"))
          }
        qdist_env_char <- pdist_qdist_env(qdist_quo) %>%
          {
            ifelse(. == "global", ".GlobalEnv", paste0("rlang::ns_env('", ., "')"))
          }
        # Build formula
        list(fmla_builder(
          theta, "custom", paste(c(
            eta,
            paste0("pdist=", tidy_eval_builder(rlang::as_label(pdist_quo), pdist_env_char)),
            paste0("qdist=", tidy_eval_builder(rlang::as_label(qdist_quo), qdist_env_char))
          ), collapse = ", ")
        ))
      }
    } else {
      list(formula(".~."))
    },
    ignore = prm_assoc_formula == formula(".~.")
  ) %>%
  dplyr::ungroup()

  if (dry_run) return(transforms_to_apply)

  # Should users be notified if an association will be added that may depend on theta being untransformed?
  if (!quiet) {
    may_need_to_update_theta <- transforms_to_apply %>%
      dplyr::filter(!ignore,
                    etatrans!="exp",
                    thetatrans!="") %>%
      dplyr::pull(theta)
    if (length(may_need_to_update_theta)!=0) {
      cli::cli_alert_info("May need to untransform thetas {.code {may_need_to_update_theta}} since the CV calculation will be dependent upon the untransformed value.")
      cli::cli_alert_info("This untransform must be done manually using {.help [{.fun mutate_prm}](xpose.xtras::mutate_prm)}.")
    }
  }

  # Build splice-friendly list of formulas to add
  arg_formula_list <- transforms_to_apply %>%
    dplyr::filter(!ignore) %>%
    dplyr::pull(prm_assoc_formula)
  if (length(arg_formula_list)==0) {
    if (!quiet)
      cli::cli_alert_info("No valid associations to add.")
    return(xpdb)
  }
  xpdb %>% add_prm_association(!!!arg_formula_list)
}


nlmixr2_duplicate_axis_text_helper <- function(
    test_for_sameness,
    xpdb_list,
    current_axis.text
) {
  if (any(duplicated(test_for_sameness))) {
    cli::cli_alert_warning("Duplicate values for default {.code axis.text}. Making result unique.")
    if (any(purrr::map_lgl(xpdb_list, ~ xpose::software(.x$xpdb)=="nlmixr2")))
      cli::cli_alert_info("For {.strong nlmixr2} models, sometimes '@file' is a better {.code axis.text}, instead of '{current_axis.text}'.")
    test_for_sameness <- make.unique(test_for_sameness)
  }
  test_for_sameness
}

# Not going to backfill this. Users will have to create and
# output their own likelihood estimate, just like one has to
# do if using M3 in NONMEM (acknowledging that is directly used
# in NONMEM, whereas it is implicit here). The data-raw for the
# M3 example shows how easy this is to do.
# backfill_nlmixr2_censlike <- function(
#     xpdb,
#
#     )
