

#' Derive full parameter set for mammillary PK model
#'
#' This function applies [`rxode2::rxDerived`] to model parameters.
#'
#' @param xpdb xpdb <`xpose_data`> object
#' @param .prm <`tidyselect`> Parameters to convert (if `NULL`, the function
#' will use `xp_var` `param` types)
#' @param .problem Optional. Problem to use.
#' @param quiet Optional. Extra output.
#' @param prefix If desired, apply prefix to new parameters.
#'
#' @returns <`data.frame`> of data with new parameters
#' @export
#' @rdname derive_prm
#' @examples
#' if (!rlang::is_installed("rxode2") ||
#'    !exists("rxDerived", envir = rlang::ns_env("rxode2"))) {
#'
#' nlmixr2_m3 %>%
#'   backfill_derived() %>%
#'   list_vars()
#'
#' derive_prm(nlmixr2_m3)
#'
#'
#' # If param has no vars, .prm should be set
#' pheno_base %>%
#'   backfill_derived(
#'     .prm = c(CL,V)
#'   ) %>%
#'   list_vars()
#' }
derive_prm <- function(
    xpdb,
    .prm = NULL,
    .problem,
    quiet = xpdb$options$quiet,
    prefix = "") {
  if (!rlang::is_installed("rxode2") ||
    !exists("rxDerived", envir = rlang::ns_env("rxode2"))) {
    cli::cli_abort("Need `rxode2` with the function `rxDerived` to use this feature.")
  }
  xpose::check_xpdb(xpdb, check = "data")
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)

  # If prm is null get var labeled param
  if (rlang::quo_is_null(rlang::enquo(.prm))) {
    rlang::try_fetch(
      .prm <- xp_var(xpdb, .problem = .problem, type = "param", silent = FALSE)$col,
      error = function(s) {
        rlang::abort(
          "Need to declare `.prm` or have at least one `param` type column.",
          parent = s
        )
      }
    )
  } else {
    # If prm is declared, set as param and then pull (to make sure they exist)
    cur_params <- xp_var(xpdb, .problem = .problem, type = "param", silent = TRUE)$col
    xpdb <- xpdb %>%
      set_var_types_x(.problem = .problem, na = dplyr::all_of(cur_params)) %>%
      set_var_types_x(.problem = .problem, param = {{ .prm }})
    .prm <- xp_var(xpdb, .problem = .problem, type = "param", silent = FALSE)$col
  }


  # Process data
  post_processing <- function(df) {
    # Extract parameter columns
    par_df <- dplyr::select(df, dplyr::all_of(.prm))
    # try to derive
    derived_df <- rxode2::rxDerived(par_df) %>%
      # to upper
      dplyr::rename_with(toupper) %>%
      # Append any user-specified prefix
      dplyr::rename_with(~ paste0(prefix, .x)) %>%
      # drop names already in prm (in fact any in df)
      dplyr::select(-dplyr::any_of(names(df)))
    # Bring parameters back
    df %>%
      dplyr::bind_cols(derived_df)
  }
  xpose::fetch_data(
    xpdb = xpdb,
    .problem = .problem,
    filter = xpose::only_distinct(xpdb, .problem, facets = NULL, quiet = quiet),
    post_processing = post_processing, quiet = quiet
  )
}


#' @param ... Passed to `derive_prm()`
#' @param group_vars Variable type(s) to join derived parameters on.
#'
#' @export
#' @rdname derive_prm
backfill_derived <- function(
    xpdb,
    .prm = NULL,
    .problem,
    quiet = xpdb$options$quiet,
    ...,
    group_vars = "id") {
  # Get dervided values
  derived <- derive_prm(xpdb = xpdb, .prm = {{ .prm }}, .problem = .problem, quiet = quiet, ...)

  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)

  # For problem, join derived parameters
  curr_prob_data <- xpdb$data$data[[.problem]]
  join_vars <- xp_var(xpdb = xpdb, .problem, type = group_vars)$col
  join_vars <- c(
    join_vars,
    # Add params to join vars
    names(dplyr::select(curr_prob_data, {{ .prm }}))
  )
  derived_cols <- setdiff(names(derived), names(curr_prob_data))
  to_join <- derived %>%
    dplyr::select(
      !!join_vars,
      # Any derived columns
      !!derived_cols
    )
  xpdb$data$data[[.problem]] <- curr_prob_data %>%
    dplyr::left_join(to_join, by = join_vars)
  xpdb$data <- xpose::xpdb_index_update(xpdb = xpdb, .problem = .problem)
  xpdb %>%
    as_xp_xtras() %>%
    set_var_types(.problem = .problem, param = c({{ .prm }}, dplyr::all_of(derived_cols)))
}

greek_letters <- c(
  "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta",
  "theta", "iota", "kappa", "lambda", "mu", "nu", "xi", "omicron",
  "pi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega"
)

#' Check for potential parameterization issues
#'
#' This function can help diagnose potential flip-flop or other
#' issues related to the parameterization of the model.
#'
#' The function prints output directly, not as an object.
#'
#' @param xpdb <`xpose_data`> object
#' @param df Optional <`data.frame`> of parameter values.
#' @param micro_pattern Regex. Pattern for microconstants
#' @param vol_pattern Regex. Pattern for volume parameter (should only match 1)
#' @param fo_abs First-order absorption parameter (singular, fixed, not regex).
#' @param fo_rates Derived ("macro") exponential rate constants (fixed). See Details
#' @param checks See Details
#' @param df_units Named list of units. If `NULL`, either ignore (`df`) or pull
#' from `xpdb` object.
#' @param .problem Used in fetching parameters.
#' @param quiet Should parameter fetching produce output?
#'
#' @details
#' A finding from these checks does not necessarily prove
#' the parameterization is erroneous (indeed, flip-flop PK can exist),
#' but coupled with other findings would help in diagnosing issues.
#'
#' For `fo_rates`, `"alpha_beta"` and `"lambda"` are convenience placeholders
#' meaning literally `c("ALPHA","BETA","GAMMA")` and `paste0("LAMBA",1:3)`,
#' respectively. If capitalization or competing names will be an issue, specify
#' a custom set of names (provide a character vector of names, do not pass
#' `"custom"` to the argument). If only a subset of `alpha_beta` or `lambda`
#' are available, but these are the parameterizations used (eg, only `ALPHA`)
#' these options can still be used. If `LAMBDA` is used alone, it will not
#' match the `"lambda"` default. If naming conventions are incompatible, it
#' is suggested `xpdb` or `df` be subject to mutation or renaming to use this
#' function.
#'
#' The available checks at this time are:
#' \itemize{
#'   \item `flip_flop` Checks if `fo_abs` are slower than the derived `fo_rates`.
#'   \item `neg_microvol` Checks if any microconstant or volume is negative. Note
#'   this check applies to parameterization of microconstants, so only a single volume
#'   (parameterizations with multiple volumes do not use microconstants) should match
#'   `vol_pattern`.
#'   \item `units_match` For any checks, verifies units are consistent. This
#'   check requires units are defined by [`set_var_units()`][xpose::set_var_units]
#'   or `df_units` for parameters applicable to a requested check.
#' }
#' Checks must be requested as a named list of these elements, either `TRUE` or
#' `FALSE` (truth determines if the test is done). If the default `NULL` is used,
#' test will be run if the required parameters are present.
#'
#'
#' @returns Nothing
#' @export
#'
#' @seealso [backfill_derived()]
#'
#' @examples
#'
#' if (!rlang::is_installed("rxode2") ||
#'    !exists("rxDerived", envir = rlang::ns_env("rxode2"))) {
#' nlmixr2_m3 %>%
#'   backfill_derived() %>%
#'   diagnose_constants(vol_pattern = "^V$")
#'
#' nlmixr2_m3 %>%
#'   backfill_derived() %>%
#'   diagnose_constants(
#'     vol_pattern = "^V$",
#'     df_units = list(KA = "1/hr", ALPHA = "1/hr"),
#'     checks = list(neg_microvol = FALSE)
#'   )
#'
#' # Using df form
#' derive_prm(nlmixr2_m3) %>%
#'   diagnose_constants(df = ., vol_pattern = "^V$")
#'
#' }
diagnose_constants <- function(
    xpdb,
    df = NULL,
    micro_pattern = "^K(\\d+|EL?)$",
    vol_pattern = "^V(C|D|1|2|)$",
    fo_abs = "KA",
    fo_rates = c(
      "alpha_beta",
      "lambda",
      "custom"
    ),
    checks = list(
      flip_flop = NULL,
      neg_microvol = NULL,
      units_match = NULL
    ),
    df_units = NULL, # if df, named list: list(column = unit)
    .problem,
    quiet = xpdb$options$quiet) {
  if (length(vol_pattern) > 1) {
    # Allow multiple, but concatenate regexes
    vol_pattern <- paste0("(", paste(vol_pattern, collapse = ")|("), ")")
  }


  # FO rates check
  default_fo_rates <- eval(formals()$fo_rates)
  if (length(fo_rates) > 1) {
    if (identical(fo_rates, default_fo_rates)) {
      fo_rates <- default_fo_rates[1]
    }
    # Alternative is user-defined values and fine
  } else if (fo_rates == "custom") {
    cli::cli_abort("Instead of `'custom'`, pass the actual names of the rate parameters as a character vector.")
  }
  if (fo_rates %in% default_fo_rates) {
    fo_rates <- switch(fo_rates,
      alpha_beta = toupper(greek_letters[1:3]),
      lambda = paste0("LAMBDA", 1:3)
    )
  }
  xpa("string", fo_abs, "For this check, can only have 1 absorption parameter. Perhaps derive a single value approximation if the model is complex?")

  # if df is provided, skip xpdb processing
  if (is.null(df)) {
    if (missing(xpdb)) {
      cli::cli_abort("Need `xpdb` or `df` for this function.")
    }
    xpose::check_xpdb(xpdb, check = "data")
    if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
    df <- xpose::fetch_data(
      xpdb = xpdb,
      .problem = .problem,
      filter = xpose::only_distinct(xpdb, .problem, facets = NULL, quiet = quiet),
      quiet = quiet
    )
    if (is.null(df_units)) {
      df_units <- get_index(xpdb, .problem = .problem) %>%
        {
          rlang::set_names(as.list(.$units), .$col)
        } %>%
        # unique
        .[!duplicated(.)] %>%
        # drop NA
        .[!purrr::map_lgl(., is.na)]
    }
  }

  # Determine df columns which match pattern
  # Avoid matching for tests that won't be done (so unit_match is still done)
  if ("flip_flop" %in% names(checks) &&
    rlang::is_false(checks$flip_flop)) {
    fo_abs <- fo_rates <- ""
  }
  if ("neg_microvol" %in% names(checks) &&
    rlang::is_false(checks$neg_microvol)) {
    micro_pattern <- vol_pattern <- "^$"
  }
  match_cols <- dplyr::select(
    df,
    dplyr::any_of(c(fo_abs, fo_rates)),
    dplyr::matches(micro_pattern),
    dplyr::matches(vol_pattern)
  ) %>%
    names()
  xpa("character", match_cols, "Need some columns that match function specifications.", min.len=1)
  xpa("character", match_cols[grepl(vol_pattern, match_cols)], "Volume should only match 1 column for checking.",
    min.len = 0, max.len = 1
  )

  # Check checks argument
  xpa("list", checks, "Checks should be a list.")
  default_check_names <- eval(formals()$checks) %>% names()
  if (any(!names(checks) %in% default_check_names)) {
    cli::cli_abort("Invalid check requested: {setdiff(names(checks), default_check_names)}")
  }
  checks <- modifyList(
    eval(formals()$checks),
    checks,
    keep.null = TRUE
  )
  # Based on column/unit availability, fill out NULL checks, or error if TRUE and cannot be done
  check_check <- function(found_val, set_val, label) {
    `if`(
      rlang::is_true(set_val) && !found_val,
      cli::cli_abort("Needed info not available for check `{label}`"),
      `if`(rlang::is_false(set_val), set_val, found_val)
    )
  }
  checks <- purrr::imap(checks, ~ switch(.y,
    flip_flop = {
      (all(fo_abs %in% match_cols) && any(fo_rates %in% match_cols)) %>%
        check_check(.x, .y)
    },
    neg_microvol = {
      (sum(
        grepl(micro_pattern, match_cols),
        grepl(vol_pattern, match_cols)
      ) > 0) %>%
        check_check(.x, .y)
    },
    units_match = {
      (length(df_units) > 0 && all(match_cols %in% names(df_units))) %>%
        check_check(.x, .y)
    }
  ))
  if (purrr::every(checks, isFALSE)) {
    cli::cli_warn("No valid checks to apply.")
    return(invisible())
  }


  # Walk through each check and output the results in a nice format
  cli_check <- function(ok, msg) {
    sym <- if (ok) cli::symbol$tick else cli::symbol$cross
    colf <- if (ok) cli::col_green else cli::col_red
    cli::cli_text(paste(colf(sym), msg))
  }
  # keep backward-compatible wrappers if you use them elsewhere
  pass_check <- function(msg) cli_check(TRUE, msg)
  fail_check <- function(msg) cli_check(FALSE, msg)
  # generic result emitter
  emit_check <- function(flags, msgs) {
    # flags: logical vector per row indicating a failing condition
    nrows <- length(flags)
    any_bad <- any(flags)
    pct_str <- sprintf("%.1f", 100 * mean(flags))

    if (nrows > 1 && any_bad) {
      fail_check(glue::glue(msgs$fail_multi, pct = pct_str))
    } else if (any_bad) {
      fail_check(msgs$fail_single)
    } else if (nrows > 1) {
      pass_check(msgs$pass_multi)
    } else {
      pass_check(msgs$pass_single)
    }
  }
  # -rowwise OR across a set of columns
  row_any_over <- function(df, cols, col_predicate) {
    # col_predicate: function(vec, name, df) -> logical vector (per-row flags)
    if (length(cols) == 0) {
      return(rep(FALSE, nrow(df)))
    }
    purrr::map(cols, function(nm) col_predicate(df[[nm]], nm, df)) %>%
      Reduce(`|`, .)
  }
  purrr::iwalk(checks, ~ {
    if (!.x) {
      return()
    }
    switch(.y,
      flip_flop = {
        cli::cli_alert_info("Checking for absorption flip-flop (first-order absorption slower than derived rate constants)...")
        cols <- intersect(fo_rates, match_cols)
        flags <- row_any_over(
          df, cols,
          function(vec, nm, df_) vec > df_[[fo_abs]]
        )
        emit_check(
          flags,
          msgs = list(
            fail_multi  = "Some parameter sets are suggestive of flip-flop ({pct}%).",
            fail_single = "Parameters are suggestive of flip-flop.",
            pass_multi  = "No parameter sets are suggestive of flip-flop.",
            pass_single = "Parameters are not suggestive of flip-flop."
          )
        )
      },
      neg_microvol = {
        cli::cli_alert_info("Checking for negative microconstants or volume...")
        cols <- match_cols[grepl(micro_pattern, match_cols) | grepl(vol_pattern, match_cols)]
        flags <- row_any_over(
          df, cols,
          function(vec, nm, df_) vec < 0
        )

        emit_check(
          flags,
          msgs = list(
            fail_multi  = "Some parameter sets have negative microconstants or volumes ({pct}%).",
            fail_single = "Parameters have negative microconstants or volumes.",
            pass_multi  = "No parameter sets have negative microconstants or volumes.",
            pass_single = "Parameters do not have negative microconstants or volumes."
          )
        )
      },
      units_match = {
        cli::cli_alert_info("Checking that compared units match...")
        check_micros <- TRUE
        check_vols <- TRUE
        if (checks$neg_microvol) {
          check_micros <- match_cols %>%
            .[grepl(micro_pattern, .)] %>%
            purrr::map_chr(~ df_units[[.x]]) %>%
            {
              all(. == .[1L])
            }
          # This behaves like multiple volumes are used, but should only be 1
          check_vols <- match_cols %>%
            .[grepl(vol_pattern, .)] %>%
            purrr::map_chr(~ df_units[[.x]]) %>%
            {
              all(. == .[1L])
            }
        }
        check_fo <- TRUE
        if (checks$flip_flop) {
          check_fo <- intersect(
            match_cols,
            c(fo_abs, fo_rates)
          ) %>%
            purrr::map_chr(~ df_units[[.x]]) %>%
            {
              all(. == .[1L])
            }
        }
        if (!check_micros || !check_vols || !check_fo) {
          failed <- c(
            "microconstants/volumes",
            "first-order rates"
          )[c(
            (!check_micros || !check_vols),
            !check_fo
          )]
          fail_check(glue::glue("Units don't match for {failed}"))
        } else {
          pass_check("All relevant units seem to match.")
        }
      }
    )
  })
}
