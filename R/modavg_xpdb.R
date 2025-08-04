#' Create a model-averaged xpose data object
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function is a helper for plotting functions where models in
#' an `xpose_set` can be averaged together. The implementation attempts
#' to match and extend from the cited prior work.
#'
#'
#' @param xpdb_s <`xpose_set`> object
#' @param ... <`tidyselect`> of models in set. If empty, all models are
#' used in order of their position in the set. May also use a formula,
#' which will just be processed with `all.vars()`.
#' @param .lineage <`logical`> where if `TRUE`, `...` is processed
#' @param avg_cols <`tidyselect`> columns in data to average
#' @param avg_by_type <`character`> Mainly for use in wrapper functions.
#' Column type to average, but resulting column names must be valid
#' for `avg_cols` (ie, same across all objects in the set). `avg_cols` will
#' be overwritten.
#' @param algorithm <`character`> Model selection or model averaging
#' @param weight_type <`character`> Individual-level averaging or by full dataset.
#' @param auto_backfill <`logical`> If true, <[`backfill_iofv`]> is automatically
#' applied.
#' @param weight_basis <`character`> Weigh by OFV (default), AIC or residual.
#' @param res_col <`character`> Column to weight by if `"res"` weight basis.
#' @param quiet <`logical`> Minimize extra output.
#'
#' @return Weight-averaged <`xpose_data`> object.
#' @export
#'
#' @references
#' Uster, D.W., Stocker, S.L., Carland, J.E., Brett, J., Marriott, D.J.E.,
#' Day, R.O. and Wicha, S.G. (2021), A Model Averaging/Selection Approach
#' Improves the Predictive Performance of Model-Informed Precision Dosing:
#' Vancomycin as a Case Study. Clin. Pharmacol. Ther., 109: 175-183.
#' https://doi.org/10.1002/cpt.2065
#'
#' @examples
#'
#' pheno_set %>%
#'   modavg_xpdb(
#'     avg_cols = IPRED,
#'     auto_backfill = TRUE,
#'     algorithm = "maa",
#'     weight_basis = "aic"
#'   )
#'
modavg_xpdb <- function(
    xpdb_s,
    ...,
    .lineage = FALSE,
    avg_cols = NULL,
    avg_by_type = NULL,
    algorithm = c("maa", "msa"),
    weight_type = c("individual", "population"),
    auto_backfill = FALSE,
    weight_basis = c("ofv", "aic", "res"),
    res_col = "RES",
    quiet) {
  check_xpose_set(xpdb_s, .warn = FALSE)

  # Make sure dots are unnamed
  rlang::check_dots_unnamed()

  n_set_dots(xpdb_s, ..., .lineage=.lineage) # makes `mods`

  pre_process <- function(x) unfocus_xpdb(x)
  if (auto_backfill == TRUE) pre_process <- function(x) focus_qapply(x, backfill_iofv)
  xpose_subset <- xpdb_s %>%
    pre_process() %>%
    select(!!mods)

  # extra checks
  if (missing(quiet)) {
    quiet <- xpose_subset[[1]]$xpdb$options$quiet
  }
  if (rlang::quo_is_null(rlang::enquo(avg_cols)) && is.null(avg_by_type)) {
    rlang::abort("Columns to average are required. Provide argument `avg_cols`.")
  }
  algorithm <- rlang::arg_match(algorithm, values = c("maa", "msa"))
  weight_type <- rlang::arg_match(weight_type, values = c("population", "individual"))
  weight_basis <- rlang::arg_match(weight_basis, values = c("ofv", "aic", "res"))
  if (length(res_col) > 1 && weight_basis == "res") {
    rlang::abort("Only one residual column can be used as the weighting basis.")
  }

  xpdb_l <- purrr::map(xpose_subset, ~ .x$xpdb)

  if (!is.null(avg_by_type)) {
    avg_cols <- xp_var(xpdb_l[[1]], type = avg_by_type, silent = TRUE)$col %>%
      {
        rlang::quo(dplyr::any_of(.))
      }
  }

  # Get combined xpdb
  rlang::try_fetch(
    xpdb_f <- franken_xpdb(
      !!!xpdb_l,
      .types = c("iofv"),
      .cols = c({{ avg_cols }}, dplyr::all_of(res_col)) # TODO: update franken_props here
    ),
    error = function(s) {
      rlang::abort(auto_backfill_suggestion, parent = s)
    }
  )
  # To make working with new columns easier
  ofv_cols <- purrr::map_chr(
    xpdb_l,
    ~ xp_var(.x, type = "iofv", silent = TRUE)$col[1]
  )
  ofv_frk_cols <- paste0(ofv_cols, "_", seq_along(ofv_cols))
  res_cols <- paste0(res_col, "_", seq_along(xpdb_l))
  avgd_cols <- get_index(xpdb_f) %>% # < can do it like this because all cols have to be same
    dplyr::pull(col) %>%
    setNames(., .) %>%
    as.list() %>%
    tibble::as_tibble() %>%
    dplyr::select({{ avg_cols }}) %>%
    names()
  avgd_cols_num <- purrr::map(avgd_cols, ~ paste0(.x, "_", seq_along(xpdb_l)))
  avgd_frk_cols <- purrr::list_c(avgd_cols_num)
  # Add AIC
  aic_cols <- paste0("AIC_", seq_along(xpdb_l))
  if (weight_basis == "aic") {
    for (prob in xpose::all_data_problem(xpdb_f)) {
      id_col <- xp_var(xpdb_f, .problem = prob, type = "id", silent = TRUE)$col
      for (i in seq_along(xpdb_l)) {
        xpdb <- xpdb_l[[i]]
        new_col <- aic_cols[i]
        ofv_col <- ofv_frk_cols[i]
        npars <- hot_swap_base_get_prm(xpdb, .problem = prob, quiet = TRUE) %>%
          dplyr::pull(fixed) %>%
          magrittr::not() %>%
          sum()
        xpdb_f <- xpdb_f %>%
          `if`(
            weight_type == "individual",
            group_by_x(., across(any_of(id_col)), .problem = prob),
            .
          ) %>%
          mutate_x(
            !!new_col := sum(.data[[ofv_col]][!duplicated(.data[[id_col]])]) + 2 * .env$npars,
            .problem = prob
          ) %>%
          `if`(
            weight_type == "individual",
            ungroup_x(., .problem = prob),
            .
          )
      }
    }
  } else {
    for (i in seq_along(xpdb_l)) {
      new_col <- aic_cols[i]
      xpdb_f <- xpdb_f %>%
        mutate_x(
          !!new_col := 0
        )
    }
  }
  # make sure ofv is full data ofv when population
  if (weight_basis == "ofv" && weight_type=="population") {
    for (prob in xpose::all_data_problem(xpdb_f)) {
      id_col <- xp_var(xpdb_f, .problem = prob, type = "id", silent = TRUE)$col
      for (i in seq_along(xpdb_l)) {
        ofv_col <- ofv_frk_cols[i]
        xpdb_f <- xpdb_f %>%
          mutate_x(
            !!ofv_col := sum(.data[[ofv_col]][!duplicated(.data[[id_col]])]),
            .problem = prob
          )
      }
    }
  }


  macols <- list(
    ofv = "maOFV",
    res = "maRES",
    aic = "maAIC"
  )
  mod_ave_fn <- function(df) {
    # expect to be grouped if individual weighting
    # reshape for either case
    df_new <- df %>%
      # drop any columns that will be values pivot
      dplyr::select(-any_of(
        unlist(macols, use.names = FALSE)
      ), -{{ avg_cols }}) %>%
      # Rename to ensure consistency
      dplyr::rename_with(~ {
        dplyr::case_when(
          .x %in% ofv_frk_cols ~ stringr::str_replace(.x, "^.+_(\\d+)$", paste0(macols$ofv, "_\\1")),
          .x %in% res_cols ~ stringr::str_replace(.x, "^.+_(\\d+)$", paste0(macols$res, "_\\1")),
          .x %in% aic_cols ~ stringr::str_replace(.x, "^.+_(\\d+)$", paste0(macols$aic, "_\\1")),
          TRUE ~ .x
        )
      }) %>%
      # Pivot
      tidyr::pivot_longer(
        cols = c(
          dplyr::all_of(avgd_frk_cols),
          dplyr::starts_with(macols$ofv),
          dplyr::starts_with(macols$res),
          dplyr::starts_with(macols$aic)
        ),
        names_to = c(".value", "model"),
        names_sep = "_"
      ) %>%
      dplyr::arrange(model)
    # Some basis column transformations to directly exponentiate
    basis_col <- macols[[weight_basis]]
    if (weight_basis %in% c("ofv", "aic")) {
      df_new <- df_new %>%
        # keep original basis
        dplyr::mutate(compare_basis = .data[[basis_col]]) %>%
        dplyr::mutate(!!basis_col := -0.5 * .data[[basis_col]])
    }
    if (weight_basis %in% c("res")) {
      df_new_x <- df_new %>%
        dplyr::group_by(model, .add = dplyr::is_grouped_df(.)) %>%
        # This will be biased if it doesn't ignore dosing
        # TODO: check best way to only consider obs here
        # keep original basis sum
        dplyr::mutate(compare_basis = sum(.data[[basis_col]])) %>%
        dplyr::mutate(!!basis_col := -0.5 * sum(.data[[basis_col]])) %>%
        dplyr::ungroup()
      df_new <- df_new %>%
        dplyr::mutate(!!basis_col := df_new_x[[basis_col]][dplyr::cur_group_rows()]) %>%
        dplyr::mutate(compare_basis = df_new_x$compare_basis[dplyr::cur_group_rows()])
    }
    # Final algorith implementation
    if (algorithm == "maa") {
      df_new <- df_new %>%
        dplyr::mutate(across(
          {{ avg_cols }}, function(.x) {
            modnum <- as.numeric(.data$model)
            basis_val <- (exp(.data[[basis_col]][!duplicated(model)]))
            basis_vals <- basis_val %>%
              rep(each = sum(modnum == 1))

            new_vals <- .x * basis_vals /
              sum(basis_val)
            matrix(new_vals, ncol = max(modnum)) %>%
              rowSums() %>%
              rep(max(modnum))
          }
        ))
    } else if (algorithm == "msa") {
      df_new <- df_new %>%
        dplyr::mutate(across(
          {{ avg_cols }}, function(.x) {
            modnum <- as.numeric(.data$model)
            .x[.data$compare_basis == min(.data$compare_basis)] %>%
              # if multiple minimums, just pick first
              .[1:sum(modnum == 1)] %>%
              rep(max(modnum))
          }
        ))
    }
    df_new %>%
      `if`(dplyr::is_grouped_df(.), dplyr::ungroup(.), .) %>%
      dplyr::filter(model == 1) %>%
      dplyr::select({{ avg_cols }})
  }

  for (prob in xpose::all_data_problem(xpdb_f)) {
    id_col <- xp_var(xpdb_f, .problem = prob, type = "id", silent = TRUE)$col
    # get data
    modav_dat <- xpose::get_data(xpdb_f, .problem = prob, quiet = TRUE) %>%
      # group as needed
      `if`(weight_type == "individual", dplyr::group_by(., across(any_of(id_col))), .) %>%
      # get model averaged value
      mod_ave_fn()

    # use mutate_x to replace value in xpdb object
    xpdb_f <- xpdb_f %>%
      mutate_x(across(
        {{ avg_cols }}, ~ modav_dat[[dplyr::cur_column()]]
      ), .problem = prob)
  }
  xpdb_f
}

auto_backfill_suggestion <- paste0(
  "Failed to combine `xpose_data` objects. ",
  "Individual OFV is required in data even if not used for averaging. ",
  "Setting `auto_backfill=TRUE` may help."
)
