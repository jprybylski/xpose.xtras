derive_prm <- function(
    xpdb,
    .prm = NULL,
    .problem,
    quiet = xpdb$options$quiet,
    prefix = ""
  ) {
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
      dplyr::rename_with(~paste0(prefix,.x)) %>%
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


backfill_derived <- function(
    xpdb,
    .prm = NULL,
    .problem,
    quiet = xpdb$options$quiet,
    ...,
    group_vars = "id"
) {

  # Get dervided values
  derived <- derive_prm(xpdb=xpdb, .prm = {{.prm}}, .problem=.problem, quiet=quiet, ...)

  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)

  # For problem, join derived parameters
  curr_prob_data <- xpdb$data$data[[.problem]]
  join_vars <- xp_var(xpdb = xpdb, .problem, type = group_vars)$col
  join_vars <- c(join_vars,
                 # Add params to join vars
                 names(dplyr::select(curr_prob_data, {{.prm}})))
  derived_cols <- setdiff(names(derived),names(curr_prob_data))
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
    set_var_types(.problem = .problem, param = c({{.prm}}, dplyr::all_of(derived_cols)))
}


diagnose_constasts <- function(
    xpdb,
    df = NULL,
    micro_pattern = "^K\\d+$",
    vol_pattern = "^V((C)|(P\\d?))$",
    fo_abs = "KA",
    fo_elim = "KEL",
    checks = list(
      flip_flop = NULL,
      neg_microvol = NULL,
      units_match = NULL
    ),
    df_units = NULL, # if df, named list: list(column = unit)
    .problem,
    quiet = xpdb$options$quiet
) {
  # if df is provided, skip xpdb processing
  if (is.null(df)) {
    xpose::check_xpdb(xpdb, check = "data")
    if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
    df <- xpose::fetch_data(
      xpdb = xpdb,
      .problem = .problem,
      filter = xpose::only_distinct(xpdb, .problem, facets = NULL, quiet = quiet),
      quiet = quiet
    )
    df_units <- get_index(xpdb, .problem=.problem) %>%
      {rlang::set_names(as.list(.$units), .$col)} %>%
      # unique
      .[!duplicated(.)] %>%
      # drop NA
      .[!purrr::map_lgl(., is.na)]
  }

  # Determine df columns which match pattern
  match_cols <- dplyr::select(df,
                              dplyr::any_of(c(fo_abs,fo_elim)),
                              dplyr::matches(micro_pattern),
                              dplyr::matches(vol_pattern)
                              ) %>%
    names()
  xpa("character", match_cols, "Need some columns that match function specifications.")

  # Based on column/unit availability, fill out NULL checks, or error if TRUE and cannot be done
  check_check <- function(found_val, set_val, label) {
    `if`(
      rlang::is_true(set_val) && !found_val,
      cli::abort("Needed info not available for check `{label}`"),
      `if`(rlang::is_false(set_val), set_val, found_val)
      )
  }
  checks <- purrr::imap(checks, ~dplyr::case_when(
    .y == "flip_flop" ~ all(c(fo_abs, fo_elim) %in% match_cols) %>%
      check_check(.x, .y),
    .y == "neg_microvol" ~ sum(
      grepl(micro_pattern, match_cols),
      grepl(vol_pattern, match_cols)
    ) %>%
      check_check(.x, .y),
    .y == "units_match" ~ all(names(df_units) %in% match_cols) %>%
      check_check(.x, .y)
  ))


  # Walk through each check and output the results in a nice format
  purrr::iwalk(checks, ~{
    if (!.x) return()
    switch(
      .y,
      flip_flop = {

      },
      neg_microvol = {

      },
      units_match = {

      }
    )
  })
}
