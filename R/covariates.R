######
# Covariate plots
######

#' Grid plots
#'
#' @description
#' This is essentially a wrapper around [`ggpairs`][GGally::ggpairs],
#' except it uses `xpose` motifs and styling. Note that this function
#' produces a lot of repetitive output if `quiet=FALSE`; this may not
#' be an issue, but it could look like an error has occurred if many covariates
#' and individual parameter estimates are included.
#'
#'
#' @rdname grid_plots
#'
#' @param xpdb <`xp_xtras> or  <`xpose_data`> object
#' @param mapping `ggplot2` style mapping
#' @param etavar `tidyselect` for `eta` variables
#' @param cols `tidyselect` for covariates variables
#' @param covvar For `eta_vs_cov_grid` only: an alias for `cols` (matching
#' the `covvar` argument of [`eta_vs_contcov()`]/[`eta_vs_catcov()`]). If
#' supplied (non-`NULL`), takes precedence over `cols`.
#' @param covtypes Subset to specific covariate type?
#' @param show_n Count the number of `ID`s in each category
#' @param drop_fixed As in `xpose`
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param etacov For`eta_vs_cov_grid`, `eta` are sorted after covariates to
#' give an `x` orientation to covariate relationships.
#' @param pairs_opts List of arguments to pass to `_opts`. See <[`xplot_pairs`]>
#' @param .problem Problem number
#' @param quiet Silence extra debugging output
#' @param ... Passed to `xplot_pairs`
#'
#' @return `xp_tras_plot` object
#' @export
#'
#'
#' @examples
#' \donttest{
#'
#' eta_grid(xpdb_x)
#' cov_grid(xpdb_x)
#' eta_vs_cov_grid(xpdb_x)
#'
#' # Labels and units are also supported
#' xpdb_x %>%
#'   xpose::set_var_labels(AGE="Age", MED1 = "Digoxin") %>%
#'   xpose::set_var_units(AGE="yrs") %>%
#'   set_var_levels(SEX=lvl_sex(), MED1 = lvl_bin()) %>%
#'   eta_vs_cov_grid()
#'
#' }
eta_grid <- function(xpdb,
                     mapping  = NULL,
                     etavar = NULL,
                     drop_fixed = TRUE,
                     title    = 'Eta correlations | @run',
                     subtitle = 'Based on @nind individuals, Eta shrink: @etashk',
                     caption  = '@dir',
                     tag      = NULL,
                     pairs_opts,
                     .problem,
                     quiet,
                     ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet

  # Get eta col(s)
  .etavar_quo <- rlang::enquo(etavar)
  eta_col <- resolve_var_cols(
    xpdb, .problem, type = 'eta', varsel = .etavar_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = 'etavar', label = 'eta'
  )
  etavar <- eta_col # overwrite the raw promise -- see resolve_var_cols() note

  # Eta label consistency
  if (xpose::software(xpdb) == 'nonmem') {
    eta_col_old <- eta_col
    eta_col_new <- stringr::str_replace(eta_col_old, "^ET(A?)(\\d+)$", "ETA(\\2)")
    post_processing_eta <-  function(x) {
      x %>%
        dplyr::rename(!!!rlang::set_names(eta_col_old, eta_col_new))
    }
    eta_col <- eta_col_new
  } else {
    post_processing_eta <- function(x) x
  }
  post_processing <- function(x) {
    post_processing_eta(x) %>%
      dplyr::select(!!eta_col)
  }

  opt <- xpose::data_opt(.problem = .problem,
                         filter = xpose::only_distinct(xpdb, .problem, NULL, quiet),
                         post_processing = post_processing)

  if (missing(pairs_opts)) pairs_opts <- list()
  pairs_opts_ <- pairs_opts_defaults(pairs_opts)

  xplot_pairs(
    xpdb,
    mapping   = mapping,
    cont_opts = pairs_opts_$cont_opts,
    dist_opts = pairs_opts_$dist_opts,
    cat_opts = pairs_opts_$cat_opts,
    contcont_opts = pairs_opts_$contcont_opts,
    catcont_opts = pairs_opts_$catcont_opts,
    catcat_opts = pairs_opts_$catcat_opts,
    title     = title,
    subtitle  = subtitle,
    caption   = caption,
    tag       = tag,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    opt=opt,
    quiet=quiet,
    ...
  )
}


#' @rdname grid_plots
#' @export
cov_grid <- function(xpdb,
                     mapping  = NULL,
                     cols = NULL,
                     covtypes = c("cont","cat"),
                     show_n = TRUE,
                     drop_fixed = TRUE,
                     title    = 'Covariate relationships | @run',
                     subtitle = 'Based on @nind individuals',
                     caption  = '@dir',
                     tag      = NULL,
                     pairs_opts,
                     .problem,
                     quiet,
                     ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet

  # Get col(s)
  valid_covtypes <- eval(formals()$covtypes)
  if (is.null(covtypes) || any(!covtypes %in% valid_covtypes)) {
    cli::cli_abort("Invalid `covtype`(s): {setdiff(covtypes, valid_covtypes)}")
  }
  get_govs <- paste0(covtypes, "cov")
  .cols_quo <- rlang::enquo(cols)
  cov_col <- resolve_var_cols(
    xpdb, .problem, type = get_govs, varsel = .cols_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = 'cols', label = paste0("(", paste(covtypes, collapse = ", "), ") covariate")
  )
  cols <- cov_col # overwrite the raw promise -- see resolve_var_cols() note

  # Set cov factor to label and units, if relevant
  lvld_cov <- cov_col[cov_col %in% xp_var(xpdb, .problem, type = "catcov")$col]
  if (!check_xpdb_x(xpdb, .warn=FALSE)) {
    post_processing_cov <- apply_lul_wide(xpdb = xpdb, cols=cov_col,
                                          lvl_cols=lvld_cov, .problem = .problem)
    if (show_n && !quiet) cli::cli_inform("Cannot show N unless xpdb is converted to a cross-compatible xp_xtras object. `as_xpdb_x()` should do this.")
  } else {
    post_processing_cov <- apply_lul_wide(xpdb = xpdb, cols=cov_col,
                                          lvl_cols=lvld_cov, .problem = .problem, show_n = show_n)
  }
  post_processing <- function(x) {
    orig_names <- names(x)
    proc_df <- post_processing_cov(x)
    new_names <- names(proc_df)
    # only return processed data
    return_names <- new_names[match(cov_col, orig_names)]
    dplyr::select(proc_df, !!return_names)
  }

  opt <- xpose::data_opt(.problem = .problem,
                         filter = xpose::only_distinct(xpdb, .problem, NULL, quiet),
                         post_processing = post_processing)

  if (missing(pairs_opts)) pairs_opts <- list()
  pairs_opts_ <- pairs_opts_defaults(pairs_opts)

  xplot_pairs(
    xpdb,
    mapping   = mapping,
    cont_opts = pairs_opts_$cont_opts,
    dist_opts = pairs_opts_$dist_opts,
    cat_opts = pairs_opts_$cat_opts,
    contcont_opts = pairs_opts_$contcont_opts,
    catcont_opts = pairs_opts_$catcont_opts,
    catcat_opts = pairs_opts_$catcat_opts,
    title     = title,
    subtitle  = subtitle,
    caption   = caption,
    tag       = tag,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    opt=opt,
    quiet=quiet,
    ...
  )
}

#' @rdname grid_plots
#' @export
eta_vs_cov_grid <- function(xpdb,
                            mapping  = NULL,
                            etavar = NULL,
                            cols = NULL,
                            covvar = NULL,
                            covtypes = c("cont","cat"),
                            show_n = TRUE,
                            drop_fixed = TRUE,
                            title    = 'Eta covariate correlations | @run',
                            subtitle = 'Based on @nind individuals, Eta shrink: @etashk',
                            caption  = '@dir',
                            tag      = NULL,
                            etacov = TRUE,
                            pairs_opts,
                            .problem,
                            quiet,
                            ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet

  # Get eta col(s)
  .etavar_quo <- rlang::enquo(etavar)
  eta_col <- resolve_var_cols(
    xpdb, .problem, type = 'eta', varsel = .etavar_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = 'etavar', label = 'eta'
  )
  etavar <- eta_col # overwrite the raw promise -- see resolve_var_cols() note

  # Get cov col(s)
  valid_covtypes <- eval(formals()$covtypes)
  if (is.null(covtypes) || any(!covtypes %in% valid_covtypes)) {
    cli::cli_abort("Invalid `covtype`(s): {setdiff(covtypes, valid_covtypes)}")
  }
  get_govs <- paste0(covtypes, "cov")
  .cols_quo <- rlang::enquo(cols)
  .covvar_quo <- rlang::enquo(covvar)
  # `covvar` is an alias for `cols` (see #82) -- takes precedence when supplied
  cols_arg_name <- 'cols'
  if (!rlang::quo_is_null(.covvar_quo)) {
    .cols_quo <- .covvar_quo
    cols_arg_name <- 'covvar'
  }
  cov_col <- resolve_var_cols(
    xpdb, .problem, type = get_govs, varsel = .cols_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = cols_arg_name, label = paste0("(", paste(covtypes, collapse = ", "), ") covariate")
  )
  cols <- cov_col # overwrite the raw promise -- see resolve_var_cols() note
  covvar <- cov_col # overwrite the raw promise -- see resolve_var_cols() note

  # Eta label consistency
  if (xpose::software(xpdb) == 'nonmem') {
    eta_col_old <- eta_col
    eta_col_new <- stringr::str_replace(eta_col_old, "^ET(A?)(\\d+)$", "ETA(\\2)")
    post_processing_eta <-  function(x) {
      x %>%
        dplyr::rename(!!!rlang::set_names(eta_col_old, eta_col_new))
    }
    eta_col <- eta_col_new
  } else {
    post_processing_eta <- function(x) x
  }


  # Set cov factor to label and units, if relevant
  lvld_cov <- cov_col[cov_col %in% xp_var(xpdb, .problem, type = "catcov", silent = TRUE)$col] # silent=TRUE or else this throws error
  if (!check_xpdb_x(xpdb, .warn=FALSE)) {
    post_processing_cov <- apply_lul_wide(xpdb = xpdb, cols=cov_col, lvl_cols=lvld_cov, .problem = .problem)
    if (show_n && !quiet) cli::cli_inform("Cannot show N unless xpdb is converted to a cross-compatible xp_xtras object. `as_xpdb_x()` should do this.")
  } else {
    post_processing_cov <- apply_lul_wide(xpdb = xpdb, cols=cov_col, lvl_cols=lvld_cov, .problem = .problem, show_n = show_n)
  }

  post_processing <- function(x) {
    orig_names <- names(x)
    proc_df <- post_processing_eta(x) %>%
      post_processing_cov()
    new_names <- names(proc_df)
    # only return processed data
    return_names <- new_names[match(cov_col, orig_names)]

    if (etacov) {
      list_first <- return_names
      list_second <- eta_col
    } else {
      list_first <- eta_col
      list_second <- return_names
    }

    dplyr::select(proc_df, !!list_first, !!list_second)
  }

  opt <- xpose::data_opt(.problem = .problem,
                         filter = xpose::only_distinct(xpdb, .problem, NULL, quiet),
                         post_processing = post_processing)

  if (missing(pairs_opts)) pairs_opts <- list()
  pairs_opts_ <- pairs_opts_defaults(pairs_opts)

  xplot_pairs(
    xpdb,
    mapping   = mapping,
    cont_opts = pairs_opts_$cont_opts,
    dist_opts = pairs_opts_$dist_opts,
    cat_opts = pairs_opts_$cat_opts,
    contcont_opts = pairs_opts_$contcont_opts,
    catcont_opts = pairs_opts_$catcont_opts,
    catcat_opts = pairs_opts_$catcat_opts,
    title     = title,
    subtitle  = subtitle,
    caption   = caption,
    tag       = tag,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    opt=opt,
    quiet=quiet,
    ...
  )
}


#' Eta continuous covariate plots (typical)
#'
#' @param xpdb <`xp_xtras> or <`xpose_data`> object
#' @param mapping `ggplot2` style mapping
#' @param etavar `tidyselect` for `eta` variables
#' @param covvar `tidyselect` for continuous covariate variables; `NULL`
#' (default) selects every continuous covariate in the `xpdb` data index.
#' @param drop_fixed As in `xpose`
#' @param linsm If `type` contains "s" should the smooth method by `lm`?
#' @param type Passed to `xplot_scatter`
#' @param list <`logical`> Only relevant when `etavar` resolves to more
#' than one eta. If `TRUE` (default, for backwards compatibility), returns
#' a plain list of one plot per eta. If `FALSE`, all etas are instead
#' combined onto one shared plot -- faceted by eta, in addition to the
#' existing per-covariate facet -- automatically paginating (at most 9
#' panels per page, i.e. `ncol`/`nrow` of 3) via `xpose`'s own
#' `facet_wrap_paginate` mechanism. Printing the returned plot renders
#' every page; pass `page` to `print()` to select a specific one.
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param log Log scale covariate value?
#' @param guide Add guide line?
#' @param facets Additional facets
#' @param .problem Problem number
#' @param quiet Silence output
#' @param ... Any additional aesthetics.
#'
#' @export
#' @returns The desired plot, or (when `etavar` resolves to more than one
#' eta and `list = TRUE`) a plain list of one plot per eta.
#'
#' @examples
#' \donttest{
#'
#' eta_vs_contcov(xpdb_x)
#'
#' # Labels and units are also supported
#' xpdb_x %>%
#'   xpose::set_var_labels(AGE="Age", MED1 = "Digoxin") %>%
#'   xpose::set_var_units(AGE="yrs") %>%
#'   set_var_levels(SEX=lvl_sex(), MED1 = lvl_bin()) %>%
#'   eta_vs_contcov()
#'
#' # Combine all etas onto one shared, faceted plot instead of a list
#' eta_vs_contcov(xpdb_x, list = FALSE)
#'
#' # Restrict to specific covariates with covvar, just like etavar
#' eta_vs_contcov(xpdb_x, covvar = AGE)
#' }
eta_vs_contcov <- function(xpdb,
                           mapping  = NULL,
                           etavar = NULL,
                           covvar = NULL,
                           drop_fixed = TRUE,
                           linsm = FALSE,
                           type     = 'ps',
                           list     = TRUE,
                           title    = 'Eta versus continuous covariates | @run',
                           subtitle = 'Based on @nind individuals, Eta shrink: @etashk',
                           caption  = '@dir',
                           tag      = NULL,
                           log      = NULL,
                           guide    = TRUE,
                           facets,
                           .problem,
                           quiet,
                           ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  checkmate::assert_flag(list)

  if (missing(facets)) facets <- xpose::add_facet_var(facets = xpdb$xp_theme$facets,
                                                      variable = 'variable')

  # Get eta col(s)
  .etavar_quo <- rlang::enquo(etavar)
  eta_col <- resolve_var_cols(
    xpdb, .problem, type = 'eta', varsel = .etavar_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = 'etavar', label = 'eta'
  )
  etavar <- eta_col # overwrite the raw promise -- see resolve_var_cols() note

  # Get cov col(s)
  .covvar_quo <- rlang::enquo(covvar)
  cov_col <- resolve_var_cols(
    xpdb, .problem, type = 'contcov', varsel = .covvar_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = 'covvar', label = 'continuous covariate'
  )
  covvar <- cov_col # overwrite the raw promise -- see resolve_var_cols() note

  if (length(eta_col)>1 && list) {
    return(purrr::map(eta_col, function(x) eta_vs_contcov(xpdb = xpdb,
                                                          mapping  = mapping,
                                                          etavar = {{x}},
                                                          covvar = dplyr::all_of(covvar),
                                                          drop_fixed = drop_fixed,
                                                          linsm = linsm,
                                                          type     = type,
                                                          title    = title,
                                                          subtitle = subtitle,
                                                          caption  = caption,
                                                          tag      = tag,
                                                          log      = log,
                                                          guide    = guide,
                                                          facets=facets,
                                                          .problem=.problem,
                                                          quiet=quiet,
                                                          ...)
    ))
  }
  # Only reached with a single eta (backwards-compatible path) or with
  # `list = FALSE` and more than one -- the latter combines onto one
  # shared, eta-faceted plot instead of recursing per eta (see #82).
  combine <- length(eta_col) > 1

  if (linsm) {
    smooth_method ="lm"
  } else {
    smooth_method = xpdb$xp_theme$smooth_method
  }

  # Set cov factor to label and units, if relevant
  post_processing_cov <- apply_labels_units(xpdb = xpdb, .problem = .problem)

  # Eta label consistency
  if (xpose::software(xpdb) == 'nonmem') {
    eta_col_old <- eta_col
    eta_col_new <- stringr::str_replace(eta_col_old, "^ET(A?)(\\d+)$", "ETA(\\2)")
    post_processing_eta <-  function(x) {
      x %>%
        dplyr::rename(!!!rlang::set_names(eta_col_old, eta_col_new))
    }
    eta_col <- eta_col_new
  } else {
    post_processing_eta <- function(x) x
  }

  # For `combine`, the eta column(s) are pivoted long *after*
  # post_processing_cov()/post_processing_eta() run on the (still
  # eta-wide) covariate-tidied data -- so the covariate side is
  # unaffected by how many etas end up sharing the plot.
  if (combine) {
    post_processing <- function(x) {
      post_processing_cov(x) %>%
        post_processing_eta() %>%
        tidyr::pivot_longer(cols = dplyr::all_of(eta_col), names_to = "eta_name", values_to = "eta_value")
    }
    plot_facets <- xpose::add_facet_var(facets = facets, variable = "eta_name")
    y_ref <- "eta_value"
  } else {
    post_processing <- function(x) post_processing_eta(post_processing_cov(x))
    plot_facets <- facets
    y_ref <- eta_col
  }

  opt <- xpose::data_opt(.problem = .problem,
                         filter = xpose::only_distinct(xpdb, .problem, facets, quiet),
                         tidy = TRUE, value_col = cov_col, post_processing = post_processing)
  vars <- xpose::aes_c(aes(
    x = .data[["value"]],
    y = .data[[y_ref]]), mapping)

  # Cap combined plots at 9 panels/page (3x3), relying on xpose's own
  # facet_wrap_paginate-based pagination (see print.xpose_plot()) rather
  # than any additional plot-combining machinery/dependency.
  dots <- rlang::list2(...)
  if (combine) dots <- utils::modifyList(rlang::list2(ncol = 3, nrow = 3), dots)

  rlang::exec(
    xpose::xplot_scatter,
    xpdb = xpdb,
    quiet = quiet,
    opt = opt,
    mapping = vars,
    type = type,
    guide = guide,
    facets = plot_facets,
    xscale = xpose::check_scales('x', log),
    yscale = xpose::check_scales('y', NULL),
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    smooth_method = smooth_method,
    guide_slope=0,
    !!!dots)
}

#' Eta categorical covariate plots (typical)
#'
#' @param xpdb <`xp_xtras> or  <`xpose_data`> object
#' @param mapping `ggplot2` style mapping
#' @param etavar `tidyselect` for `eta` variables
#' @param covvar `tidyselect` for categorical covariate variables; `NULL`
#' (default) selects every categorical covariate in the `xpdb` data index.
#' @param drop_fixed As in `xpose`
#' @param orientation Passed to `xplot_boxplot`
#' @param show_n Add "N=" to plot
#' @param type Passed to `xplot_boxplot`
#' @param list <`logical`> Only relevant when `etavar` resolves to more
#' than one eta. If `TRUE` (default, for backwards compatibility), returns
#' a plain list of one plot per eta. If `FALSE`, all etas are instead
#' combined onto one shared plot -- faceted by eta, in addition to the
#' existing per-covariate facet -- automatically paginating (at most 9
#' panels per page, i.e. `ncol`/`nrow` of 3) via `xpose`'s own
#' `facet_wrap_paginate` mechanism. Printing the returned plot renders
#' every page; pass `page` to `print()` to select a specific one.
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param facets Additional facets
#' @param .problem Problem number
#' @param quiet Silence output
#' @param ... Any additional aesthetics.
#'
#' @export
#'
#' @returns The desired plot, or (when `etavar` resolves to more than one
#' eta and `list = TRUE`) a plain list of one plot per eta.
#'
#' @details
#' The ability to show number per covariate level is inspired
#' by the package `pmplots`, but is implements here within
#' the `xpose` ecosystem for consistency.
#'
#'
#' @examples
#' \donttest{
#'
#' eta_vs_catcov(xpdb_x)
#'
#' # Labels and units are also supported
#' xpdb_x %>%
#'   xpose::set_var_labels(AGE="Age", MED1 = "Digoxin") %>%
#'   xpose::set_var_units(AGE="yrs") %>%
#'   set_var_levels(SEX=lvl_sex(), MED1 = lvl_bin()) %>%
#'   eta_vs_catcov()
#'
#' # Combine all etas onto one shared, faceted plot instead of a list
#' eta_vs_catcov(xpdb_x, list = FALSE)
#'
#' # Restrict to specific covariates with covvar, just like etavar
#' eta_vs_catcov(xpdb_x, covvar = SEX)
#' }
eta_vs_catcov <- function(xpdb,
                          mapping  = NULL,
                          etavar = NULL,
                          covvar = NULL,
                          drop_fixed = TRUE,
                          orientation = "x",
                          show_n = check_xpdb_x(xpdb, .warn=FALSE),
                          type     = 'bol',
                          list     = TRUE,
                          title    = 'Eta versus categorical covariates | @run',
                          subtitle = 'Based on @nind individuals, Eta shrink: @etashk',
                          caption  = '@dir',
                          tag      = NULL,
                          facets,
                          .problem,
                          quiet,
                          ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  checkmate::assert_flag(list)

  if (missing(facets)) facets <- xpose::add_facet_var(facets = xpdb$xp_theme$facets,
                                                      variable = 'variable')

  # Get eta col(s)
  .etavar_quo <- rlang::enquo(etavar)
  eta_col <- resolve_var_cols(
    xpdb, .problem, type = 'eta', varsel = .etavar_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = 'etavar', label = 'eta'
  )
  etavar <- eta_col # overwrite the raw promise -- see resolve_var_cols() note

  # Get cov col(s)
  .covvar_quo <- rlang::enquo(covvar)
  cov_col <- resolve_var_cols(
    xpdb, .problem, type = 'catcov', varsel = .covvar_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = 'covvar', label = 'categorical covariate'
  )
  covvar <- cov_col # overwrite the raw promise -- see resolve_var_cols() note

  if (length(eta_col)>1 && list) {
    return(purrr::map(eta_col, function(x) eta_vs_catcov(xpdb=xpdb,
                                                         mapping  = mapping,
                                                         etavar = {{x}},
                                                         covvar = dplyr::all_of(covvar),
                                                         drop_fixed = drop_fixed,
                                                         orientation = orientation,
                                                         show_n = show_n,
                                                         type     = type,
                                                         title    = title,
                                                         subtitle = subtitle,
                                                         caption  = caption,
                                                         tag      = tag,
                                                         facets=facets,
                                                         .problem=.problem,
                                                         quiet=quiet,
                                                         ...)
    ))
  }
  # Only reached with a single eta (backwards-compatible path) or with
  # `list = FALSE` and more than one -- the latter combines onto one
  # shared, eta-faceted plot instead of recursing per eta (see #82).
  combine <- length(eta_col) > 1

  # Set cov factor to label and units, if relevant -- N= counts (if any)
  # are computed here, on covariate-tidied data where the eta column(s)
  # are still wide/un-pivoted, so they aren't inflated by how many etas
  # end up sharing the plot (see the `combine` pivot below).
  if (!check_xpdb_x(xpdb, .warn=FALSE)) {
    post_processing_cov <- apply_labels_units(xpdb = xpdb, .problem = .problem)
    if (show_n && !quiet) cli::cli_inform("Cannot show N unless xpdb is converted to a cross-compatible xp_xtras object. `as_xpdb_x()` should do this.")
  } else {
    post_processing_cov <- apply_labels_units_levels(xpdb = xpdb, .problem = .problem, show_n = show_n)
  }

  # Eta label consistency
  if (xpose::software(xpdb) == 'nonmem') {
    eta_col_old <- eta_col
    eta_col_new <- stringr::str_replace(eta_col_old, "^ET(A?)(\\d+)$", "ETA(\\2)")
    post_processing_eta <-  function(x) {
      x %>%
        dplyr::rename(!!!rlang::set_names(eta_col_old, eta_col_new))
    }
    eta_col <- eta_col_new
  } else {
    post_processing_eta <- function(x) x
  }

  if (combine) {
    post_processing <- function(x) {
      post_processing_cov(x) %>%
        post_processing_eta() %>%
        tidyr::pivot_longer(cols = dplyr::all_of(eta_col), names_to = "eta_name", values_to = "eta_value")
    }
    plot_facets <- xpose::add_facet_var(facets = facets, variable = "eta_name")
    y_ref <- "eta_value"
  } else {
    post_processing <- function(x) post_processing_eta(post_processing_cov(x))
    plot_facets <- facets
    y_ref <- eta_col
  }

  opt <- xpose::data_opt(.problem = .problem,
                         filter = xpose::only_distinct(xpdb, .problem, facets, quiet),
                         tidy = TRUE, value_col = cov_col, post_processing = post_processing)

  if (orientation=="x") {
    vars <- xpose::aes_c(aes(
      x = .data[["value"]],
      y = .data[[y_ref]]), mapping)
    xscale = "discrete"
    yscale = xpose::check_scales('y', NULL)
  } else {
    vars <- xpose::aes_c(aes(
      y = .data[["value"]],
      x = .data[[y_ref]]), mapping)
    yscale = "discrete"
    xscale = xpose::check_scales('x', NULL)
  }

  really_quiet <- function(x) x
  if (quiet) really_quiet <- function(x) suppressWarnings(x) # <- trivial reshape warning silenced

  # Cap combined plots at 9 panels/page (3x3), relying on xpose's own
  # facet_wrap_paginate-based pagination (see print.xpose_plot()) rather
  # than any additional plot-combining machinery/dependency.
  dots <- rlang::list2(...)
  if (combine) dots <- utils::modifyList(rlang::list2(ncol = 3, nrow = 3), dots)

  really_quiet(rlang::exec(
    xplot_boxplot,
    xpdb = xpdb,
    quiet = quiet,
    opt = opt,
    mapping = vars,
    type = type,
    facets = plot_facets,
    xscale = xscale,
    yscale = yscale,
    orientation = orientation,
    title = title,
    subtitle = subtitle, caption = caption,
    tag = tag,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    !!!dots))
}

######
# Shrinkage contribution plots
######

#' Shrinkage contribution grid plots
#'
#' @description
#' These mirror [`eta_grid()`]/[`eta_vs_cov_grid()`], but for the
#' per-individual shrinkage contribution diagnostic (`shk` type columns,
#' see [`derive_shk()`]/[`backfill_shk()`]) instead of the etas
#' themselves.
#'
#' @rdname shk_grid_plots
#'
#' @param xpdb <`xp_xtras`> or <`xpose_data`> object
#' @param mapping `ggplot2` style mapping
#' @param shkvar `tidyselect` for `shk` variables
#' @param cols `tidyselect` for covariates variables
#' @param covvar For `shk_vs_cov_grid` only: an alias for `cols` (matching
#' the `covvar` argument of [`shk_vs_contcov()`]/[`shk_vs_catcov()`]). If
#' supplied (non-`NULL`), takes precedence over `cols`.
#' @param covtypes Subset to specific covariate type?
#' @param show_n Count the number of `ID`s in each category
#' @param drop_fixed As in `xpose`
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param shkcov For `shk_vs_cov_grid`, `shk` are sorted after covariates
#' to give an `x` orientation to covariate relationships.
#' @param pairs_opts List of arguments to pass to `_opts`. See <[`xplot_pairs`]>
#' @param .problem Problem number
#' @param quiet Silence extra debugging output
#' @param ... Passed to `xplot_pairs`
#'
#' @return `xp_tras_plot` object
#' @export
#'
#' @examples
#' \donttest{
#'
#' xpdb_shk <- backfill_shk(xpdb_x)
#' shk_grid(xpdb_shk)
#' shk_vs_cov_grid(xpdb_shk)
#' }
shk_grid <- function(xpdb,
                     mapping  = NULL,
                     shkvar = NULL,
                     drop_fixed = TRUE,
                     title    = 'Shrinkage contribution correlations | @run',
                     subtitle = 'Based on @nind individuals',
                     caption  = '@dir',
                     tag      = NULL,
                     pairs_opts,
                     .problem,
                     quiet,
                     ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet

  .shkvar_quo <- rlang::enquo(shkvar)
  shk_col <- resolve_var_cols(
    xpdb, .problem, type = 'shk', varsel = .shkvar_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = 'shkvar', label = 'shrinkage contribution'
  )
  shkvar <- shk_col # overwrite the raw promise -- see resolve_var_cols() note

  opt <- xpose::data_opt(.problem = .problem,
                         filter = xpose::only_distinct(xpdb, .problem, NULL, quiet),
                         post_processing = function(x) dplyr::select(x, !!shk_col))

  if (missing(pairs_opts)) pairs_opts <- list()
  pairs_opts_ <- pairs_opts_defaults(pairs_opts)

  xplot_pairs(
    xpdb,
    mapping   = mapping,
    cont_opts = pairs_opts_$cont_opts,
    dist_opts = pairs_opts_$dist_opts,
    cat_opts = pairs_opts_$cat_opts,
    contcont_opts = pairs_opts_$contcont_opts,
    catcont_opts = pairs_opts_$catcont_opts,
    catcat_opts = pairs_opts_$catcat_opts,
    title     = title,
    subtitle  = subtitle,
    caption   = caption,
    tag       = tag,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    opt=opt,
    quiet=quiet,
    ...
  )
}

#' @rdname shk_grid_plots
#' @export
shk_vs_cov_grid <- function(xpdb,
                            mapping  = NULL,
                            shkvar = NULL,
                            cols = NULL,
                            covvar = NULL,
                            covtypes = c("cont","cat"),
                            show_n = TRUE,
                            drop_fixed = TRUE,
                            title    = 'Shrinkage contribution covariate correlations | @run',
                            subtitle = 'Based on @nind individuals',
                            caption  = '@dir',
                            tag      = NULL,
                            shkcov = TRUE,
                            pairs_opts,
                            .problem,
                            quiet,
                            ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet

  .shkvar_quo <- rlang::enquo(shkvar)
  shk_col <- resolve_var_cols(
    xpdb, .problem, type = 'shk', varsel = .shkvar_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = 'shkvar', label = 'shrinkage contribution'
  )
  shkvar <- shk_col # overwrite the raw promise -- see resolve_var_cols() note

  valid_covtypes <- eval(formals()$covtypes)
  if (is.null(covtypes) || any(!covtypes %in% valid_covtypes)) {
    cli::cli_abort("Invalid `covtype`(s): {setdiff(covtypes, valid_covtypes)}")
  }
  get_govs <- paste0(covtypes, "cov")
  .cols_quo <- rlang::enquo(cols)
  .covvar_quo <- rlang::enquo(covvar)
  # `covvar` is an alias for `cols` (see #82) -- takes precedence when supplied
  cols_arg_name <- 'cols'
  if (!rlang::quo_is_null(.covvar_quo)) {
    .cols_quo <- .covvar_quo
    cols_arg_name <- 'covvar'
  }
  cov_col <- resolve_var_cols(
    xpdb, .problem, type = get_govs, varsel = .cols_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = cols_arg_name, label = paste0("(", paste(covtypes, collapse = ", "), ") covariate")
  )
  cols <- cov_col # overwrite the raw promise -- see resolve_var_cols() note
  covvar <- cov_col # overwrite the raw promise -- see resolve_var_cols() note

  # Set cov factor to label and units, if relevant
  lvld_cov <- cov_col[cov_col %in% xp_var(xpdb, .problem, type = "catcov", silent = TRUE)$col]
  if (!check_xpdb_x(xpdb, .warn=FALSE)) {
    post_processing_cov <- apply_lul_wide(xpdb = xpdb, cols=cov_col, lvl_cols=lvld_cov, .problem = .problem)
    if (show_n && !quiet) cli::cli_inform("Cannot show N unless xpdb is converted to a cross-compatible xp_xtras object. `as_xpdb_x()` should do this.")
  } else {
    post_processing_cov <- apply_lul_wide(xpdb = xpdb, cols=cov_col, lvl_cols=lvld_cov, .problem = .problem, show_n = show_n)
  }

  post_processing <- function(x) {
    orig_names <- names(x)
    proc_df <- post_processing_cov(x)
    new_names <- names(proc_df)
    # only return processed data
    return_names <- new_names[match(cov_col, orig_names)]

    if (shkcov) {
      list_first <- return_names
      list_second <- shk_col
    } else {
      list_first <- shk_col
      list_second <- return_names
    }

    dplyr::select(proc_df, !!list_first, !!list_second)
  }

  opt <- xpose::data_opt(.problem = .problem,
                         filter = xpose::only_distinct(xpdb, .problem, NULL, quiet),
                         post_processing = post_processing)

  if (missing(pairs_opts)) pairs_opts <- list()
  pairs_opts_ <- pairs_opts_defaults(pairs_opts)

  xplot_pairs(
    xpdb,
    mapping   = mapping,
    cont_opts = pairs_opts_$cont_opts,
    dist_opts = pairs_opts_$dist_opts,
    cat_opts = pairs_opts_$cat_opts,
    contcont_opts = pairs_opts_$contcont_opts,
    catcont_opts = pairs_opts_$catcont_opts,
    catcat_opts = pairs_opts_$catcat_opts,
    title     = title,
    subtitle  = subtitle,
    caption   = caption,
    tag       = tag,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    opt=opt,
    quiet=quiet,
    ...
  )
}

#' Shrinkage contribution versus continuous covariates
#'
#' @description
#' Mirrors [`eta_vs_contcov()`], but for the per-individual shrinkage
#' contribution diagnostic (`shk` type columns, see
#' [`derive_shk()`]/[`backfill_shk()`]) instead of the etas themselves.
#'
#' @param xpdb <`xp_xtras`> or <`xpose_data`> object
#' @param mapping `ggplot2` style mapping
#' @param shkvar `tidyselect` for `shk` variables
#' @param covvar `tidyselect` for continuous covariate variables; `NULL`
#' (default) selects every continuous covariate in the `xpdb` data index.
#' @param drop_fixed As in `xpose`
#' @param linsm If `type` contains "s" should the smooth method by `lm`?
#' @param type Passed to `xplot_scatter`
#' @param list <`logical`> Only relevant when `shkvar` resolves to more
#' than one `shk` column. If `TRUE` (default, for backwards compatibility),
#' returns a plain list of one plot per `shk` column. If `FALSE`, they are
#' instead combined onto one shared plot -- faceted by `shk` column, in
#' addition to the existing per-covariate facet -- automatically
#' paginating (at most 9 panels per page, i.e. `ncol`/`nrow` of 3) via
#' `xpose`'s own `facet_wrap_paginate` mechanism. Printing the returned
#' plot renders every page; pass `page` to `print()` to select a specific
#' one.
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param log Log scale covariate value?
#' @param guide Add guide line?
#' @param facets Additional facets
#' @param .problem Problem number
#' @param quiet Silence output
#' @param ... Any additional aesthetics.
#'
#' @export
#' @returns The desired plot, or (when `shkvar` resolves to more than one
#' `shk` column and `list = TRUE`) a plain list of one plot per column.
#'
#' @examples
#' \donttest{
#'
#' xpdb_x %>%
#'   backfill_shk() %>%
#'   shk_vs_contcov()
#'
#' # Combine all shk columns onto one shared, faceted plot instead of a list
#' xpdb_x %>%
#'   backfill_shk() %>%
#'   shk_vs_contcov(list = FALSE)
#' }
shk_vs_contcov <- function(xpdb,
                           mapping  = NULL,
                           shkvar = NULL,
                           covvar = NULL,
                           drop_fixed = TRUE,
                           linsm = FALSE,
                           type     = 'ps',
                           list     = TRUE,
                           title    = 'Shrinkage contribution versus continuous covariates | @run',
                           subtitle = 'Based on @nind individuals',
                           caption  = '@dir',
                           tag      = NULL,
                           log      = NULL,
                           guide    = TRUE,
                           facets,
                           .problem,
                           quiet,
                           ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  checkmate::assert_flag(list)

  if (missing(facets)) facets <- xpose::add_facet_var(facets = xpdb$xp_theme$facets,
                                                      variable = 'variable')

  .shkvar_quo <- rlang::enquo(shkvar)
  shk_col <- resolve_var_cols(
    xpdb, .problem, type = 'shk', varsel = .shkvar_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = 'shkvar', label = 'shrinkage contribution'
  )
  shkvar <- shk_col # overwrite the raw promise -- see resolve_var_cols() note

  # Get cov col(s)
  .covvar_quo <- rlang::enquo(covvar)
  cov_col <- resolve_var_cols(
    xpdb, .problem, type = 'contcov', varsel = .covvar_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = 'covvar', label = 'continuous covariate'
  )
  covvar <- cov_col # overwrite the raw promise -- see resolve_var_cols() note

  if (length(shk_col)>1 && list) {
    return(purrr::map(shk_col, function(x) shk_vs_contcov(xpdb = xpdb,
                                                          mapping  = mapping,
                                                          shkvar = {{x}},
                                                          covvar = dplyr::all_of(covvar),
                                                          drop_fixed = drop_fixed,
                                                          linsm = linsm,
                                                          type     = type,
                                                          title    = title,
                                                          subtitle = subtitle,
                                                          caption  = caption,
                                                          tag      = tag,
                                                          log      = log,
                                                          guide    = guide,
                                                          facets=facets,
                                                          .problem=.problem,
                                                          quiet=quiet,
                                                          ...)
    ))
  }
  # Only reached with a single shk column (backwards-compatible path) or
  # with `list = FALSE` and more than one -- the latter combines onto one
  # shared, shk-faceted plot instead of recursing per column (see #82).
  combine <- length(shk_col) > 1

  if (linsm) {
    smooth_method <- "lm"
  } else {
    smooth_method <- xpdb$xp_theme$smooth_method
  }

  # Set cov factor to label and units, if relevant
  post_processing_cov <- apply_labels_units(xpdb = xpdb, .problem = .problem)

  if (combine) {
    post_processing <- function(x) {
      post_processing_cov(x) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(shk_col), names_to = "shk_name", values_to = "shk_value")
    }
    plot_facets <- xpose::add_facet_var(facets = facets, variable = "shk_name")
    y_ref <- "shk_value"
  } else {
    post_processing <- post_processing_cov
    plot_facets <- facets
    y_ref <- shk_col
  }

  opt <- xpose::data_opt(.problem = .problem,
                         filter = xpose::only_distinct(xpdb, .problem, facets, quiet),
                         tidy = TRUE, value_col = cov_col, post_processing = post_processing)
  vars <- xpose::aes_c(aes(
    x = .data[["value"]],
    y = .data[[y_ref]]), mapping)

  # Cap combined plots at 9 panels/page (3x3), relying on xpose's own
  # facet_wrap_paginate-based pagination (see print.xpose_plot()) rather
  # than any additional plot-combining machinery/dependency.
  dots <- rlang::list2(...)
  if (combine) dots <- utils::modifyList(rlang::list2(ncol = 3, nrow = 3), dots)

  rlang::exec(
    xpose::xplot_scatter,
    xpdb = xpdb,
    quiet = quiet,
    opt = opt,
    mapping = vars,
    type = type,
    guide = guide,
    facets = plot_facets,
    xscale = xpose::check_scales('x', log),
    yscale = xpose::check_scales('y', NULL),
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    smooth_method = smooth_method,
    guide_slope=0,
    !!!dots)
}

#' Shrinkage contribution versus categorical covariates
#'
#' @description
#' Mirrors [`eta_vs_catcov()`], but for the per-individual shrinkage
#' contribution diagnostic (`shk` type columns, see
#' [`derive_shk()`]/[`backfill_shk()`]) instead of the etas themselves.
#'
#' @param xpdb <`xp_xtras`> or <`xpose_data`> object
#' @param mapping `ggplot2` style mapping
#' @param shkvar `tidyselect` for `shk` variables
#' @param covvar `tidyselect` for categorical covariate variables; `NULL`
#' (default) selects every categorical covariate in the `xpdb` data index.
#' @param drop_fixed As in `xpose`
#' @param orientation Passed to `xplot_boxplot`
#' @param show_n Add "N=" to plot
#' @param type Passed to `xplot_boxplot`
#' @param list <`logical`> Only relevant when `shkvar` resolves to more
#' than one `shk` column. If `TRUE` (default, for backwards compatibility),
#' returns a plain list of one plot per `shk` column. If `FALSE`, they are
#' instead combined onto one shared plot -- faceted by `shk` column, in
#' addition to the existing per-covariate facet -- automatically
#' paginating (at most 9 panels per page, i.e. `ncol`/`nrow` of 3) via
#' `xpose`'s own `facet_wrap_paginate` mechanism. Printing the returned
#' plot renders every page; pass `page` to `print()` to select a specific
#' one.
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param facets Additional facets
#' @param .problem Problem number
#' @param quiet Silence output
#' @param ... Any additional aesthetics.
#'
#' @export
#'
#' @returns The desired plot, or (when `shkvar` resolves to more than one
#' `shk` column and `list = TRUE`) a plain list of one plot per column.
#'
#' @examples
#' \donttest{
#'
#' xpdb_x %>%
#'   backfill_shk() %>%
#'   shk_vs_catcov()
#'
#' # Combine all shk columns onto one shared, faceted plot instead of a list
#' xpdb_x %>%
#'   backfill_shk() %>%
#'   shk_vs_catcov(list = FALSE)
#' }
shk_vs_catcov <- function(xpdb,
                          mapping  = NULL,
                          shkvar = NULL,
                          covvar = NULL,
                          drop_fixed = TRUE,
                          orientation = "x",
                          show_n = check_xpdb_x(xpdb, .warn=FALSE),
                          type     = 'bol',
                          list     = TRUE,
                          title    = 'Shrinkage contribution versus categorical covariates | @run',
                          subtitle = 'Based on @nind individuals',
                          caption  = '@dir',
                          tag      = NULL,
                          facets,
                          .problem,
                          quiet,
                          ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  checkmate::assert_flag(list)

  if (missing(facets)) facets <- xpose::add_facet_var(facets = xpdb$xp_theme$facets,
                                                      variable = 'variable')

  .shkvar_quo <- rlang::enquo(shkvar)
  shk_col <- resolve_var_cols(
    xpdb, .problem, type = 'shk', varsel = .shkvar_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = 'shkvar', label = 'shrinkage contribution'
  )
  shkvar <- shk_col # overwrite the raw promise -- see resolve_var_cols() note

  # Get cov col(s)
  .covvar_quo <- rlang::enquo(covvar)
  cov_col <- resolve_var_cols(
    xpdb, .problem, type = 'catcov', varsel = .covvar_quo,
    drop_fixed = drop_fixed, quiet = quiet,
    arg_name = 'covvar', label = 'categorical covariate'
  )
  covvar <- cov_col # overwrite the raw promise -- see resolve_var_cols() note

  if (length(shk_col)>1 && list) {
    return(purrr::map(shk_col, function(x) shk_vs_catcov(xpdb=xpdb,
                                                         mapping  = mapping,
                                                         shkvar = {{x}},
                                                         covvar = dplyr::all_of(covvar),
                                                         drop_fixed = drop_fixed,
                                                         orientation = orientation,
                                                         show_n = show_n,
                                                         type     = type,
                                                         title    = title,
                                                         subtitle = subtitle,
                                                         caption  = caption,
                                                         tag      = tag,
                                                         facets=facets,
                                                         .problem=.problem,
                                                         quiet=quiet,
                                                         ...)
    ))
  }
  # Only reached with a single shk column (backwards-compatible path) or
  # with `list = FALSE` and more than one -- the latter combines onto one
  # shared, shk-faceted plot instead of recursing per column (see #82).
  combine <- length(shk_col) > 1

  # Set cov factor to label and units, if relevant -- N= counts (if any)
  # are computed here, on covariate-tidied data where the shk column(s)
  # are still wide/un-pivoted, so they aren't inflated by how many shk
  # columns end up sharing the plot (see the `combine` pivot below).
  if (!check_xpdb_x(xpdb, .warn=FALSE)) {
    post_processing_cov <- apply_labels_units(xpdb = xpdb, .problem = .problem)
    if (show_n && !quiet) cli::cli_inform("Cannot show N unless xpdb is converted to a cross-compatible xp_xtras object. `as_xpdb_x()` should do this.")
  } else {
    post_processing_cov <- apply_labels_units_levels(xpdb = xpdb, .problem = .problem, show_n = show_n)
  }

  if (combine) {
    post_processing <- function(x) {
      post_processing_cov(x) %>%
        tidyr::pivot_longer(cols = dplyr::all_of(shk_col), names_to = "shk_name", values_to = "shk_value")
    }
    plot_facets <- xpose::add_facet_var(facets = facets, variable = "shk_name")
    y_ref <- "shk_value"
  } else {
    post_processing <- post_processing_cov
    plot_facets <- facets
    y_ref <- shk_col
  }

  opt <- xpose::data_opt(.problem = .problem,
                         filter = xpose::only_distinct(xpdb, .problem, facets, quiet),
                         tidy = TRUE, value_col = cov_col, post_processing = post_processing)

  if (orientation=="x") {
    vars <- xpose::aes_c(aes(
      x = .data[["value"]],
      y = .data[[y_ref]]), mapping)
    xscale = "discrete"
    yscale = xpose::check_scales('y', NULL)
  } else {
    vars <- xpose::aes_c(aes(
      y = .data[["value"]],
      x = .data[[y_ref]]), mapping)
    yscale = "discrete"
    xscale = xpose::check_scales('x', NULL)
  }

  really_quiet <- function(x) x
  if (quiet) really_quiet <- function(x) suppressWarnings(x) # <- trivial reshape warning silenced

  # Cap combined plots at 9 panels/page (3x3), relying on xpose's own
  # facet_wrap_paginate-based pagination (see print.xpose_plot()) rather
  # than any additional plot-combining machinery/dependency.
  dots <- rlang::list2(...)
  if (combine) dots <- utils::modifyList(rlang::list2(ncol = 3, nrow = 3), dots)

  really_quiet(rlang::exec(
    xplot_boxplot,
    xpdb = xpdb,
    quiet = quiet,
    opt = opt,
    mapping = vars,
    type = type,
    facets = plot_facets,
    xscale = xscale,
    yscale = yscale,
    orientation = orientation,
    title = title,
    subtitle = subtitle, caption = caption,
    tag = tag,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    !!!dots))
}

#' Covariate effect forest plot
#'
#' @description
#' Visualizes the effect of covariates on structural parameters, as
#' declared with [`add_cov_association()`], as a forest plot: one row per
#' (parameter, covariate, evaluation point), the point estimate and
#' interval as a ratio to the parameter's typical value, with a reference
#' line at `1`.
#'
#' This is the covariate-specific wrapper: it calls [`prm_cov()`] to
#' compute the effect-size table and [`xplot_forest()`] (a generic,
#' forest-plot-agnostic renderer, see its own documentation) to draw it.
#'
#' @param xpdb <`xp_xtras`> object with covariate associations declared
#' via [`add_cov_association()`]
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Forwarded to
#' [`prm_cov()`] -- eg `param ~ covariate` selectors, `ci_method`,
#' `probs`, `level`, `nsim`.
#' @param type Passed to [`xplot_forest()`]; defaults to `'pilr'` (point +
#' interval + reference line + shaded reference region -- `xplot_forest()`'s
#' own defaults omit the line and region, since those are `cov_forest()`-
#' specific opinions, not generic ones).
#' Including `"v"` adds a violin/density layer of the raw simulation draws
#' behind each interval; this forces `prm_cov(keep_draws = TRUE)`, which
#' in turn requires `ci_method = "simulation"` (the default) -- pass
#' `ci_method = "delta"` in `...` together with `type` containing `"v"`
#' and it will error, since no draws exist for the delta method.
#' @param region <`numeric(2)`> `c(low, high)` bounds for the shaded
#' reference region (`type` includes `"r"`, the default); `NULL`
#' (default) falls back to `c(0.8, 1.25)`, a common bioequivalence-style
#' "no relevant effect" band.
#' @param show_ref <`logical`> Include the reference row(s) (`effect`/
#' `ci_low`/`ci_high` always `1`, by construction, for every reference
#' covariate value/level)? Defaults to `TRUE`; set `FALSE` to drop them
#' from the plot -- they carry no information beyond what the reference
#' line already shows, and cutting them can reduce clutter when there are
#' many covariates.
#' @param log <`logical`> Log-scale the effect-ratio (x) axis? Defaults to
#' `TRUE`. Unlike most of the package's `log` arguments (eg
#' [`eta_vs_contcov()`]'s), this is a plain boolean rather than an
#' `"x"`/`"y"`/`NULL` axis-selector string -- `cov_forest()`'s orientation
#' isn't user-configurable, so the axis being logged is never ambiguous.
#' @param forest_opts <`list`> Extra named arguments forwarded to
#' [`xplot_forest()`] (eg theme overrides), the same way `pairs_opts`
#' works for [`cov_grid()`]/[`eta_grid()`]. Rarely needed since the most
#' common override, `type`, is already its own argument.
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param .problem <`numeric`> Problem number
#' @param .subprob <`numeric`> Subprob number
#' @param .method <`numeric`> Method
#' @param quiet Silence extra output
#'
#' @export
#'
#' @returns The desired plot
#'
#' @seealso [`add_cov_association()`], [`prm_cov()`], [`xplot_forest()`]
#'
#' @examples
#' \donttest{
#'
#' xpdb_x %>%
#'   add_cov_association(
#'     TVCL ~ power(CLCR, THETA7, ref = 64),
#'     TVCL ~ catshift(SEX, THETA4, ref = 1)
#'   ) %>%
#'   cov_forest()
#' }
cov_forest <- function(xpdb,
                       ...,
                       type     = 'pilr',
                       region   = NULL,
                       show_ref = TRUE,
                       log = TRUE,
                       forest_opts = list(),
                       title    = 'Covariate effects on model parameters | @run',
                       subtitle = 'Ratio to typical parameter value; reference line at 1',
                       caption  = '@dir',
                       tag      = NULL,
                       .problem = NULL,
                       .subprob = NULL,
                       .method  = NULL,
                       quiet) {
  xpose::check_xpdb(xpdb, check = 'data')
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (is.null(.problem)) .problem <- xpose::default_plot_problem(xpdb)

  needs_violin <- stringr::str_detect(type, stringr::fixed('v', ignore_case = TRUE))

  cov_tbl <- prm_cov(xpdb, ..., .problem=.problem, .subprob=.subprob, .method=.method,
                     keep_draws = needs_violin, quiet=quiet)
  if (nrow(cov_tbl)==0) {
    rlang::abort("No covariate associations found to plot. Declare some with `add_cov_association()` first.")
  }

  if (!show_ref) cov_tbl <- dplyr::filter(cov_tbl, !is_ref)
  if (nrow(cov_tbl)==0) {
    rlang::abort("No rows left to plot after `show_ref = FALSE` removed all reference rows (every continuous covariate only had a reference point, or every categorical covariate only had its reference level).")
  }

  plot_data <- cov_tbl %>%
    dplyr::mutate(
      row_label = paste0(covariate, ": ", level),
      row_label = forcats::fct_inorder(row_label) %>% forcats::fct_rev()
    )

  facets <- xpose::add_facet_var(facets = xpdb$xp_theme$facets, variable = 'param')

  vars <- ggplot2::aes(
    x = .data[["effect"]],
    y = .data[["row_label"]],
    xmin = .data[["ci_low"]],
    xmax = .data[["ci_high"]]
  )

  opt <- xpose::data_opt(.problem = .problem, post_processing = function(x) plot_data)

  violin_opt <- NULL
  if (needs_violin) {
    # One row per draw, not one row per category -- a different shape than
    # `plot_data`, so it's its own data_opt() rather than reusing `opt`
    draws_long <- plot_data %>%
      dplyr::select(row_label, draws) %>%
      tidyr::unnest(draws)
    violin_opt <- xpose::data_opt(.problem = .problem, post_processing = function(x) draws_long)
    vars <- xpose::aes_c(vars, ggplot2::aes(
      violin_x = .data[["draws"]],
      violin_y = .data[["row_label"]]
    ))
  }

  forest_args <- utils::modifyList(
    list(
      xpdb = xpdb,
      mapping = vars,
      type = type,
      region = region,
      opt = opt,
      violin_opt = violin_opt,
      facets = facets,
      xscale = if (isTRUE(log)) "log10" else "continuous",
      vline_xintercept = 1,
      title = title,
      subtitle = subtitle,
      caption = caption,
      tag = tag,
      plot_name = 'cov_forest',
      quiet = quiet
    ),
    forest_opts
  )

  do.call(xplot_forest, forest_args) +
    ggplot2::labs(x = "Effect ratio (relative to typical parameter value)", y = NULL)
}
