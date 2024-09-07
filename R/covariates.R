######
# Covariate plots
######

#' Grid plots
#'
#' @rdname grid_plots
#'
#' @param xpdb <`xp_xtras> or  <`xpose_data`> object
#' @param mapping `ggplot2` style mapping
#' @param etavar `tidyselect` for `eta` variables
#' @param cols `tidyselect` for covariates variables
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
#'
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
  all_eta_cols <- xpose::xp_var(xpdb, .problem, type = 'eta')$col
  if (rlang::quo_is_null(rlang::enquo(etavar))) {
    etavar <- all_eta_cols
  } else {
    etavar <- dplyr::select(
      xpose::get_data(xpdb, .problem = .problem, quiet = TRUE),
      {{etavar}}
    ) %>% names() %>% unique()
  }

  eta_col <- etavar
  if (drop_fixed) {
    eta_col <- xpose::drop_fixed_cols(xpdb, .problem, cols = eta_col, quiet = quiet)
  }
  if (is.null(eta_col)) {
    rlang::abort('No usable eta column found in the xpdb data index.')
  }
  if (any(!eta_col %in% all_eta_cols)) {
    cli::cli_abort("`etavar` should only include etas, which does not seem to apply to: {setdiff(eta_col, xpose::xp_var(xpdb, .problem, type = 'eta')$col)}")
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
  post_processing <- function(x) {
    post_processing_eta(x) %>%
      dplyr::select(!!eta_col)
  }

  opt <- xpose::data_opt(.problem = .problem,
                         filter = xpose::only_distinct(xpdb, .problem, NULL, quiet),
                         post_processing = post_processing)

  if (missing(pairs_opts)) pairs_opts <- list()
  pairs_opts_ <- formals(xplot_pairs) %>%
    names() %>%
    stringr::str_subset("_opts$") %>%
    rlang::set_names(.,.) %>%
    purrr::map(~{if (.x %in% names(pairs_opts)) pairs_opts[[.x]] else list()})

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
  all_cov_cols <- xp_var(xpdb, .problem, type = get_govs)$col
  if (rlang::quo_is_null(rlang::enquo(cols))) {
    covvar <- all_cov_cols
  } else {
    covvar <- dplyr::select(
      xpose::get_data(xpdb, .problem = .problem, quiet = TRUE),
      {{cols}}
    ) %>% names() %>% unique()
  }

  cov_col <- covvar
  if (drop_fixed) {
    cov_col <- xpose::drop_fixed_cols(xpdb, .problem, cols = cov_col, quiet = quiet)
  }
  if (is.null(cov_col)) {
    rlang::abort('No usable covariate column found in the xpdb data index.')
  }
  if (any(!cov_col %in% all_cov_cols)) {
    cli::cli_abort("`cols` should only include ({covtypes}) covariates, which does not seem to apply to: {setdiff(cov_col, all_cov_cols)}")
  }

  # Set cov factor to label and units, if relevant
  lvld_cov <- cov_col[cov_col %in% xp_var(xpdb, .problem, type = "catcov")$col]
  if (!check_xpdb_x(xpdb, .warn=FALSE)) {
    post_processing_cov <- apply_lul_wide(xpdb = xpdb, cols=cov_col, lvl_cols=lvld_cov, .problem = .problem)
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
  pairs_opts_ <- formals(xplot_pairs) %>%
    names() %>%
    stringr::str_subset("_opts$") %>%
    rlang::set_names(.,.) %>%
    purrr::map(~{if (.x %in% names(pairs_opts)) pairs_opts[[.x]] else list()})

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
  all_eta_cols <- xpose::xp_var(xpdb, .problem, type = 'eta')$col
  if (rlang::quo_is_null(rlang::enquo(etavar))) {
    etavar <- all_eta_cols
  } else {
    etavar <- dplyr::select(
      xpose::get_data(xpdb, .problem = .problem, quiet = TRUE),
      {{etavar}}
    ) %>% names() %>% unique()
  }

  # Get cov col(s)
  valid_covtypes <- eval(formals()$covtypes)
  if (is.null(covtypes) || any(!covtypes %in% valid_covtypes)) {
    cli::cli_abort("Invalid `covtype`(s): {setdiff(covtypes, valid_covtypes)}")
  }
  get_govs <- paste0(covtypes, "cov")
  all_cov_cols <- xp_var(xpdb, .problem, type = get_govs)$col
  if (rlang::quo_is_null(rlang::enquo(cols))) {
    covvar <- all_cov_cols
  } else {
    covvar <- dplyr::select(
      xpose::get_data(xpdb, .problem = .problem, quiet = TRUE),
      {{cols}}
    ) %>% names() %>% unique()
  }

  eta_col <- etavar
  cov_col <- covvar
  if (drop_fixed) {
    eta_col <- xpose::drop_fixed_cols(xpdb, .problem, cols = eta_col, quiet = quiet)
    cov_col <- xpose::drop_fixed_cols(xpdb, .problem, cols = cov_col, quiet = quiet)
  }
  if (is.null(eta_col) || is.null(cov_col)) {
    rlang::abort('No usable eta or covariate column found in the xpdb data index.')
  }
  if (any(!cov_col %in% all_cov_cols)) {
    cli::cli_abort("`cols` should only include ({covtypes}) covariates, which does not seem to apply to: {setdiff(cov_col, all_cov_cols)}")
  }
  if (any(!eta_col %in% all_eta_cols)) {
    cli::cli_abort("`etavar` should only include etas, which does not seem to apply to: {setdiff(eta_col, xpose::xp_var(xpdb, .problem, type = 'eta')$col)}")
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


  # Set cov factor to label and units, if relevant
  lvld_cov <- cov_col[cov_col %in% xp_var(xpdb, .problem, type = "catcov")$col]
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
  pairs_opts_ <- formals(xplot_pairs) %>%
    names() %>%
    stringr::str_subset("_opts$") %>%
    rlang::set_names(.,.) %>%
    purrr::map(~{if (.x %in% names(pairs_opts)) pairs_opts[[.x]] else list()})


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
#' @param drop_fixed As in `xpose`
#' @param linsm If `type` contains "s" should the smooth method by `lm`?
#' @param type Passed to `xplot_scatter`
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param log Log scale covariate value?
#' @param guide Add guide line?
#' @param facets Additional facets
#' @param .problem Problem number
#' @param quiet Silence output
#' @param ...
#'
#' @export
#'
#' @examples
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
eta_vs_contcov <- function(xpdb,
                           mapping  = NULL,
                           etavar = NULL,
                           drop_fixed = TRUE,
                           linsm = FALSE,
                           type     = 'ps',
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

  if (missing(facets)) facets <- xpose::add_facet_var(facets = xpdb$xp_theme$facets,
                                                      variable = 'variable')

  # Get eta col(s)
  if (rlang::quo_is_null(rlang::enquo(etavar))) {
    etavar <- xpose::xp_var(xpdb, .problem, type = 'eta')$col
  } else {
    etavar <- dplyr::select(
      xpose::get_data(xpdb, .problem = .problem, quiet = TRUE),
      {{etavar}}
    ) %>% names() %>% unique()
  }

  eta_col <- etavar
  cov_col <- xpose::xp_var(xpdb, .problem, type = 'contcov')$col
  if (drop_fixed) {
    eta_col <- xpose::drop_fixed_cols(xpdb, .problem, cols = eta_col, quiet = quiet)
    cov_col <- xpose::drop_fixed_cols(xpdb, .problem, cols = cov_col, quiet = quiet)
  }
  if (is.null(eta_col) || is.null(cov_col)) {
    rlang::abort('No usable eta or covariate column found in the xpdb data index.')
  }
  if (any(!eta_col %in% xpose::xp_var(xpdb, .problem, type = 'eta')$col)) {
    cli::cli_abort("`etavar` should only include etas, which does not seem to apply to: {setdiff(eta_col, xpose::xp_var(xpdb, .problem, type = 'eta')$col)}")
  }

  if (length(eta_col)>1) {
    return(purrr::map(eta_col, function(x) eta_vs_contcov(xpdb = xpdb,
                                                          mapping  = mapping,
                                                          etavar = {{x}},
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
    post_processing <-  function(x) {
      post_processing_cov(x) %>%
        dplyr::rename(!!eta_col_new:=!!eta_col_old)
    }
    eta_col <- eta_col_new
  } else {
    post_processing <- post_processing_cov
  }

  opt <- xpose::data_opt(.problem = .problem,
                         filter = xpose::only_distinct(xpdb, .problem, facets, quiet),
                         tidy = TRUE, value_col = cov_col, post_processing = post_processing)
  vars <- xpose::aes_c(aes(
    x = .data[["value"]],
    y = .data[[eta_col]]), mapping)

  xpose::xplot_scatter(
    xpdb = xpdb,
    quiet = quiet,
    opt = opt,
    mapping = vars,
    type = type,
    guide = guide,
    facets = facets,
    xscale = xpose::check_scales('x', log),
    yscale = xpose::check_scales('y', NULL),
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    smooth_method = smooth_method,
    guide_slope=0,
    ...)
}

#' Eta categorical covariate plots (typical)
#'
#' @param xpdb <`xp_xtras> or  <`xpose_data`> object
#' @param mapping `ggplot2` style mapping
#' @param etavar `tidyselect` for `eta` variables
#' @param drop_fixed As in `xpose`
#' @param orientation Passed to `xplot_boxplot`
#' @param show_n Add "N=" to plot
#' @param type Passed to `xplot_boxplot`
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param facets Additional facets
#' @param .problem Problem number
#' @param quiet Silence output
#' @param ...
#'
#' @export
#'
#' @examples
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
eta_vs_catcov <- function(xpdb,
                          mapping  = NULL,
                          etavar = NULL,
                          drop_fixed = TRUE,
                          orientation = "x",
                          show_n = check_xpdb_x(xpdb, .warn=FALSE),
                          type     = 'bol',
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

  if (missing(facets)) facets <- xpose::add_facet_var(facets = xpdb$xp_theme$facets,
                                                      variable = 'variable')

  # Get eta col(s)
  if (rlang::quo_is_null(rlang::enquo(etavar))) {
    etavar <- xpose::xp_var(xpdb, .problem, type = 'eta')$col
  } else {
    etavar <- dplyr::select(
      xpose::get_data(xpdb, .problem = .problem, quiet = TRUE),
      {{etavar}}
    ) %>% names() %>% unique()
  }

  eta_col <- etavar
  cov_col <- xpose::xp_var(xpdb, .problem, type = 'catcov')$col
  if (drop_fixed) {
    eta_col <- xpose::drop_fixed_cols(xpdb, .problem, cols = eta_col, quiet = quiet)
    cov_col <- xpose::drop_fixed_cols(xpdb, .problem, cols = cov_col, quiet = quiet)
  }
  if (is.null(eta_col) || is.null(cov_col)) {
    rlang::abort('No usable eta or covariate column found in the xpdb data index.')
  }
  if (!any(eta_col %in% xpose::xp_var(xpdb, .problem, type = 'eta')$col)) {
    cli::cli_abort("`etavar` should only include etas, which does not seem to apply to {setdiff(eta_col, xpose::xp_var(xpdb, .problem, type = 'eta')$col)}")
  }

  if (length(eta_col)>1) {
    return(purrr::map(eta_col, function(x) eta_vs_catcov(xpdb=xpdb,
                                                         mapping  = mapping,
                                                         etavar = {{x}},
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

  # Set cov factor to label and units, if relevant
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
    post_processing <-  function(x) {
      post_processing_cov(x) %>%
        dplyr::rename(!!eta_col_new:=!!eta_col_old)
    }
    eta_col <- eta_col_new
  } else {
    post_processing <- post_processing_cov
  }

  opt <- xpose::data_opt(.problem = .problem,
                         filter = xpose::only_distinct(xpdb, .problem, facets, quiet),
                         tidy = TRUE, value_col = cov_col, post_processing = post_processing)

  if (orientation=="x") {
    vars <- xpose::aes_c(aes(
      x = .data[["value"]],
      y = .data[[eta_col]]), mapping)
    xscale = "discrete"
    yscale = xpose::check_scales('y', NULL)
  } else {
    vars <- xpose::aes_c(aes(
      y = .data[["value"]],
      x = .data[[eta_col]]), mapping)
    yscale = "discrete"
    xscale = xpose::check_scales('x', NULL)
  }

  really_quiet <- `(`
  if (quiet) really_quiet <- function(x) suppressWarnings(x) # <- trivial reshape warning silenced

  really_quiet(xplot_boxplot(
    xpdb = xpdb,
    quiet = quiet,
    opt = opt,
    mapping = vars,
    type = type,
    facets = facets,
    xscale = xscale,
    yscale = yscale,
    orientation = orientation,
    title = title,
    subtitle = subtitle, caption = caption,
    tag = tag,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    ...))
}
