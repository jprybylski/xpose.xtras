######
# Covariate plots
######

### Grid plots

eta_grid <- function() {}
cov_grid <- function() {}
eat_cov_grid <- function() {}

### Non-grid plots

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
  if (!any(eta_col %in% xpose::xp_var(xpdb, .problem, type = 'eta')$col)) {
    cli::cli_abort("`etavar` should only include etas, which does not seem to apply to: {setdiff(eta_col, xpose::xp_var(xpdb, .problem, type = 'eta')$col)}")
  }

  if (length(eta_col)>1) {
    return(purrr::map(eta_col, function(x) eta_vs_contcov(xpdb = xpdb,
                                               mapping  = mapping,
                                               etavar = x,
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
    yscale = xpose::check_scales('y', log),
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    smooth_method = smooth_method,
    guide_slope=0,
    ...)
}

eta_vs_catcov <- function(xpdb,
                          mapping  = NULL,
                          etavar = NULL,
                          drop_fixed = TRUE,
                          orientation = "x",
                          show_n = check_xpdb_x(xpdb, warn=FALSE),
                          type     = 'bol',
                          title    = 'Eta versus categorical covariates | @run',
                          subtitle = 'Based on @nind individuals, Eta shrink: @etashk',
                          caption  = '@dir',
                          tag      = NULL,
                          log      = NULL,
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
                                              etavar = x,
                                              drop_fixed = drop_fixed,
                                              orientation = orientation,
                                              show_n = show_n,
                                              type     = type,
                                              title    = title,
                                              subtitle = subtitle,
                                              caption  = caption,
                                              tag      = tag,
                                              log      = log,
                                              facets=facets,
                                              .problem=.problem,
                                              quiet=quiet,
                                              ...)
    ))
  }

  # Set cov factor to label and units, if relevant
  if (!check_xpdb_x(xpdb, warn=FALSE)) {
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
    yscale = xpose::check_scales('y', log)
  } else {
    vars <- xpose::aes_c(aes(
      y = .data[["value"]],
      x = .data[[eta_col]]), mapping)
    yscale = "discrete"
    xscale = xpose::check_scales('x', log)
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

#######
# Categorical dv plots
#######

# Pearson residuals can already be treated as res in res plots
# Just need to add documentation in that regard

# sum of n in cat for each cat faceted
catdv_vs_idv <- function() {}
# Distribution of res faceted over each category
res_vs_catdv <- function() {}

#####
# Themes
#####

#' @export
xp_xtra_theme <- function(base_on = NULL) {
  if (is.null(base_on)) base_on <- xpose::theme_xp_default()

  # New defaults
  new_defs <- rlang::list2(
    boxplot_fill = "grey45",
    boxplot_linewidth = base_on$density_linewidth,
    boxplot_linetype = base_on$density_linetype,
    boxplot_outlier.colour = base_on$point_color,
    boxplot_outlier.shape = base_on$point_shape,
    violin_fill = base_on$density_fill,
    violin_linewidth = base_on$density_linewidth,
    violin_linetype =  base_on$density_linetype,
    violin_alpha =  base_on$density_alpha,
    dotplot_stackdir = "center",
    dotplot_binpositions = "all",
    dotplot_dotsize = 0.8,
    dotplot_fill = base_on$histogram_fill,
    dotplot_linetype = base_on$area_linetype,
    dotplot_binwidth =NULL,
    hline_color = base_on$guide_color,
    hline_linewidth = base_on$guide_linewidth,
    hline_linetype = base_on$guide_linetype,
    hline_yintercept = 0,
    vline_color = base_on$guide_color,
    vline_linewidth = base_on$guide_linewidth,
    vline_linetype = base_on$guide_linetype,
    vline_xintercept = 0,
    ggabarDiag_fill = base_on$histogram_fill,
    ggabarDiag_color = base_on$histogram_color,
    ggabarDiag_alpha = base_on$histogram_alpha,
    ggabarDiag_linewidth = base_on$histogram_linewidth,
    ggacount_fill = base_on$histogram_fill,
    ggacount_color = base_on$histogram_color,
    ggacount_alpha = base_on$histogram_alpha,
    ggacount_linewidth = base_on$histogram_linewidth,
    ggafacetbar_fill = base_on$histogram_fill,
    ggafacetbar_color = base_on$histogram_color,
    ggafacetbar_alpha = base_on$histogram_alpha,
    ggafacetbar_linewidth = base_on$histogram_linewidth
  )

  # May rarely have these xp_theme elements already defined for an xpose
  # object being based_on, so don't want to overwrite.
  already_covered <- names(new_defs) %in% names(base_on)

  modifyList(
    base_on,
    new_defs[!already_covered],
  ) %>%
    xpose::as.xpose.theme()
}

#' @export
xp4_xtra_theme <- function() xp_xtra_theme(xpose::theme_xp_xpose4())

#########
# Labels and levels
#########

#' @export
apply_labels_units <- function(xpdb, .problem=NULL) {
  function(x) {
    vars <- sort(unique(x$variable))
    xp_var_res <- xp_var(xpdb, .problem=.problem, col = vars) %>%
      dplyr::slice(match(.env$vars,.data$col))
    x  %>%
      dplyr::arrange(variable) %>%
      dplyr::mutate(variable = factor(
        variable,
        levels = .env$vars,
        labels = dplyr::case_when(
          !is.na(xp_var_res$label) & !is.na(xp_var_res$units) ~ sprintf("%s (%s)", xp_var_res$label, xp_var_res$units),
          !is.na(xp_var_res$label) ~ xp_var_res$label,
          !is.na(xp_var_res$units) ~ sprintf("%s (%s)", xp_var_res$col, xp_var_res$units),
          TRUE ~ xp_var_res$col
        )
      ))
  }
}

#' @export
apply_levels <- function(xpdb, .problem=NULL, show_n = TRUE) {
  # xp_xtras class should be checked before this function is called
  function(x) {
    vars <- sort(unique(x$variable))
    xp_var_res <- xp_var(xpdb, .problem=.problem, col = vars) %>%
      dplyr::slice(match(.env$vars,.data$col))
    out <- x  %>%
      dplyr::arrange(variable) %>%
      dplyr::mutate(rn = cumsum(!duplicated(variable))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        this_lvls = xp_var_res$levels[rn],
        value = `if`(nrow(this_lvls)==0, val2lvl(value), val2lvl(value, this_lvls))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(rn, this_lvls))
    if (!show_n) return(out)
    out %>%
      dplyr::group_by(variable, value) %>%
      dplyr::mutate(
        value = paste0(value,"\nN = ", dplyr::n())
      ) %>%
      dplyr::ungroup()
  }
}

apply_labels_units_levels <- function(xpdb, .problem=NULL, show_n = TRUE) {
  lbl_unt_fun <- apply_labels_units(xpdb = xpdb, .problem = .problem)
  lvl_fun <- apply_levels(xpdb = xpdb, .problem = .problem, show_n = show_n)
  function(x) {
    x %>%
      lvl_fun() %>%
      lbl_unt_fun()
  }
}


#########
# Utility functions
#########

#' Grab processed `xpose_plot`
#'
#' @description
#' This function is very simple and unlikely to capture
#' every possible situation. Paginated plots are not supported.
#'
#' This is helpful for working with `xpose` plots in `patchwork` or
#' `ggpubr` functions.
#'
#'
#' @param plot <`xpose_plot`> or list thereof
#'
#' @return Grob or list of grobs
#' @export
#'
#' @examples
#'
#' single_plot <- xpdb_x %>%
#' eta_vs_catcov(etavar = ETA1) %>%
#' grab_xpose_plot()
#'
#' listof_plots <- xpdb_x %>%
#' eta_vs_catcov(etavar = c(ETA1,ETA3)) %>%
#' grab_xpose_plot()
#'
grab_xpose_plot <- function(plot) {
  if (class(plot$facet)[1] %in% c("FacetWrapPaginate", "FacetGridPaginate")) {
    rlang::abort("Use built-in xpose pagination rather than grab function.")
  }
  if (class(plot)[1]=="list") return(purrr::map(plot, grab_xpose_plot))
  pdf(file = NULL)
  out <- xpose:::print.xpose_plot(plot)
  dev.off()
  out
}


#' Ensure consistent style with `GGally` functions
#'
#' @param fn <`character`> name of `GGally` function
#' @param ... <`any`> additional arguments to pass to `GGally` function
#' @param mapping <`ggplot2::aes`> mapping
#' @param xp_theme theme to use
#'
#' @return `ggplot2` function
#' @export
#'
#'
wrap_xp_ggally <- function(fn, xp_theme, ...) {
  assertthat::is.string(fn)
  ggally_fun <- utils::getFromNamespace(paste0("ggally_",fn), "GGally")
  theme_name <- paste0("gga",fn)
  function(data = NULL, mapping = NULL) {
    true_mapping <- mapping
    if (!is.null(mapping))
      mapping <- xpose::parse_arg(mapping, theme_name)
    thm_arg <- xpose::filter_xp_theme(xp_theme, stringr::str_c("^",
                                                               theme_name, "_"))
    arg <- xpose::update_args(thm_arg, theme_name, ...)
    arg$mapping <- true_mapping
    arg$data <- data

    do.call(ggally_fun, arg[!names(arg) %in% names(true_mapping)])
  }
}
