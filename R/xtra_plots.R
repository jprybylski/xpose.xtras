#' Wrapper around ggpairs
#'
#' @description
#' Following the `xpose` design pattern to derive <[`ggpairs`][GGally::ggpairs]> plots.
#'
#' Established `xplot_` are used to generate parts of the grid.
#'
#'
#' @return specified pair plot
#' @export
#'
#' @examples
#' c()
#'
xplot_pairs <- function(
  xpdb,
  mapping   = NULL,
  group     = 'ID',
  type      = 'pls',
  guide     = FALSE,
  title     = NULL,
  subtitle  = NULL,
  caption   = NULL,
  tag       = NULL,
  gg_theme,
  xp_theme,
  opt,
  quiet,
  ...
  ) {

}

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
  if (is.null(etavar)) etavar <- xpose::xp_var(xpdb, .problem, type = 'eta')$col
  if (!is.null(etavar)) etavar <- dplyr::select(
    xpose::get_data(xpdb, .problem = .problem, quiet = TRUE),
    {{etavar}}
  ) %>% names() %>% unique()

  eta_col <- etavar
  cov_col <- xpose::xp_var(xpdb, .problem, type = 'contcov')$col
  if (drop_fixed) {
    eta_col <- xpose::drop_fixed_cols(xpdb, .problem, cols = eta_col, quiet = quiet)
    cov_col <- xpose::drop_fixed_cols(xpdb, .problem, cols = cov_col, quiet = quiet)
  }
  if (is.null(eta_col) || is.null(cov_col)) {
    rlang::abort('No usable eta or covariate column found in the xpdb data index.')
  }

  if (length(eta_col)>1) {
    return(purrr::map(eta_col, ~eta_vs_contcov(xpdb=xpdb,  mapping  = mapping, etavar = .x, drop_fixed = drop_fixed,
                                        type     = type, title    = title, subtitle = subtitle, caption  = caption,
                                        tag      = tag, log      = log,  guide    = guide, facets = facets,
                                        .problem = .problem, quiet = quiet, ...)
    ))
  }

  # Set cov factor to label and units, if relevant
  post_processing_cov <- NULL

  opt <- xpose::data_opt(.problem = .problem,
                  filter = xpose::only_distinct(xpdb, .problem, facets, quiet),
                  tidy = TRUE, value_col = cov_col, post_processing = post_processing_cov)
  vars <- xpose::aes_c(aes(
    x = .data[["value"]],
    y = .data[[eta_col]]), mapping)

  xpose::xplot_scatter(xpdb = xpdb, quiet = quiet,
                opt = opt,
                mapping = vars,
                type = type, guide = guide, facets = facets,
                xscale = xpose::check_scales('x', log),
                yscale = xpose::check_scales('y', log),
                title = title, subtitle = subtitle, caption = caption,
                tag = tag, plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
                guide_slope = 0, ...)
}

eta_vs_catcov <- function() {}
contcov_vs_catcov <- function() {}

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
    hline_color = base_on$line_color,
    hline_linewidth = base_on$line_linewidth,
    hline_linetype = 2,
    hline_yintercept = 0,
    vline_color = base_on$line_color,
    vline_linewidth = base_on$line_linewidth,
    vline_linetype = 2,
    vline_xintercept = 0,
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
