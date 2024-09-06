#' Wrapper around ggpairs
#'
#' @param xpdb <`xp_xtras> or  <`xpose_data`> object
#' @param mapping `ggplot2` style mapping
#' @param cont_opts List of options to pass to `xplot_scatter`. See Details
#' @param dist_opts List of options to pass to `xplot_distribution`. See Details
#' @param cat_opts List of options to pass to `xplot_boxplot`. See Details
#' @param contcont_opts List of options to pass to `ggally_cors`. See Details
#' @param catcont_opts List of options to pass to `ggally_statistic`. See Details
#' @param catcat_opts A list wit `use_rho` `TRUE` or `FALSE`. If `TRUE` (default),
#' then the Spearman rho is displayed, else the ggpairs default count is used.
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param plot_name Metadata name of plot
#' @param gg_theme As in `xpose`
#' @param xp_theme As in `xpose`
#' @param opt Processing options for fetched data
#' @param quiet Silence extra debugging output
#' @param progress Show a progress bar as the plot is generated?
#' @param switch Passed to `ggpairs`
#' @param ...
#'
#' @description
#' Following the `xpose` design pattern to derive <[`ggpairs`][GGally::ggpairs]> plots.
#'
#' Established `xplot_` are used to generate parts of the grid.
#'
#' @details
#' There is only limited control over the underlying `ggpairs()` call given
#' the need to address abstractions in `GGally` and `xpose`. However, users
#' can modify  key display features. For `scatter`, `distribution` and `boxplots`,
#' the `type` option is directly forwarded to the user. For upper elements of the matrix,
#' users can modify features of the text displayed or supply some other
#' function entirely (`other_fun`).
#'
#' `_opts` lists are consumed with <[`modifyList`][utils::modifyList]> from the default,
#' so there is no need to declare preferences that align with the default if updating
#' a subset.
#'
#'
#' @importFrom GGally ggpairs
#'
#' @return specified pair plot
#' @export
#'
xplot_pairs <- function(
    xpdb,
    mapping   = NULL,
    cont_opts = list(
      group = "ID",
      guide     = FALSE,
      type     = 'ps'
    ),
    dist_opts = list(
      guide     = FALSE,
      type = "hr"
    ),
    cat_opts = list(
      type      = 'bo',
      log = NULL
    ),
    contcont_opts = list(
      other_fun = NULL,
      stars= FALSE,
      digits = reportable_digits(xpdb),
      title = "Pearson Corr"
    ),
    catcont_opts = list(
      other_fun = NULL,
      stars= FALSE,
      digits = reportable_digits(xpdb),
      title = "Spearman rho"
    ),
    catcat_opts = list(
      use_rho = TRUE
    ),
    title     = NULL,
    subtitle  = NULL,
    caption   = NULL,
    tag       = NULL,
    plot_name = 'pairs',
    gg_theme,
    xp_theme,
    opt,
    quiet,
    progress = rlang::is_interactive() && quiet,
    switch = NULL,
    ...
) {
  #### Boilerplate for typical parts
  # Check input
  xpose::check_xpdb(xpdb, check = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet

  # Fetch data
  if (missing(opt)) opt <- xpose::data_opt()
  data <- xpose::fetch_data(xpdb, quiet = quiet, .problem = opt$problem, .subprob = opt$subprob,
                            .method = opt$method, .source = opt$source, simtab = opt$simtab,
                            filter = opt$filter, tidy = opt$tidy, index_col = opt$index_col,
                            value_col = opt$value_col, post_processing = opt$post_processing)
  if (is.null(data) || nrow(data) == 0) {
    rlang::abort('No data available for plotting. Please check the variable mapping and filering options.')
  }

  # Update _opts defauls
  use_upt <- function(x_opt) {
    opt_nm <- deparse(substitute(x_opt))
    if (is.list(x_opt)) modifyList(eval(formals(xplot_pairs)[[opt_nm]]), x_opt) else cli::cli_abort("`{opt_nm}` must be a list.")
  }

  # Check types (allow incomplete list to specify opts)
  cont_opts <- use_upt(cont_opts)
  xpose::check_plot_type(cont_opts$type, allowed = c("l", "p", "s", "t"))
  dist_opts <- use_upt(dist_opts)
  xpose::check_plot_type(dist_opts$type, allowed = c("d", "h", "r"))
  cat_opts <- use_upt(cat_opts)
  xpose::check_plot_type(cat_opts$type, allowed = c('b', "p","v","o","l"))

  # Check other options
  contcont_opts <- use_upt(contcont_opts)
  catcont_opts <- use_upt(catcont_opts)
  catcat_opts <- use_upt(catcat_opts)

  # Assign xp_theme
  if (!missing(xp_theme)) xpdb <- xpose::update_themes(xpdb = xpdb, xp_theme = xp_theme)

  # Update theme of non-xp_xtra object
  if (!is_xp_xtras(xpdb)) xpdb <- xpose::update_themes(xpdb = xpdb, xp_theme = xp_xtra_theme(xpdb$xp_theme))

  # Assign gg_theme
  if (missing(gg_theme)) {
    gg_theme <- xpdb$gg_theme
  } else {
    gg_theme <- xpose::update_themes(xpdb = xpdb, gg_theme = gg_theme)$gg_theme
  }
  if (is.function(gg_theme)) {
    gg_theme <- do.call(gg_theme, args = list())
  }


  #### Wrapped functions for ggpairs
  wrapped_scatter <- function(data = NULL, mapping = NULL) {
    xpose::xplot_scatter(
      xpdb = xpdb,
      mapping = mapping,
      group = cont_opts$group,
      type = cont_opts$type,
      guide = cont_opts$guid,
      opt = opt,
      gg_theme=xpdb$gg_theme,
      xp_theme=xpdb$xp_theme,
      quiet=quiet,
      smooth_formula=y~x
    )
  }
  wrapped_dist <- function(data = NULL, mapping = NULL) {
    xpose::xplot_distrib(
      xpdb = xpdb,
      mapping = mapping,
      group = dist_opts$group,
      type = dist_opts$type,
      guide = dist_opts$guid,
      opt = opt,
      gg_theme=xpdb$gg_theme,
      xp_theme=xpdb$xp_theme,
      quiet=quiet
    )
  }
  wrapped_box <- function(data = NULL, mapping = NULL) {
    orientation = formals(xplot_boxplot)$orientation
    var_x <- rlang::eval_tidy(mapping$x, data)
    var_y <- rlang::eval_tidy(mapping$y, data)
    if (inherits(var_x, "factor") && inherits(var_y, "numeric")) {
      orientation <- "x"
      xscale = "discrete"
      yscale = xpose::check_scales("y", cat_opts$log)
    } else {
      orientation <- "y"
      yscale = "discrete"
      xscale = xpose::check_scales("x", cat_opts$log)
    }

    xplot_boxplot(
      xpdb = xpdb,
      mapping = mapping,
      group = cat_opts$group,
      type = cat_opts$type,
      guide = cat_opts$guid,
      xscale = xscale,
      yscale = yscale,
      orientation = orientation,
      opt = opt,
      gg_theme=xpdb$gg_theme,
      xp_theme=xpdb$xp_theme,
      quiet=quiet
    )
  }
  ## Upper cells
  if (!is.null(contcont_opts$other_fun)) {
    if (!rlang::is_function(contcont_opts$other_fun)) {
      cli::cli_abort("`contcont_opts$otherfun` must be a function compatible with `GGally::ggpairs()`, not a {cli::col_yellow(class(contcont_opts$other_fun))}.")
    }
    xp_cor <- contcont_opts$other_fun
  } else {
    contcont_opts <- within(contcont_opts, rm(other_fun))
    xp_cor <- GGally::wrapp("cor", params = contcont_opts)
  }
  if (!is.null(catcont_opts$other_fun)) {
    if (!rlang::is_function(catcont_opts$other_fun)) {
      cli::cli_abort("`catcont_opts$otherfun` must be a function compatible with `GGally::ggpairs()`, not a {cli::col_yellow(class(catcont_opts$other_fun))}.")
    }
    xp_aov <- catcont_opts$other_fun
  } else {
    catcont_opts <- within(catcont_opts, rm(other_fun))
    rho_fun <-  function(x, y) {
      corObj <- stats::cor.test(as.numeric(y),as.numeric(x), method="spearman", exact = FALSE)
      cor_est <- as.numeric(corObj$estimate)
      cor_txt <- formatC(cor_est, digits = catcont_opts$digits, format = "f")
      if (isTRUE(catcont_opts$stars)) {
        cor_txt <- stringr::str_c(cor_txt, GGally::signif_stars(corObj$p.value))
      }
      cor_txt
    }
    xp_rho <- GGally::wrap("statistic", text_fn = rho_fun, title = catcont_opts$title, sep=":\n")
  }

  if (catcat_opts$use_rho && exists("xp_rho")) {
    catcat_upper <- xp_rho
  } else {
    catcat_upper <- wrap_xp_ggally("count", xp_theme = xpdb$xp_theme)
  }

  if (!"pairs_labeller" %in% names(xpdb$xp_theme)) {
    use_labeller <- xpdb$xp_theme$labeller
  } else {
    use_labeller <- xpdb$xp_theme$pairs_labeller
  }

  xp <-
    ggpairs(
      data,
      diag = list(continuous = wrapped_dist, discrete = wrap_xp_ggally("barDiag", xp_theme = xpdb$xp_theme), na = "naDiag"),
      lower = list(continuous = wrapped_scatter, combo = wrapped_box, discrete = wrap_xp_ggally("facetbar", xp_theme = xpdb$xp_theme), na =
                     "na"),
      upper = list(continuous = xp_cor, combo = xp_rho, discrete = catcat_upper, na = "na"),
      progress = progress,
      labeller = use_labeller,
      switch = switch
    ) +
    xpdb$gg_theme()

  # Add labels
  xp <- xp + ggplot2::labs(title = title, subtitle = subtitle, caption = caption)

  if (utils::packageVersion('ggplot2') >= '3.0.0') {
    xp <- xp + ggplot2::labs(tag = tag)
  }

  # Add metadata to plots
  xp$xpose <- list(fun      = plot_name,
                   summary  = xpdb$summary,
                   problem  = attr(data, 'problem'),
                   subprob  = attr(data, 'subprob'),
                   method   = attr(data, 'method'),
                   quiet    = quiet,
                   xp_theme = xpdb$xp_theme[stringr::str_c(c('title', 'subtitle',
                                                             'caption', 'tag'), '_suffix')])

  # Output the plot
  xpose::as.xpose.plot(xp) %>%
   structure(class=c("xp_xtra_plot", class(.)))
}


#' @export
print.xp_xtra_plot <- function(x, page, ...) {
  if (!inherits(x, "ggmatrix")) return(NextMethod())

  if (xpose::is.xpose.plot(x)) {
    x$title <- xpose::append_suffix(x$xpose, x$title,
                                    "title")
    x$gg$labs$subtitle <- xpose::append_suffix(x$xpose, x$gg$labs$subtitle,
                                       "subtitle")
    x$gg$labs$caption <- xpose::append_suffix(x$xpose, x$gg$labs$caption,
                                      "caption")
    if (utils::packageVersion("ggplot2") >= "3.0.0") {
      x$gg$labs$tag <- xpose::append_suffix(x$xpose, x$gg$labs$tag,
                                    "tag")
    }
    var_map <- x$mapping %>% as.character() %>% stringr::str_remove(pattern = "^~") %>%
      ifelse(stringr::str_detect(., "\\.data\\[\\[\"\\w+\"]]"),
             yes = stringr::str_remove_all(., "(\\.data\\[\\[\")|(\"]])"),
             no = .) %>% purrr::set_names(names(x$mapping))
    tr_vals <- function(xx) {
      if (is.null(xx)) return(xx)
      xx %>%
        {`if`(
          rlang::is_character(.),
          list(.),
          .
        )} %>%
        structure(class="list") %>%
        purrr::map_if(stringr::str_detect(purrr::list_c(.), "@"),
                      .f = xpose::parse_title, xpdb = x$xpose, problem = x$xpose$problem,
                      quiet = x$xpose$quiet, ignore_key = c("page", "lastpage"),
                      extra_key = c("plotfun", "timeplot", names(var_map)),
                      extra_value = c(x$xpose$fun, format(Sys.time(), "%a %b %d %X %Z %Y"),
                                      var_map)) %>%
        structure(class=class(xx))
    }

    x$gg$labs <- x$gg$labs %>% tr_vals()
    x$title <- x$title %>% tr_vals()
  }
  if (!missing(page)) NULL
  x %>% GGally:::print.ggmatrix()
}


