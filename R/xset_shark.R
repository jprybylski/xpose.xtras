#' Individual contributions to dOFV
#'
#' @description
#' This is intended to match the overall behavior of
#' `dOFV.vs.id()` in `xpose4`, within the framework
#' of the `xpose_set` object.
#'
#' `dofv_vs_id` is an alias of the function `shark_plot`,
#' for recognition.
#'
#'
#' @rdname shark_plot
#' @order 1
#'
#' @param xpdb_s <`xpose_set`> object
#' @param ... See <[`two_set_dots`]>
#' @param .inorder See <[`two_set_dots`]>
#' @param type See Details.
#' @param alpha alpha for LRT
#' @param df degrees of freedom for LRT. If `"guess"` (default), then
#' use the difference in the number of unfixed parameters.
#' @param text_cutoff If less than 1, the percentile of absolute
#' individual dOFV values above which to show labels of IDs.
#' If above 1, the absolute number of IDs to show. To show all,
#' use an extreme positive number like 9999.
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param ylab y-axis label
#' @param xlab x-axis label
#' @param opt User-specified data options. Only some of these
#' will be used.
#' @param facets <`character`> vector selecting facets, or `NULL` (default).
#' @param .problem The problem to be used, by default returns the last one.
#' @param .subprob The subproblem to be used, by default returns the last one.
#' @param .method The estimation method to be used, by default returns the last one.
#' @param quiet Silence extra debugging output
#'
#' @return <`xpose_plot`> object
#'
#' @details
#' For type-based customization of plots:
#' \itemize{
#'   \item `p` points (using aesthetics for `sharkup` and `sharkdn`)
#'   \item `l` lines for dOFV (both total dOFV and significance are plotted)
#'   \item `t` text (using aesthetics for `shkuptxt` and `shkdntxt`)
#' }
#'
#' In `xpose4`, users can control `sig.drop`, but this function uses
#' `alpha` and `df` to determine the critical delta by the likelihood
#' ratio test. It is acknowledged there are situations where this may
#' not be valid, but it is suggested that `df` or `alpha` be adjusted
#' to meet the desired `sig.drop`.
#'
#' ``` r
#' my_alpha <- 0.05
#' my_df <- 1.34 # fractional, perhaps to account for different IIVs
#'
#' my_sigdrop <- -stats::qchisq(1-my_alpha, my_df)
#' my_sigdrop
#' #> [1] -4.633671
#' # Then use alpha=my_alpha, df=my_df in `shark_plot` call.
#' ```
#'
#'
#' @export
#'
#' @seealso [shark_colors()]
#'
#' @examples
#' \donttest{
#'
#' pheno_set %>%
#'   # Make sure set has iofv var types defined
#'   focus_xpdb(everything()) %>%
#'   focus_function(backfill_iofv) %>%
#'   # Pick two models or consistent with two_set_dots()
#'   shark_plot(run6,run11)
#'
#' pheno_set %>%
#'   # As before
#'   focus_xpdb(everything()) %>%
#'   focus_function(backfill_iofv) %>%
#'   # Add indicator (or use established covariate)
#'   mutate(APGRtest = as.numeric(as.character(APGR))<5) %>%
#'   # Pick two models or consistent with two_set_dots()
#'   shark_plot(run6,run11, facets = "APGRtest")
#'
#' }
shark_plot <- function(
    xpdb_s,
    ...,
    .inorder=FALSE,
    type = "plt",
    alpha = 0.05,
    df = "guess",
    text_cutoff = 0.8,
    title    = 'Individual contributions to dOFV | @run',
    subtitle = 'Based on @nind individuals, OFVs: @ofv',
    caption  = '@dir',
    tag      = NULL,
    ylab = "dOFV",
    xlab = "Number of individuals removed",
    opt,
    facets = NULL,
    .problem,
    .subprob,
    .method,
    quiet
) {

  # process dots
  two_set_dots(xpdb_s, ..., .inorder=.inorder)
  # Now have mod1 and mod2

  # Default to the settings for mod1
  if (missing(.problem)) .problem <- xpose::default_plot_problem(mod1$xpdb)
  if (missing(.subprob))
    .subprob <- xpose::last_file_subprob(mod1$xpdb, ext = "ext", .problem = .problem)
  if (missing(.method))
    .method <- xpose::last_file_method(mod1$xpdb, ext = "ext", .problem = .problem,
                                       .subprob = .subprob)
  if (missing(quiet)) quiet <- mod1$xpdb$options$quiet

  # Make sure both models have iofv
  rlang::try_fetch({
    iofv1 <- xp_var(mod1$xpdb, .problem = .problem, type = "iofv")$col
    iofv2 <- xp_var(mod2$xpdb, .problem = .problem, type = "iofv")$col
  },
  error = function(s)
    rlang::abort(paste("Individual OFV is required in data for this function.",
                       "There is no `auto_backfill` option for this function,",
                       "so please see documentation for alternative approaches."), parent=s)
  )
  # Get combined xpdb
  xpdb_f <- franken_xpdb(
    mod1$xpdb,
    mod2$xpdb,
    .types = "iofv",
    problem = .problem,
    prop_transforms = function(xpdb_f, xpdb_list, problem) {
      # Combine OFV
      updated <- franken_prop(
        xpdb_f = xpdb_f,
        xpdb_list = xpdb_list,
        prop = "ofv",
        problem = problem,
        glue_cmd = franken_numprop)
      # Combine dir
      updated <- franken_prop(
        xpdb_f = updated,
        xpdb_list = xpdb_list,
        prop = "dir")
      updated
    }
  )

  # Data options
  ofv_columns <- paste0(c(iofv1,iofv2),"_",1:2)
  # Check if user tries to pass symbol to facets instead of character
  if ((rlang::quo_is_symbol(rlang::enquo(facets)) &&
      !exists(deparse(substitute(facets)), envir = parent.frame())) ||
      (!is.character(facets) &&
       !is.null(facets))) {
    cli::cli_abort("Facets should be a simple character vector.")
  }
  post_processing_cov <- function(x) x
  if (!is.null(facets) && facets %in% xp_var(xpdb_f, .problem = .problem, type = "catcov")$col) {
    # TODO: use apply_lul_wide here for facets, but will have to be sure the new name is captured
  }
  post_processing <- function(df) {
    df %>%
      dplyr::mutate(
        dOFV = .data[[ofv_columns[2]]] - .data[[ofv_columns[1]]]
      ) %>%
      # Ensure sort
      dplyr::arrange(-abs(dOFV)) %>%
      # Sum (maybe faceted) total dOFV
      dplyr::group_by(across(dplyr::any_of(facets))) %>%
      dplyr::group_modify(~{
        dplyr::mutate(.x,
          total_dOFV = sum(dOFV)
        )
      }) %>%
      dplyr::group_by(dOFV>0, .add = TRUE) %>%
      dplyr::mutate(
        nind = 1:dplyr::n(),
        OFV = total_dOFV - cumsum(dOFV)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        nind = ifelse(dOFV==0, 0, nind),
        OFV = ifelse(dOFV==0, total_dOFV, OFV)
      )
  }
  def_opt <- xpose::data_opt(.problem = .problem, .subprob = .subprob, .method = .method,
                             filter = xpose::only_distinct(xpdb_f, .problem, facets, quiet),
                             post_processing = post_processing)
  if (missing(opt)) opt <- def_opt
  # Only some data options should be used
  data <- xpose::fetch_data(xpdb_f, quiet = quiet, .problem = def_opt$problem, .subprob = def_opt$subprob,
                            .method = def_opt$method, .source = opt$source, simtab = opt$simtab,
                            filter = def_opt$filter, tidy = FALSE, index_col = NULL,
                            value_col = NULL, post_processing = def_opt$post_processing)
  if (is.null(data) || nrow(data) == 0) {
    rlang::abort('No data available for plotting. Please check the variable mapping and filering options.')
  }
  ndofv <- data %>% dplyr::filter(dOFV<=0)
  pdofv <- data %>% dplyr::filter(dOFV>=0)
  if (all(data$dOFV==0))
    cli::cli_warn(paste("All individual dOFV values are 0.",
                        "This is unlikely to be an informative plot,",
                        "since these are probably the same model."))

  # Significance of dOFV
  if (df == "guess") {
    prm1 <- xpose::get_prm(mod1$xpdb, .problem = .problem, .subprob = .subprob, .method = .method, quiet = quiet)
    prm2 <- xpose::get_prm(mod2$xpdb, .problem = .problem, .subprob = .subprob, .method = .method, quiet = quiet)

    nfit1 <- sum(!prm1$fixed)
    nfit2 <- sum(!prm2$fixed)

    df <- nfit2 - nfit1
    if (df<=0) {
      cli::cli_warn(paste("Guessing df uses the difference in unfixed parameters.",
                          "For these models, that is {df}. Using a value of 1.",
                          "Adjust `df` and `alpha` to change singificant level."))
      df <- 1
    }
  }
  df <- rlang::try_fetch(as.numeric(df), warning=function(s) {
    rlang::warn("df can only be 'guess' or a number greater than 0. Using a value of 1.")
    1
  })
  if (df<=0) {
    cli::cli_warn(paste("df for LRT must be greater than 0. Using a value of 1."))
    df <- 1
  }
  sigOFV <- -stats::qchisq(1-alpha, df)

  # Check type
  allow_types <-  c("p","l","t")
  xpose::check_plot_type(type, allowed = allow_types)
  check_type <- purrr::map(allow_types, ~stringr::str_detect(type, stringr::fixed(.x, ignore_case = TRUE))) %>%
    setNames(allow_types)

  # Start plot# Assign gg_theme
  gg_theme <- xpdb_f$gg_theme
  if (is.function(gg_theme)) {
    gg_theme <- do.call(gg_theme, args = list())
  }

  # Create ggplot base
  xp <- ggplot2::ggplot(data = data, aes(x = nind, y = OFV)) + gg_theme

  # Add lines
  if (check_type$l) {
    xp <- xp + xpose::xp_geoms(mapping  = aes(hline_yintercept = .data[["total_dOFV"]]),
                               xp_theme = xpdb_f$xp_theme,
                               name     = "hline",
                               ggfun    = "geom_hline"
    )
    xp <- xp + xpose::xp_geoms(mapping  = NULL,
                               xp_theme = xpdb_f$xp_theme,
                               name     = "hline",
                               ggfun    = "geom_hline",
                               hline_yintercept = sigOFV,
                               hline_linetype = 2
    )
  }

  # Add points
  if (check_type$p) {
    xp <- xp + xpose::xp_geoms(mapping  = NULL,
                               xp_theme = xpdb_f$xp_theme,
                               name     = "sharkup",
                               ggfun    = "geom_point",
                               sharkup_data = ndofv
    )
    xp <- xp + xpose::xp_geoms(mapping  = NULL,
                               xp_theme = xpdb_f$xp_theme,
                               name     = "sharkdn",
                               ggfun    = "geom_point",
                               sharkdn_data = pdofv
    )
  }

  # Add text
  if (check_type$t) {
    id_col <- xp_var(xpdb_f, type = "id")$col
    text_filter <- function(df) {
      if (text_cutoff<1) return(
        df %>% dplyr::filter(abs(dOFV)>quantile(abs(dOFV), text_cutoff))
      )
      df %>% dplyr::slice(1:min(floor(text_cutoff),dplyr::n()))
    }
    hjusts <- list(up = 0, dn = 0)
    vjusts <- list(up = 0, dn = 0)
    if (check_type$p) {
      hjusts <- list(up = 1, dn = 1)
      vjusts <- list(up = -0.5, dn = 1)
    }

    xp <- xp + xpose::xp_geoms(mapping  = aes(shkuptxt_label = .data[[id_col]]),
                               xp_theme = xpdb_f$xp_theme,
                               name     = "shkuptxt",
                               ggfun    = "geom_text",
                               shkuptxt_data = text_filter(ndofv),
                               shkuptxt_hjust = hjusts$up,
                               shkuptxt_vjust = vjusts$up
    )
    xp <- xp + xpose::xp_geoms(mapping  = aes(shkdntxt_label = .data[[id_col]]),
                               xp_theme = xpdb_f$xp_theme,
                               name     = "shkdntxt",
                               ggfun    = "geom_text",
                               shkdntxt_data = text_filter(pdofv),
                               shkdntxt_hjust = hjusts$dn,
                               shkdntxt_vjust = vjusts$dn
    )
  }

  # Define panels
  if (!is.null(facets)) {
    xp <- xp + xpose::xpose_panels(xp_theme = xpdb_f$xp_theme,
                                   extra_args = list(facets = facets))
  }

  # Add labels
  xp <- xp + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    y = ylab,
    x = xlab
  )

  if (utils::packageVersion('ggplot2') >= '3.0.0') {
    xp <- xp + ggplot2::labs(tag = tag)
  }

  # Add metadata to plots
  xp$xpose <- list(fun      = "shark_plot",
                   summary  = xpdb_f$summary,
                   problem  = attr(data, 'problem'),
                   subprob  = attr(data, 'subprob'),
                   method   = attr(data, 'method'),
                   quiet    = quiet,
                   xp_theme = xpdb_f$xp_theme[stringr::str_c(c('title', 'subtitle',
                                                               'caption', 'tag'), '_suffix')])

  # Output the plot
  xpose::as.xpose.plot(xp)


}
#' @rdname shark_plot
#' @order 2
#' @export
dofv_vs_id <- function(
    xpdb_s,
    ...,
    .inorder=FALSE,
    type = "plt",
    alpha = 0.05,
    df = "guess",
    text_cutoff = 0.8,
    title    = 'Individual contributions to dOFV | @run',
    subtitle = 'Based on @nind individuals, OFVs: @ofv',
    caption  = '@dir',
    tag      = NULL,
    ylab = "dOFV",
    xlab = "Number of individuals removed",
    opt,
    facets = NULL,
    .problem,
    .subprob,
    .method,
    quiet
) {
  shark_plot(
    xpdb_s=xpdb_s,
    ...,
    .inorder=.inorder,
    type = type,
    alpha = alpha,
    df = df,
    text_cutoff = text_cutoff,
    title    = title,
    subtitle = subtitle,
    caption  = caption,
    tag      = tag,
    ylab = ylab,
    xlab = xlab,
    opt=opt,
    facets = facets,
    .problem=.problem,
    .subprob=.subprob,
    .method=.method,
    quiet=quiet
  )
}
#' Change colors of shark plots
#'
#' @description
#' This changes the point and text color
#' in the `xp_theme` of an `xpose_data` object.
#'
#'
#' @param xpdb <`xpose_data`> object
#' @param upcolor Color for increasing dOFV
#' @param dncolor Color for decreasing dOFV
#'
#' @return <`xpose_data`> object
#' @export
#'
#' @seealso [shark_plot()]
#'
#' @examples
#' \donttest{
#'
#' # Where this would fit in a particular workflow
#' xpose_set(pheno_base, pheno_final) %>%
#'   # forward functions affecting xpdb objects
#'   focus_xpdb(everything()) %>%
#'   # Add iOFVs
#'   focus_function(backfill_iofv) %>%
#'   # Change color of all xpdb xp_themes (though only the first one needs to change)
#'   focus_function(
#'   function(x) shark_colors(
#'       x,
#'       upcolor = "purple",
#'       dncolor = "green"
#'     )) %>%
#'   # See new plot
#'   shark_plot()
#' }
shark_colors <- function(
    xpdb,
    upcolor = xp_xtra_theme(base_on = xpdb$xp_theme)$sharkup_color,
    dncolor = xp_xtra_theme(base_on = xpdb$xp_theme)$sharkdn_color
) {
  new_xp_theme <- xpdb$xp_theme

  new_xp_theme$sharkup_color <- upcolor
  new_xp_theme$shkuptxt_color <- upcolor
  new_xp_theme$sharkdn_color <- dncolor
  new_xp_theme$shkdntxt_color <- dncolor

  xpose::update_themes(
    xpdb,
    xp_theme = xpose::as.xpose.theme(new_xp_theme),
    quiet = xpdb$options$quiet
  )
}
