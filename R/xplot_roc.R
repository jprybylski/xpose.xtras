#' Default xpose ROC plot function
#'
#' @param xpdb <`xp_xtras`> or <`xpose_data`> object
#' @param mapping `ggplot2` style mapping
#' @param type See Details.
#' @param guide Should the guide (e.g. unity line) be displayed.
#' @param xscale Defaults to `continuous`.
#' @param yscale Defaults to `continuous`.
#' @param group Grouping for curves or points
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param plot_name Metadata name of plot
#' @param gg_theme As in `xpose`
#' @param xp_theme As in `xpose`
#' @param opt Processing options for fetched data
#' @param quiet Silence extra debugging output
#' @param thres_fixed Fixed threshold value for space
#' @param like_col Column for likelihood/probability value
#' @param obs_col Column for observed value
#' @param obs_target Target observed value for likelihood
#' @param auc_sprintf Format to apply to AUC label
#' @param ... Any additional aesthetics.
#'
#' @description Manually generate ROCs from an xpdb object.
#'
#' @details
#' For type-based customization of plots:
#' \itemize{
#'   \item `c` ROC curve (using `geom_path`)
#'   \item `k` Key points on ROC curve (where on curve the
#'   threshold is `thres_fixed`) (using `geom_point`)
#'   \item `p` ROC space points (using `geom_point`)
#'   \item `t` ROC space text (using `geom_text`)
#'   \item `a` AUC in bottom right (using `geom_label`)
#' }
#'
#' @returns The desired plot
#'
#' @export
xplot_rocplot <- function(xpdb,
                          mapping = NULL,
                          type = "c",
                          guide = TRUE,
                          xscale = "continuous",
                          yscale = "continuous",
                          group = NULL, #' ID',
                          title = NULL,
                          subtitle = NULL,
                          caption = NULL,
                          tag = NULL,
                          plot_name = "xplot_rocplot",
                          gg_theme,
                          xp_theme,
                          opt,
                          quiet,
                          thres_fixed = 0.5,
                          like_col = NULL,
                          obs_col = NULL,
                          obs_target = NULL,
                          auc_sprintf = "AUC: %.3f",
                          ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet

  # Fetch data
  if (missing(opt)) opt <- xpose::data_opt()
  data <- xpose::fetch_data(xpdb,
    quiet = quiet, .problem = opt$problem, .subprob = opt$subprob,
    .method = opt$method, .source = opt$source, simtab = opt$simtab,
    filter = opt$filter, tidy = opt$tidy, index_col = opt$index_col,
    value_col = opt$value_col, post_processing = opt$post_processing
  )
  if (is.null(data) || nrow(data) == 0) {
    rlang::abort("No data available for plotting. Please check the variable mapping and filering options.")
  }

  # Check type
  allow_types <- c("c", "p", "t", "a", "k")
  xpose::check_plot_type(type, allowed = allow_types)
  check_type <- purrr::map(allow_types, ~ stringr::str_detect(type, stringr::fixed(.x, ignore_case = TRUE))) %>%
    setNames(allow_types)


  ### This plot requires data manipulation

  # If `p` or `t`, need grouping variable
  if ((check_type$p || check_type$t) && is.null(group)) {
    cli::cli_abort("For points or text, `group` must be set.")
  }
  points_by <- group

  # If any facets are required, need grouped analysis
  strata <- c()
  if (!is.null(list(...)[["facets"]])) {
    strata <- c(strata, list(...)[["facets"]])
  }

  # Make sure other required info is here
  xpa("number", thres_fixed)
  xpa("string", like_col)
  xpa("string", obs_col)
  xpa("number", obs_target)

  # xcol and ycol
  # Should be FPR and TPR respectively
  # This is a placeholder to leave space to handle dynamically later if the need arises
  # Could have xcol and ycol function args or could pull from mapping
  # If pulled from mapping, the possibity of .data pronouns or other complexity would
  # have to be considered.
  # At this time, it doesn't seem like flexibility for this needs to be implemented.
  xcol <- "FPR"
  ycol <- "TPR"


  # Transform data to include sens and spc
  avoid_conflict <- paste(sample(letters, 5), collapse = "") # make sure can unnest without names conflict
  # Functionalize confmatr generation since this is done for two situations
  add_columns <- function(df, only_fixed = FALSE) {
    df %>%
      # Create nested tibble
      dplyr::mutate(
        confmatr = list(
          confmatr_by_threshold(
            test_vec = .data[[like_col]],
            true_vec = .data[[obs_col]],
            threshold = `if`(
              only_fixed, thres_fixed,
              c(thres_fixed, .data[[like_col]]) %>%
                unique() %>% sort()
            ),
            pos_val = obs_target,
            cols = c(threshold, dplyr::all_of(c(xcol, ycol))),
            prepend = avoid_conflict
          ) %>%
            # Verbose (to account for avoid_conflict) for arrange(FPR,TPR)
            dplyr::arrange(dplyr::across(c(dplyr::ends_with(xcol), dplyr::ends_with(ycol)))) %>%
            # AUC
            dplyr::mutate(
              ...auc... := sum(
                .data[[paste0(avoid_conflict,ycol)]]*diff(
                  c(0,.data[[paste0(avoid_conflict,ycol)]])
                )
              )
            )
        )
      ) %>%
      dplyr::ungroup() %>% # if no grouping this has no effect
      # mutate -> summarize without losing columns
      # group_vars(df) gets grouping of original df even though the piped version has been ungrouped
      dplyr::distinct(dplyr::across(dplyr::all_of(dplyr::group_vars(df))), .keep_all = TRUE) %>%
      # Sensitivity and specificity into main columns
      tidyr::unnest(confmatr) %>%
      # allow threshold to be found without the avoid_conflict part
      dplyr::rename(
        ...threshold... = .data[[paste0(avoid_conflict, "threshold")]]
      )
  }
  # For points
  data_points <- dplyr::slice(data, 0) # empty tibble with correct dimensions
  if (!is.null(points_by)) {
    data_points <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(points_by, strata)))) %>%
      # Just need one threshold for points
      add_columns(TRUE)
  }
  # For curves (and overall plot)
  data <- data %>%
    `if`(
      length(strata) > 0,
      dplyr::group_by(., dplyr::across(dplyr::all_of(strata))),
      .
    ) %>%
    add_columns()

  x_axis <- paste0(avoid_conflict, xcol)
  y_axis <- paste0(avoid_conflict, ycol)
  # Overwrite mapping x and y
  mapping <- xpose::aes_c(
    mapping,
    aes(
      x = .data[[x_axis]],
      y = .data[[y_axis]]
    )
  )
  #### Building plot

  ## Notify user of 0s
  already_warning <- FALSE
  zeros_warning <- "Some sensitivies and specificities not calculable due to 0s."
  if (any(
    is.na(data[[x_axis]]) | is.na(data[[y_axis]])
  )) {
    already_warning <- TRUE
    cli::cli_warn(zeros_warning)
    data <- na.omit(data)
  }
  if (any(
    is.na(data_points[[x_axis]]) | is.na(data_points[[y_axis]])
  )) {
    if (!already_warning) {
      cli::cli_warn(zeros_warning)
    }
    data_points <- na.omit(data_points)
  }


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

  # Create ggplot base
  xp <- ggplot2::ggplot(data = data, xpose::aes_filter(mapping, keep_only = c("x", "y"))) + gg_theme


  # Add curve
  if (check_type$c) {
    xp <- xp + xpose::xp_geoms(
      mapping = mapping,
      xp_theme = xpdb$xp_theme,
      name = "path",
      ggfun = "geom_path",
      ...
    )
  }

  # Add key points
  if (check_type$k) {
    xp <- xp + xpose::xp_geoms(
      mapping = mapping,
      xp_theme = xpdb$xp_theme,
      name = "point",
      ggfun = "geom_point",
      point_data = dplyr::filter(data, ...threshold... == thres_fixed),
      ...
    )
  }


  # Add points
  if (check_type$p) {
    xp <- xp + xpose::xp_geoms(
      mapping = mapping,
      xp_theme = xpdb$xp_theme,
      name = "point",
      ggfun = "geom_point",
      point_data = data_points,
      ...
    )
  }


  # Add text to points
  if (check_type$t) {
    xp <- xp + xpose::xp_geoms(
      mapping = aes(text_label = .data[[group]]),
      xp_theme = xpdb$xp_theme,
      name = "text",
      ggfun = "geom_text",
      text_data = data_points,
      text_position = ggplot2::position_nudge(y = ifelse(check_type$p, -0.05, 0)),
      ...
    )
  }

  # Add AUC label
  if (check_type$a) {
    if (!check_type$c) {
      cli::cli_abort("Need curve to calculate area under curve.")
    }
    xp <- xp + xpose::xp_geoms(
      mapping = aes(label_label = sprintf("AUC: %.3f", .data[["...auc..."]]),
                    label_x = 0.75, label_y = 0.1),
      xp_theme = xpdb$xp_theme,
      name = "label",
      ggfun = "geom_label",
      label_data = dplyr::distinct(data, dplyr::across(dplyr::any_of(strata)), .keep_all = TRUE),
      ...
    )
  }

  # Define scales
  xp <- xp +
    xpose::xp_geoms(
      mapping = mapping,
      xp_theme = xpdb$xp_theme,
      name = "xscale",
      ggfun = paste0("scale_x_", xscale),
      ...
    ) +
    xpose::xp_geoms(
      mapping = mapping,
      xp_theme = xpdb$xp_theme,
      name = "yscale",
      ggfun = paste0("scale_y_", yscale),
      ...
    )

  # Add abline
  if (guide) {
    xp <- xp + xpose::xp_geoms(
      xp_theme = xpdb$xp_theme, name = "guide",
      ggfun = "geom_abline", ...
    )
  }

  # Define panels
  if (!is.null(list(...)[["facets"]])) {
    xp <- xp + xpose::xpose_panels(
      xp_theme = xpdb$xp_theme,
      extra_args = list(...)
    )
  }

  # Add labels
  xp <- xp + ggplot2::labs(title = title, subtitle = subtitle, caption = caption, x = xcol, y = ycol)

  if (utils::packageVersion("ggplot2") >= "3.0.0") {
    xp <- xp + ggplot2::labs(tag = tag)
  }

  # Add metadata to plots
  xp$xpose <- list(
    fun = plot_name,
    summary = xpdb$summary,
    problem = attr(data, "problem"),
    subprob = attr(data, "subprob"),
    method = attr(data, "method"),
    quiet = quiet,
    xp_theme = xpdb$xp_theme[stringr::str_c(c(
      "title", "subtitle",
      "caption", "tag"
    ), "_suffix")]
  )

  # Output the plot
  xpose::as.xpose.plot(xp)
}
