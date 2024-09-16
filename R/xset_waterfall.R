xset_waterfall <- function(
    xpdb_s,
    ...,
    .inorder=FALSE,
    type = "bt",
    .cols = NULL, # facet if more than one
    max_nind = 0.7,
    scale_diff = TRUE,
    title    = NULL,
    subtitle = NULL,
    caption  = NULL,
    tag      = NULL,
    plot_name = 'waterfall',
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

  # Get combined xpdb
  xpdb_f <- franken_xpdb(
    mod1$xpdb,
    mod2$xpdb,
    .cols = {{.cols}},
    problem = .problem,
    prop_transforms = function(xpdb_f, xpdb_list, problem) {
      # Combine dir
      franken_prop(
        xpdb_f = xpdb_f,
        xpdb_list = xpdb_list,
        prop = "dir")
    }
  )

  # Selected column
  if (rlang::quo_is_null(rlang::enquo(.cols))) rlang::abort("Must select at least one column.")
  sel_cols <- dplyr::select(
    xpose::get_data(xpdb_f, .problem = .problem, quiet = TRUE),
    {{.cols}}
  ) %>% names() %>% unique()

  # Data options
  combined_columns <- purrr::map(sel_cols, ~paste0(.x, "_", 1:2)) %>%
    purrr::list_c()

  # Facet processing
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
  # post-processing
  id_var <- xp_var(xpdb_f, .problem = .problem, type = "id", silent = TRUE)$col
  nind_filter <- function(df) {
    if (max_nind<1) return(
      df %>% dplyr::filter(abs(value)>quantile(abs(value), max_nind))
    )
    df %>% dplyr::slice(1:min(floor(max_nind),dplyr::n()))
  }
  post_processing <- function(df) {
    # Given tidy processing, need to summarize from
    # variable: col1_1, col1_2, col2_1, col2_2,...
    # value: .....
    # to
    # variable: col1, col2, ...
    # value: diffs
    # Valid regex to get all col_1s without accidentally grabbing another column with _1
    col1_regex <- paste(sel_cols,collapse="|") %>%
      paste0("^(",.,")_1$")

    if (any(is.na(df$value)))
      cli::cli_warn(
        "NA values in columns {.bold {df$variable(is.na(df$value))}} will be removed."
      )

    df %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(
        new_variable = stringr::str_extract(variable, "(^.*)_\\d+$", group=1)
      ) %>%
      dplyr::group_by(new_variable, across(dplyr::any_of(facets))) %>%
      dplyr::mutate(
        # scale values (relative to model 1) or keep as is
        value_weight = sd(value[grepl(col1_regex, variable)]),
        value = ifelse(
          scale_diff==TRUE & value_weight>0, # TODO: document that this silent check occurs
          (value - mean(value[grepl(col1_regex, variable)]))/value_weight,
           value
        )
      ) %>%
      dplyr::ungroup() %>%
      # Calculate difference
      dplyr::group_by(new_variable, across(dplyr::any_of(id_var))) %>%
      dplyr::mutate(
        variable = new_variable,
        value = diff(value)
      ) %>%
      dplyr::distinct(variable, .keep_all=TRUE) %>%
      dplyr::ungroup() %>%
      # Filter number
      dplyr::arrange(-abs(value)) %>%
      dplyr::group_by(variable, across(dplyr::any_of(facets))) %>%
      nind_filter() %>% # TODO: determine why this doesn't seem to be filtering right when faceted
      # Ensure waterfall sort
      dplyr::arrange(-value) %>%
      dplyr::ungroup() %>%
      # Apply labels and units
      {apply_labels_units(xpdb_f, .problem = .problem)(.)} %>%
      # Make sure ID stays in order
      dplyr::mutate(
        id_label=paste(.data[[id_var]]),
        id_order=forcats::as_factor(dplyr::row_number()) %>%
          forcats::fct_inorder()
      )
  }
  def_opt <- xpose::data_opt(.problem = .problem, .subprob = .subprob, .method = .method,
                             filter = xpose::only_distinct(xpdb_f, .problem, facets, quiet),
                             tidy = TRUE, value_col = combined_columns,
                             post_processing = post_processing)
  if (missing(opt)) opt <- def_opt
  # Only some data options should be used
  data <- xpose::fetch_data(xpdb_f, quiet = quiet, .problem = def_opt$problem, .subprob = def_opt$subprob,
                            .method = def_opt$method, .source = opt$source, simtab = opt$simtab,
                            filter = def_opt$filter, tidy = def_opt$tidy, index_col = NULL,
                            value_col = def_opt$value_col, post_processing = def_opt$post_processing)
  if (is.null(data) || nrow(data) == 0) {
    rlang::abort('No data available for plotting. Please check the variable mapping and filering options.')
  }

  # Check type
  allow_types <-  c("b", "t")
  xpose::check_plot_type(type, allowed = allow_types)
  check_type <- purrr::map(allow_types, ~stringr::str_detect(type, stringr::fixed(.x, ignore_case = TRUE))) %>%
    setNames(allow_types)

  # Start plot# Assign gg_theme
  gg_theme <- xpdb_f$gg_theme
  if (is.function(gg_theme)) {
    gg_theme <- do.call(gg_theme, args = list())
  }

  # Create ggplot base
  xp <- ggplot2::ggplot(data = data,
                        aes(x = id_order,
                            y = value)
                        ) + gg_theme +
    ggplot2::scale_x_discrete(
      labels = data$id_label,
      breaks = data$id_order
      )

  # Add lines
  if (check_type$b) {
    xp <- xp + xpose::xp_geoms(mapping  = NULL,
                               xp_theme = xpdb_f$xp_theme,
                               name     = "ggabarDiag",
                               ggfun    = "geom_bar",
                               ggabarDiag_stat = "identity"
    )
  }

  # Define panels
  if (!is.null(facets) || length(sel_cols)>1) {
    facet_arg <- "variable"
    ncol_arg <- 1
    nrow_arg <- NULL
    scales_arg <- "free_x"
    if (!is.null(facets)) {
      # facet_arg <- paste(facets,collapse="+") %>%
      #   paste0("variable~",.) %>%
      #   stats::as.formula()
      facet_arg <- c("variable",facets)
      ncol_arg <- NULL
      nrow_arg <- length(sel_cols)
    }
    xp <- xp + xpose::xpose_panels(xp_theme = xpdb_f$xp_theme,
                                   extra_args = list(
                                     facets = facet_arg,
                                     ncol=ncol_arg,
                                     nrow=nrow_arg,
                                     scales=scales_arg
                                     ))
  }

  # Add labels
  xp <- xp + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    y = ifelse(scale_diff, "Scaled change", "Change"),
    x = id_var
  )
  return(xpose::as.xpose.plot(xp))

  if (utils::packageVersion('ggplot2') >= '3.0.0') {
    xp <- xp + ggplot2::labs(tag = tag)
  }

  # Add metadata to plots
  xp$xpose <- list(fun      = plot_name,
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

# There may need to be a waterfall generic: xset_waterfall
