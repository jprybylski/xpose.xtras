#######
# Categorical dv plots
#######

# Pearson residuals can already be treated as res in res plots
# Just need to add documentation in that regard

# sum of n in cat for each cat faceted
#catdv_vs_idv <- function() {}
# Distribution of res faceted over each category
#res_vs_catdv <- function() {}


#######
# Categorical pred plots
#######

# Plots where likelihood of values is x axis and
# violins of those values are on Y. For binary,
# this forms a nice logistic-like smooth curve.
# For mulitple levels, need to facet by levels,
# So you have prob at level 0, prob at greater,
# and numb at zero number at other (and same
# for higher cutpoints)

#' Non-simulation based likelihood model diagnostic
#'
#' @description
#' These plots attempt to provide a means of verifying that the
#' estimated likelihoods and probabilities for categorical outcomes
#' are captured within the model.
#'
#' When the smooth spline is included (`type` includes `"s"`), it is
#' expected that the overall trend is up and to the right; a relatively
#' flat trend suggests that the modeled likelihood is inconsistent with the
#' observed outcome.
#'
#'
#' @inheritParams xplot_boxplot
#' @param cutpoint <`numeric`> Of defined probabilities, which one to
#' use in plots.
#' @param xlab Either use the typical basic x-axis label (the cutpoint-defined
#' column name) or label it based on the probability/likelihood it is estimating.
#' @param facets Additional facets
#' @param .problem Problem number
#'
#' @export
#' @inherit xplot_boxplot details
#'
#' @returns The desired plot
#'
#' @examples
#' # Test M3 model
#' pkpd_m3 %>%
#'   # Need to ensure var types are set
#'   set_var_types(catdv=BLQ,dvprobs=LIKE) %>%
#'   # Set probs
#'   set_dv_probs(1, 1~LIKE, .dv_var = BLQ) %>%
#'   # Optional, but useful to set levels
#'   set_var_levels(1, BLQ = lvl_bin()) %>%
#'   # Plot with basic xlab makes no assumptions
#'   catdv_vs_dvprobs(xlab = "basic")
#'
#' # Test categorical model
#' vismo_xpdb <- vismo_pomod  %>%
#'   set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
#'   set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23)
#'
#' # Various cutpoints (note axes labels and texts)
#' vismo_xpdb %>%
#'   catdv_vs_dvprobs(xlab = "basic")
#' vismo_xpdb %>%
#'   catdv_vs_dvprobs(cutpoint = 2, xlab = "basic")
#' vismo_xpdb %>%
#'   catdv_vs_dvprobs(cutpoint = 3, xlab = "basic")
#'
#' # Latter is arguably clearer with default xlab
#' vismo_xpdb %>%
#'   catdv_vs_dvprobs(cutpoint = 3)
#'
catdv_vs_dvprobs <- function(xpdb,
                             mapping  = NULL,
                             cutpoint = 1,
                             type     = 'vbs',
                             title    = '@y vs. @x | @run',
                             subtitle = 'Ofv: @ofv, Number of individuals: @nind',
                             caption  = '@dir',
                             tag      = NULL,
                             xlab = c("probability","basic"),
                             facets,
                             .problem,
                             quiet,
                             ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet))   quiet <- xpdb$options$quiet
  if (missing(facets))  facets <- xpdb$xp_theme$facets
  xlab <- rlang::arg_match(xlab, c("basic", "probability"))

  # Extra processing

  # Get relevant columns (or throw error)
  dvprob_cols <- xp_var(xpdb, .problem, type = 'dvprobs')$col
  catdv_cols <- xp_var(xpdb, .problem, type = 'catdv')$col
  if (length(catdv_cols)>1) {
    cli::cli_warn("Only one categorical DV will be used ({catdv_cols[1]}).")
    catdv_cols <- catdv_cols[1]
  }

  cp <- make_catdv_cutpoint(xpdb, .problem, catdv_cols, cutpoint)

  xplot_boxplot(
    xpdb = xpdb, group = NULL, quiet = quiet,
    opt = xpose::data_opt(
      .problem = .problem,
      filter = xpose::only_obs(xpdb, .problem, quiet),
      post_processing = cp$post_process
      ),
    mapping = xpose::aes_c(aes(x = .data[[cp$prob_col]],
                        y = .data[[catdv_cols]],
                        smooth_group = NA,
                        smooth_method = "loess"),
                        mapping),
    type = type, facets = facets,
    xscale = "continuous",
    yscale = "discrete",
    orientation = "y",
    title = title, subtitle = subtitle, caption = caption,
    tag = tag, plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    ...
  ) +
    ggplot2::labs(
      x = ifelse(
        xlab=="basic",
        cp$prob_col,
        sprintf("Probability %s %s", catdv_cols, cp$cp_label)
      )
    )
}

#' Binned calibration plot for categorical DVs
#'
#' @description
#' A binned alternative to [catdv_vs_dvprobs()]. The probability column
#' associated with `cutpoint` is split into `bins` equally-sized groups,
#' from lowest to highest predicted probability, and for each bin the
#' observed proportion of the categorical DV meeting the cutpoint
#' condition is calculated (i.e. the m/M observations in that bin with
#' the target value).
#'
#' For a well-specified model, the mean predicted probability of a bin
#' should be close to the bin's observed proportion, so plotted points
#' are expected to fall around the unity (`y = x`) line.
#'
#' @inheritParams catdv_vs_dvprobs
#' @param bins <`numeric`> Number of (roughly) equally-sized bins used to
#' group the probability column, from lowest to highest.
#' @param type String setting the type of plot to be used: line `l`,
#' point `p`, smooth `s` and text `t`, or any combination thereof. See
#' [xpose::xplot_scatter()].
#' @param guide Include the unity (`y = x`) guide line?
#'
#' @export
#'
#' @returns The desired plot
#'
#' @seealso [catdv_vs_dvprobs()]
#'
#' @examples
#' # Test M3 model
#' pkpd_m3 %>%
#'   # Need to ensure var types are set
#'   set_var_types(catdv=BLQ,dvprobs=LIKE) %>%
#'   # Set probs
#'   set_dv_probs(1, 1~LIKE, .dv_var = BLQ) %>%
#'   # Optional, but useful to set levels
#'   set_var_levels(1, BLQ = lvl_bin()) %>%
#'   # Plot with 5 bins
#'   catdv_vs_ipred(bins = 5)
#'
#' # Test categorical model
#' vismo_xpdb <- vismo_pomod  %>%
#'   set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
#'   set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23)
#'
#' # Various cutpoints and bin counts
#' vismo_xpdb %>%
#'   catdv_vs_ipred(bins = 8, xlab = "basic")
#' vismo_xpdb %>%
#'   catdv_vs_ipred(cutpoint = 2, bins = 8, xlab = "basic")
#' vismo_xpdb %>%
#'   catdv_vs_ipred(cutpoint = 3, bins = 8, xlab = "basic")
#'
catdv_vs_ipred <- function(xpdb,
                            mapping  = NULL,
                            cutpoint = 1,
                            bins     = 10,
                            type     = 'pl',
                            guide    = TRUE,
                            title    = 'Observed frequency vs. predicted probability | @run',
                            subtitle = 'Ofv: @ofv, Number of individuals: @nind',
                            caption  = '@dir',
                            tag      = NULL,
                            xlab = c("probability","basic"),
                            facets,
                            .problem,
                            quiet,
                            ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet))   quiet <- xpdb$options$quiet
  if (missing(facets))  facets <- xpdb$xp_theme$facets
  xlab <- rlang::arg_match(xlab, c("basic", "probability"))
  checkmate::assert_count(bins, positive = TRUE)

  # Get relevant columns (or throw error)
  dvprob_cols <- xp_var(xpdb, .problem, type = 'dvprobs')$col
  catdv_cols <- xp_var(xpdb, .problem, type = 'catdv')$col
  if (length(catdv_cols)>1) {
    cli::cli_warn("Only one categorical DV will be used ({catdv_cols[1]}).")
    catdv_cols <- catdv_cols[1]
  }

  cp <- make_catdv_cutpoint(xpdb, .problem, catdv_cols, cutpoint)

  # Bin/stratify per facet column(s), if supplied as simple column names
  strata <- character(0)
  if (is.character(facets)) strata <- facets

  # Bin the probability column into `bins` (roughly) equally-sized groups
  # (lowest to highest), then summarize observed proportion per bin
  post_processing <- function(df) {
    cp$post_process(df) %>%
      dplyr::mutate(
        !!catdv_cols := as.numeric(.data[[catdv_cols]]) - 1,
        `...bin...`   = dplyr::ntile(.data[[cp$prob_col]], bins)
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(strata, "...bin...")))) %>%
      dplyr::summarise(
        !!cp$prob_col := mean(.data[[cp$prob_col]], na.rm = TRUE),
        !!catdv_cols  := mean(.data[[catdv_cols]], na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data[["...bin..."]]) %>%
      dplyr::mutate(`...group...` = 1L)
  }

  xpose::xplot_scatter(
    xpdb = xpdb, group = "...group...", quiet = quiet,
    opt = xpose::data_opt(
      .problem = .problem,
      filter = xpose::only_obs(xpdb, .problem, quiet),
      post_processing = post_processing
    ),
    mapping = xpose::aes_c(aes(x = .data[[cp$prob_col]],
                        y = .data[[catdv_cols]],
                        size = .data[["n"]]),
                        mapping),
    type = type, guide = guide, facets = facets,
    xscale = "continuous",
    yscale = "continuous",
    title = title, subtitle = subtitle, caption = caption,
    tag = tag, plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    ...
  ) +
    ggplot2::labs(
      x = ifelse(
        xlab=="basic",
        cp$prob_col,
        sprintf("Probability %s %s", catdv_cols, cp$cp_label)
      ),
      y = sprintf("Observed frequency %s %s", catdv_cols, cp$cp_label)
    )
}

#' Longitudinal binned observed vs. predicted plot for categorical DVs
#'
#' @description
#' A longitudinal alternative to [catdv_vs_ipred()] and to `xpose`'s own
#' [xpose::dv_preds_vs_idv()] for categorical outcomes. Rather than binning
#' by predicted probability (as [catdv_vs_ipred()] does) or plotting raw
#' per-subject values against a continuous independent variable, this bins
#' observations by a discrete, typically ordered grouping variable (eg an
#' `occ`-typed occasion column) and plots the observed proportion meeting
#' the cutpoint condition alongside the mean predicted probability, one
#' point/line per bin.
#'
#' @inheritParams catdv_vs_ipred
#' @param bin <`tidyselect`> Column to bin/group by. Defaults to the first
#' `occ`-typed column (see [`set_var_types()`]). If that column has
#' defined levels (see [`set_var_levels()`]), those labels (and their
#' order) are used; otherwise raw values are coerced to a factor as-is.
#' @param type String setting the type of plot to be used: point `p`, line
#' `l`, and smooth `s`, or any combination thereof. See [`xplot_binned()`].
#'
#' @export
#'
#' @returns The desired plot
#'
#' @seealso [catdv_vs_ipred()], [catdv_vs_dvprobs()]
#'
#' @examples
#' # Derive an occasion column (TIME is in hours here) and level it in
#' # visit order
#' vismo_xpdb <- vismo_pomod %>%
#'   set_var_types(.problem = 1, catdv = DV, dvprobs = matches("^P\\d+$")) %>%
#'   set_dv_probs(.problem = 1, 0~P0, 1~P1, ge(2)~P23) %>%
#'   xpose::mutate(OCC = ceiling((TIME + 1) / 24), .problem = 1) %>%
#'   set_var_types(.problem = 1, occ = OCC) %>%
#'   set_var_levels(.problem = 1, OCC = lvl_inord(paste("Day", 1:12)))
#'
#' vismo_xpdb %>%
#'   catdv_vs_occ()
#'
#' vismo_xpdb %>%
#'   catdv_vs_occ(cutpoint = 3)
#'
catdv_vs_occ <- function(xpdb,
                          mapping  = NULL,
                          bin      = NULL,
                          cutpoint = 1,
                          type     = 'pl',
                          title    = 'Observed and predicted probability vs. @x | @run',
                          subtitle = 'Ofv: @ofv, Number of individuals: @nind',
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
  if (missing(quiet))   quiet <- xpdb$options$quiet
  if (missing(facets))  facets <- xpdb$xp_theme$facets

  # Get relevant columns (or throw error)
  catdv_cols <- xp_var(xpdb, .problem, type = 'catdv')$col
  if (length(catdv_cols)>1) {
    cli::cli_warn("Only one categorical DV will be used ({catdv_cols[1]}).")
    catdv_cols <- catdv_cols[1]
  }

  # Resolve the binning/grouping column
  if (rlang::quo_is_null(rlang::enquo(bin))) {
    occ_cols <- xp_var(xpdb, .problem, type = "occ", silent = TRUE)$col
    if (length(occ_cols) == 0) {
      cli::cli_abort(paste(
        "No {.code occ}-typed column found. Set one via",
        "{.code set_var_types(occ = ...)}, or pass {.arg bin} explicitly."
      ))
    }
    if (length(occ_cols) > 1) {
      cli::cli_warn("Only one occasion column will be used ({occ_cols[1]}).")
    }
    bin_col <- occ_cols[1]
  } else {
    bin_col <- xpose::get_data(xpdb, .problem = .problem, quiet = TRUE) %>%
      dplyr::select({{bin}}) %>%
      names()
  }

  cp <- make_catdv_cutpoint(xpdb, .problem, catdv_cols, cutpoint)

  # Level lookup for the binning column, mirroring make_catdv_cutpoint()'s
  # handling of catdv levels; falls back to a plain factor if unleveled
  bin_lvl_tbl <- get_index(xpdb, .problem) %>%
    dplyr::filter(col == bin_col) %>%
    dplyr::pull(levels) %>%
    .[[1]]
  if (is.null(bin_lvl_tbl) || nrow(bin_lvl_tbl) == 0) {
    if (!quiet) cli::cli_alert_info(paste(
      "{bin_col} has no defined levels; using raw values as-is. Consider",
      "{.code set_var_levels()} (eg with {.code lvl_inord()}) for readable,",
      "explicitly ordered labels."
    ))
    bin_lvl_tbl <- NULL
  }

  # Bin/stratify per facet column(s), if supplied as simple column names
  strata <- character(0)
  if (is.character(facets)) strata <- facets

  post_processing <- function(df) {
    cp$post_process(df) %>%
      dplyr::mutate(
        !!catdv_cols := as.numeric(.data[[catdv_cols]]) - 1,
        !!bin_col := val2lvl(.data[[bin_col]], bin_lvl_tbl)
      ) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(strata, bin_col)))) %>%
      dplyr::summarise(
        !!cp$prob_col := mean(.data[[cp$prob_col]], na.rm = TRUE),
        !!catdv_cols  := mean(.data[[catdv_cols]], na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(c(cp$prob_col, catdv_cols)),
        names_to = "variable", values_to = "value"
      ) %>%
      dplyr::mutate(
        variable = dplyr::recode(variable, !!cp$prob_col := "Predicted", !!catdv_cols := "Observed")
      )
  }

  xplot_binned(
    xpdb = xpdb, group = "variable", quiet = quiet,
    opt = xpose::data_opt(
      .problem = .problem,
      filter = xpose::only_obs(xpdb, .problem, quiet),
      post_processing = post_processing
    ),
    mapping = xpose::aes_c(aes(x = .data[[bin_col]],
                        y = .data[["value"]]),
                        mapping),
    type = type, facets = facets,
    xscale = "discrete",
    yscale = "continuous",
    title = title, subtitle = subtitle, caption = caption,
    tag = tag, plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    ...
  ) +
    # Observed reuses the theme's standard data-line colour; Predicted
    # reuses its model/regression colour -- consistent with how existing
    # multi-series xpose colouring is theme-driven (eg xset_shark's
    # sharkup_color/sharkdn_color) rather than an arbitrary ggplot default
    ggplot2::scale_colour_manual(values = c(
      Observed  = xpdb$xp_theme$line_color,
      Predicted = xpdb$xp_theme$smooth_color
    )) +
    ggplot2::labs(
      x = bin_col,
      y = sprintf("Frequency/probability %s %s", catdv_cols, cp$cp_label),
      colour = NULL
    )
}

make_catdv_cutpoint <- function(xpdb, .problem, catdv_col, cutpoint) {
  # pull levels & probs from the xpdb index
  idx   <- get_index(xpdb, .problem)
  levels_df <- idx %>% dplyr::filter(col == catdv_col) %>% dplyr::pull(levels) %>% .[[1]]
  probs_df  <- idx %>% dplyr::filter(col == catdv_col) %>% dplyr::pull(probs)  %>% .[[1]]
  if (nrow(probs_df)==0) {
    rlang::abort("Relationship between probabiliy column and at least one categorical DV level should be defined.")
  }
  if (!cutpoint %in% 1:nrow(probs_df)) {
    cli::cli_abort("cutpoint is the row number of established probability formulae.
                   There are {nrow(probs_df)} rows available, so cutpoint {cutpoint} is out of range.")
  }

  cp_row <- probs_df[cutpoint,]
  # find human‐readable level label if any
  val_lab <- cp_row$value
  if (nrow(levels_df) && cp_row$value %in% levels_df$value) {
    val_lab <- levels_df$level[match(cp_row$value, levels_df$value)][1]
  }

  # build EQ/GE/... labels
  qual  <- cp_row$qual
  verb  <- if (is.na(qual)) "EQ" else toupper(qual)
  cp_label  <- sprintf("%s(%s)", verb, val_lab)
  # opposite
  opp <- switch(
    qual,
    ne = "EQ", ge = "LT", gt = "LE", le = "GT", lt = "GE",
    "NE"
  )
  cp_olabel <- sprintf("%s(%s)", opp, val_lab)

  # comparator function
  cmp_fun <- switch(
    qual,
    ne = `!=`, ge = `>=`, gt = `>`, le = `<=`, lt = `<`,
    `==`
  )

  # the post‐processing closure
  post_fn <- function(df) {
    df %>%
      mutate(
        !!catdv_col := ifelse(
          cmp_fun(.data[[catdv_col]], cp_row$value),
          1L, 0L
        ) %>%
          forcats::as_factor() %>%
          forcats::fct_inseq() %>%
          forcats::fct_relabel(~ifelse(.x=="1", cp_label, cp_olabel))
      )
  }

  list(
    prob_col     = cp_row$prob,
    cut_val      = cp_row$value,
    cp_label     = cp_label,
    cp_olabel    = cp_olabel,
    post_process = post_fn
  )
}


#' Set probability columns for categorical endpoints
#'
#' @description
#' For categorical DVs or similar endpoints (such as censoring
#' flag columns, like `BLQ`), this function allows probability
#' columns to be defined for each level.
#'
#'
#' @param xpdb <`xp_xtras`> object
#' @param .problem <`numeric`> Problem number to use. Uses all problems if `NULL`
#' (the default). May be omitted entirely and left to default, even when
#' formulas are supplied positionally in `...`.
#' @param ... Formulas where LHS are levels or pseudo-functions (see Details), and RHS
#' are columns with probabilities of those levels.
#' @param .dv_var <`tidyselect`> of column having the categorical observation. Default is first-listed
#' `catdv`.
#' @param .handle_missing <`character`> How to handle missing levels: "quiet", "warn", or "error"
#'
#' @return <`xp_xtras`> object with updated probabilities
#' @export
#'
#' @details
#' The same probability cannot be assigned to multiple values. Pseudo-functions can be used, or
#' new columns can be created to overcome this limitation. The available pseudo-functions should
#' be written like `ge(value)` (for `>=`), `gt(value)` (for `>`), etc. These comparison names
#' are those used in Perl, Fortran and many other languages. The function `eq()` should not be used,
#' but it will be ignored either way; equivalence is implied with the base syntax.
#'
#'
#' @examples
#' pkpd_m3 %>%
#'  # Not necessary, but correct to set var type before using this
#'  set_var_types(.problem=1, catdv=BLQ, dvprobs=LIKE) %>%
#'  # Set var type. Warnings can be helpful unless an inverse likelihood column is available
#'  set_dv_probs(.problem=1, 1~LIKE, .dv_var = BLQ, .handle_missing = "warn") %>%
#'  list_vars()
#'
#' # Same as above with demo of inverse column
#' pkpd_m3 %>%
#'  xpose::mutate(INVLIKE = 1-LIKE) %>%
#'  set_var_types(.problem=1, catdv=BLQ, dvprobs=c(LIKE,INVLIKE)) %>%
#'  # Note no warning
#'  set_dv_probs(.problem=1, 1~LIKE, 0~INVLIKE, .dv_var = BLQ, .handle_missing = "warn")%>%
#'  list_vars()
#'
#' # With categorical model
#' vismo_pomod  %>%
#'  # Update var types
#'  set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
#'  # Warning (as noted), does not recognize 3 is covered implicitly. That's ok!
#'  set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23, .handle_missing = "warn")%>%
#'  list_vars()
#'
#' # Same as above, but...
#' vismo_pomod  %>%
#'  set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
#'  # Default is to not bother users with a warning
#'  set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23)%>%
#'  list_vars()
#'
#' # .problem can be omitted for single-problem models
#' pkpd_m3 %>%
#'  set_var_types(catdv=BLQ, dvprobs=LIKE) %>%
#'  set_dv_probs(1~LIKE, .dv_var = BLQ) %>%
#'  list_vars()
#'
set_dv_probs <- function(
    xpdb,
    .problem = NULL,
    ...,
    .dv_var = NULL, # default is first DV var
    .handle_missing = c("quiet","warn","error")
    ) {
  #### Top part is similar to set_var_levels
  # .problem is positioned before `...` so it can still be supplied
  # positionally (`set_dv_probs(1, 1~LIKE)`), but that means an omitted
  # .problem lets the first unnamed formula bind to it instead. Detect
  # that and shift it back into the formula dots.
  leading_dot <- list()
  if (rlang::is_formula(.problem)) {
    leading_dot <- list(.problem)
    .problem <- NULL
  }

  # Basic check
  if (!check_xpdb_x(xpdb)) rlang::abort("xp_xtras object required.")
  xpose::check_xpdb(xpdb, check = "data")
  xp_d <- xpdb$data
  if (!is.null(.problem) && !.problem %in% xp_d$problem) cli::cli_abort("Problem number { .problem} not valid.")
  # Make sure users did not do `=`
  rlang::try_fetch(
    rlang::check_dots_unnamed(),
    error = function(s)
      rlang::abort(paste("Only formula(e) are expected in the dots, not assignment.",
                         "Was `=` used instead of `~`?"), parent=s)
  )


  # Arg process
  .handle_missing = rlang::arg_match0(arg = .handle_missing, values = c("quiet","warn","error"))

  # Relevant index
  full_index <- get_index(xpdb, .problem=.problem)
  # Relevant data
  full_data <- xpose::get_data(xpdb, .problem=.problem, quiet = TRUE)
  # Set null dvvar
  if (rlang::quo_is_null(rlang::enquo(.dv_var))) {
    # xp_var() needs exactly one problem (unlike get_index()/get_data(),
    # which treat NULL as "all problems"), so fall back to the default
    # plot problem when .problem was not supplied.
    rlang::try_fetch(
      .dv_var <- xp_var(xpdb, .problem = if (is.null(.problem)) xpose::default_plot_problem(xpdb) else .problem, type = "catdv")$col[1],
      error = function(s)
        rlang::abort("No categorical DV in data. Perhaps DV var type should be changed?",
                     parent = s)
    )
  } else {
    .dv_var <- dplyr::select(full_data, {{.dv_var}}) %>%
      names()
  }

  # Consume dots
  prb_list <- c(leading_dot, rlang::dots_list(..., .ignore_empty = "all", .homonyms = "keep"))
  prb_tab <- check_probs(prb_list, full_index, .dv_var)
  # Add all probs

  new_x <- xpdb
  new_index <- full_index

  # make sure lhs are in data
  dv_col <- full_data %>% dplyr::pull(!!.dv_var)
  lev_vals <- prb_tab$value
  if (!all(lev_vals %in% dv_col) && .handle_missing!="quiet") {
    msg_txt <- "The following values are not in { .dv_var}: {setdiff(lev_vals, dv_col )}."
    if (.handle_missing=="warn") cli::cli_warn(paste(msg_txt,"Probability-based plots may look odd."))
    if (.handle_missing=="error") cli::cli_abort(msg_txt)
  }
  if (!all(dv_col %in% lev_vals)) {
    msg_txt <- "{cli::col_cyan(.dv_var)} values are missing in probabilities: {setdiff(.dv_var,lev_vals)}."
    if (.handle_missing=="warn") {
      cli::cli_warn(paste(msg_txt,"If other probabilities don't add up 1, the
                          inverse will apply to uncounted levels."))
      cli::cli_warn("This check does not consider qualifiers that may implicitly include
                     uncounted levels (eg, {.code lt(1)}).")
    }
    if (.handle_missing=="error") cli::cli_abort(msg_txt)
  }

  # put processed levels in the index tibble
  new_index <- new_index %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      probs = ifelse(
        col == .dv_var,
        list(prb_tab),
        list(probs)
      )
    ) %>%
    dplyr::ungroup()
  new_x <- set_index(new_x, new_index)

  new_x
}

check_probs <- function(prb_list, index, dvcol) {
  # Basic check
  if (!is_formula_list(prb_list)) rlang::abort("List of formulas required.")

  # process
  rlang::try_fetch(
    prb_tab <- proc_probs(prb_list),
    error = function(s)
      rlang::abort("Formulas contain invalid syntax. Error below may be helpful.",
                   parent = s)
  )

  # Make sure all names on RHS are in index
  if (!all(prb_tab$prob %in% index$col))
    cli::cli_abort("Probability columns indicated are not in data: {setdiff(prb_tab$prob, index$col)}")

  # Error if one probability for multiple
  if (any(duplicated(prb_tab$prob)))
    cli::cli_abort("Cannot use same probability for multiple endpoints ({unique(prb_tab$prob[duplicated(prb_tab$prob)])}).
                   Consider mutating a new column with the same value or using a comparison pseudo-function (eg {.code ge()}).")

  # Make sure any pseudo functions used are valid
  valid_psefun <- c("lt","le","gt","ge","ne",NA,"eq")
  if ("eq" %in% prb_tab$qual) {
    rlang::warn("Please avoid using the `eq` pseudo-function. Equivalence is implied.")
    # remove eq
    prb_tab$qual[prb_tab$qual=="eq"] <- NA
  }
  if (!all(prb_tab$qual %in% valid_psefun))
    cli::cli_abort("No available method to handle at least one pseudo-function: {setdiff(prb_tab$qual, valid_psefun)}")

  # Warn if probability column is not a dvprob type
  valid_index <- dplyr::filter(index, type %in% "dvprobs")
  if (!all(prb_tab$prob %in% valid_index$col)) {
    cli::cli_warn("Var types not properly assigned as `dvprobs`, but probabilities will still
                  be applied: {setdiff(prb_tab$prob, valid_index$col)}")
  }

  # Warn if dv column is not a catdv type
  valid_dvs <- dplyr::filter(index, type %in% "catdv")
  if (!dvcol %in% valid_dvs$col) {
    cli::cli_warn("Var type for DV not properly assigned as `catdv`,
                  but probabilities will still be applied: {setdiff(dvcol, valid_dvs$col)}")
  }

  prb_tab
}


proc_probs <-  function(prb_list) {
  purrr::map_dfr(
    prb_list,
    ~ {
      # Extract symbols
      lhs <- .x[[2]]
      fun <- NA_character_
      if (inherits(lhs, "call")) {
        fun <- tolower(deparse(lhs[[1]]))
        lhs <-  rlang::call_args(lhs)[[1]]
      }
      rhs <- deparse(.x[[3]])
      # Create a tibble
      tibble::tibble(
        value = lhs,
        qual = fun,
        prob = rhs
      )
    }
  )
}

#' For a categorical DV variable, show associated probabilities
#'
#' @description
#' A convenient quick check for how probabilities are currently
#' assigned, based on [`set_dv_probs`].
#'
#' @inheritParams set_dv_probs
#'
#' @return <`tibble`> of probabilities
#' @export
#'
#' @examples
#'
#' pkpd_m3 %>%
#'   set_dv_probs(1, 1~LIKE, .dv_var = BLQ) %>%
#'   list_dv_probs(.dv_var=BLQ)
#'
list_dv_probs <- function(
  xpdb,
  .problem = NULL,
  .dv_var = NULL
) {
  # Basic check
  if (!check_xpdb_x(xpdb)) rlang::abort("xp_xtras object required.")
  xpose::check_xpdb(xpdb, check = "data")
  xp_d <- xpdb$data
  if (!is.null(.problem) && !.problem %in% xp_d$problem) cli::cli_abort("Problem number { .problem} not valid.")
  if (is.null(.problem)) fill_prob_subprob_method(xpdb)

  # Relevant index
  full_index <- get_index(xpdb, .problem=.problem)
  # Relevant data
  full_data <- xpose::get_data(xpdb, .problem=.problem, quiet = TRUE)
  # Set null dvvar
  if (rlang::quo_is_null(rlang::enquo(.dv_var))) {
    # xp_var() needs exactly one problem (unlike get_index()/get_data(),
    # which treat NULL as "all problems"), so fall back to the default
    # plot problem when .problem was not supplied.
    rlang::try_fetch(
      .dv_var <- xp_var(xpdb, .problem = if (is.null(.problem)) xpose::default_plot_problem(xpdb) else .problem, type = "catdv")$col[1],
      error = function(s)
        rlang::abort("No categorical DV in data. Perhaps DV var type should be changed?",
                     parent = s)
    )
  } else {
    .dv_var <- dplyr::select(full_data, {{.dv_var}}) %>%
      names()
  }

  full_index %>%
    filter(col==.dv_var) %>%
    {dplyr::pull(.,probs)[[1]]}
}
