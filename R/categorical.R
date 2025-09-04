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
#' @param .problem <`numeric`> Problem number to use. Uses the all problems if `NULL`
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
set_dv_probs <- function(
    xpdb,
    .problem = NULL,
    ...,
    .dv_var = NULL, # default is first DV var
    .handle_missing = c("quiet","warn","error")
    ) {
  #### Top part is similar to set_var_levels
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
    rlang::try_fetch(
      .dv_var <- xp_var(xpdb, .problem = .problem, type = "catdv")$col[1],
      error = function(s)
        rlang::abort("No categorical DV in data. Perhaps DV var type should be changed?",
                     parent = s)
    )
  } else {
    .dv_var <- dplyr::select(full_data, {{.dv_var}}) %>%
      names()
  }

  # Consume dots
  prb_list <- rlang::dots_list(..., .ignore_empty = "all", .homonyms = "keep")
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
    rlang::try_fetch(
      .dv_var <- xp_var(xpdb, .problem = .problem, type = "catdv")$col[1],
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
