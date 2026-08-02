#######
# Formal DVID support
#######

# dv_ ipre_ and pred_ plot version with dvid
# dv_vs_idv_dvid <- function(dvid="a number") {}


#####
# Themes
#####

#' Extra theme defaults
#'
#'
#' @description
#' Adds aesthetics for plot components used in this
#' package.
#'
#' @param base_on `xp_theme` object to extend
#'
#' @details
#' This package attempts to generate a consistent
#' theme even if users are working with a highly
#' customized `xp_theme`. There is are only a few
#' hard-coded aesthetics, and the rest are derived from
#' existing aesthetics in `base_on`, which defaults to
#' the default from `xpose`.
#'
#' Only a few options are worth noting. In <[`xplot_pairs`]>
#' (and functions using it), the aesthetics for `GGally`-specific
#' elements like `barDiag` are defined as `gga(element)_(aesthetic)`.
#' The labeller for pairs plots is also changed from the *de facto* default
#' `label_both` to `label_value`, but any labeller can be provided as
#' `pairs_labeller`.
#'
#' @returns An `xpose` theme object
#'
#' @export
xp_xtra_theme <- function(base_on = NULL) {
  if (is.null(base_on)) base_on <- xpose::theme_xp_default()

  # New defaults
  new_defs <- rlang::list2(
    boxplot_fill = base_on$histogram_fill,
    boxplot_alpha = base_on$histogram_alpha,
    boxplot_linewidth = base_on$histogram_linewidth,
    boxplot_linetype = base_on$histogram_linetype,
    boxplot_outlier.colour = base_on$point_color,
    boxplot_outlier.shape = base_on$point_shape,
    boxplot_outlier.alpha = base_on$point_alpha,
    boxplot_outlier.size = base_on$point_size,
    boxplot_outlier.stroke = base_on$point_stroke,
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
    ggafacetbar_linewidth = base_on$histogram_linewidth,
    pairs_labeller = "label_value",
    jitter_colour = base_on$point_color,
    jitter_shape = base_on$point_shape,
    jitter_alpha = base_on$point_alpha,
    jitter_size = base_on$point_size,
    jitter_stroke = base_on$point_stroke,
    sharkup_color = "dodgerblue",
    sharkup_shape = base_on$point_shape,
    sharkup_alpha = base_on$point_alpha,
    sharkup_size = base_on$point_size,
    sharkup_stroke = base_on$point_stroke,
    sharkdn_color = "firebrick1",
    sharkdn_shape = base_on$point_shape,
    sharkdn_alpha = base_on$point_alpha,
    sharkdn_size = base_on$point_size,
    sharkdn_stroke = base_on$point_stroke,
    shkuptxt_alpha = base_on$text_alpha,
    shkuptxt_angle = base_on$text_angle,
    shkuptxt_color = "dodgerblue",
    shkuptxt_family = base_on$text_family,
    shkuptxt_fontface = base_on$text_fontface,
    shkuptxt_lineheight = base_on$text_lineheight,
    shkuptxt_size = base_on$text_size,
    shkuptxt_hjust = base_on$text_hjust,
    shkuptxt_vjust = base_on$text_vjust,
    shkdntxt_alpha = base_on$text_alpha,
    shkdntxt_angle = base_on$text_angle,
    shkdntxt_color = "firebrick1",
    shkdntxt_family = base_on$text_family,
    shkdntxt_fontface = base_on$text_fontface,
    shkdntxt_lineheight = base_on$text_lineheight,
    shkdntxt_size = base_on$text_size,
    shkdntxt_hjust = base_on$text_hjust,
    shkdntxt_vjust = base_on$text_vjust,
    label_alpha = base_on$text_alpha,
    label_color = base_on$text_color,
    label_family = base_on$text_family,
    label_fontface = base_on$text_fontface,
    label_lineheight = base_on$text_lineheight,
    label_size = base_on$text_size,
    heatmapfill_low = "steelblue",
    heatmapfill_mid = "white",
    heatmapfill_high = "firebrick",
    heatmap_color = "white",
    heatmap_linewidth = base_on$histogram_linewidth,
    heatmaptxt_alpha = base_on$text_alpha,
    heatmaptxt_angle = base_on$text_angle,
    heatmaptxt_color = base_on$text_color,
    heatmaptxt_family = base_on$text_family,
    heatmaptxt_fontface = base_on$text_fontface,
    heatmaptxt_lineheight = base_on$text_lineheight,
    heatmaptxt_size = base_on$text_size,
    heatmaptxt_hjust = base_on$text_hjust,
    heatmaptxt_vjust = base_on$text_vjust,
    linerange_color = base_on$line_color,
    linerange_linewidth = base_on$line_linewidth,
    linerange_linetype = base_on$line_linetype,
    linerange_alpha = base_on$line_alpha,
    rect_fill = "grey60",
    rect_alpha = 0.25,
  )

  # bug fix
  base_on$labeller =  ggplot2::labeller(.default = ggplot2::label_both,
                                         .multi_line = FALSE)

  # May rarely have these xp_theme elements already defined for an xpose
  # object being based_on, so don't want to overwrite.
  already_covered <- names(new_defs) %in% names(base_on)

  utils::modifyList(
    base_on,
    new_defs[!already_covered],
  ) %>%
    xpose::as.xpose.theme()
}


#' Updated version of the xpose4 theme
#' @returns An `xpose` theme object with `xpose4` color palette
#' @export
xp4_xtra_theme <- function() xp_xtra_theme(xpose::theme_xp_xpose4())

#########
# Labels and levels
#########

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
        value = `if`(
          nrow(this_lvls)==0,
          val2lvl(value),
          val2lvl(value, this_lvls)
          )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(rn, this_lvls))
    if (!show_n) return(out)
    out %>%
      dplyr::group_by(variable, value) %>%
      dplyr::mutate(
        value = paste0(value,"\nN = ", dplyr::n()) %>%
          forcats::as_factor() %>%
          forcats::fct_inorder()
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

apply_lul_wide <- function(xpdb, cols=NULL, lvl_cols=NULL, .problem=NULL, show_n = TRUE) {
  if (is.null(cols)) {
    cols <- xpose::get_data(xpdb, .problem = .problem, quiet = TRUE) %>%
      names() %>% unique()
  }
  nlnl_cols <- setdiff(cols, lvl_cols)

  lbl_unt_fun <- apply_labels_units(xpdb = xpdb, .problem = .problem)
  lvl_fun <- function(x) x
  if (check_xpdb_x(xpdb, .warn = FALSE)) lvl_fun <- apply_levels(xpdb = xpdb, .problem = .problem, show_n = show_n)

  function(x) {
   name_order <- names(x)
   if (length(nlnl_cols)>0) {
     wo_leveler_ <- x %>%
       tidyr::pivot_longer(
         cols = dplyr::all_of(nlnl_cols),
         names_to = "variable",
         values_to = "value"
       ) %>%
       dplyr::mutate(old_name = variable) %>%
       lbl_unt_fun()
     new_cols1 <- wo_leveler_ %>%
       { .$variable[match(nlnl_cols, .$old_name)] } %>%
       as.character() %>%
       unique()
     wo_leveler <- wo_leveler_ %>%
       dplyr::select(-old_name) %>%
       tidyr::pivot_wider(
         names_from = "variable",
         values_from = "value"
       )
     new_name_order <- name_order
     new_name_order[match(nlnl_cols, new_name_order)] <- new_cols1
     if (length(lvl_cols)==0) {
       return(dplyr::select(wo_leveler, !!new_name_order))
     }
   } else {
     wo_leveler <- dplyr::select(x, -everything())
     new_cols1 <- c()
     new_name_order <- name_order
   }
   w_leveler_ <- x %>%
     tidyr::pivot_longer(
       cols = dplyr::all_of(lvl_cols),
       names_to = "variable",
       values_to = "value"
     ) %>%
     lvl_fun() %>%
     dplyr::mutate(old_name = variable) %>%
     lbl_unt_fun()
   new_cols2 <- w_leveler_ %>%
     { .$variable[match(lvl_cols, .$old_name)] } %>%
     as.character() %>%
     unique()
   w_leveler <- w_leveler_ %>%
     dplyr::select(-old_name) %>%
     tidyr::pivot_wider(
       names_from = "variable",
       values_from = "value"
     ) %>%
     dplyr::mutate(dplyr::across(
       where(is.factor),
       forcats::fct_drop
     ))

   new_name_order[match(lvl_cols, new_name_order)] <- new_cols2

   dplyr::bind_cols(
     dplyr::select(x, !!setdiff(name_order, cols)),
     dplyr::select(wo_leveler, !!new_cols1),
     dplyr::select(w_leveler, !!new_cols2)
   ) %>%
     dplyr::select(!!new_name_order)
  }
}

#########
# Column resolution/grid-plot option helpers
#########

# Resolve a tidyselect (or, if `varsel` is a null quosure, every column of
# the given var type(s)) against xpdb's data, drop fixed columns, and
# validate the result actually belongs to those type(s). This is the
# column-resolution block shared by the eta_*/cov_*/shk_* plot family
# (see covariates.R).
#
# Callers must build `varsel` as its own statement (`q <- rlang::enquo(x);
# resolve_var_cols(..., varsel = q)`), not inline (`varsel =
# rlang::enquo(x)`) -- enquo() has to run in the frame that owns `x`'s
# promise, and an inline call is instead forced lazily from inside this
# function's frame, silently capturing the wrong (and useless) quosure.
# Callers must also reassign their own tidyselect argument (eg `etavar <-
# eta_col`) to the resolved result afterwards: aes()/ggplot() captures the
# caller's whole frame as `.Environment`, and `x`'s original promise (eg
# the bare symbol `ETA1`) is not valid outside a data-mask context --
# forced later (eg by waldo::compare()/expect_identical() walking that
# environment), it errors with "object 'ETA1' not found". Overwriting the
# binding with the already-resolved character vector avoids that.
resolve_var_cols <- function(xpdb, .problem, type, varsel, drop_fixed, quiet,
                              arg_name, label) {
  all_cols <- c()
  for (t in type) {
    all_cols <- c(all_cols, xpose::xp_var(xpdb, .problem, type = t, silent = TRUE)$col)
  }
  if (length(all_cols) == 0) {
    cli::cli_abort("No {label} column found in the xpdb data index.")
  }
  if (rlang::quo_is_null(varsel)) {
    sel_cols <- all_cols
  } else {
    sel_cols <- dplyr::select(
      xpose::get_data(xpdb, .problem = .problem, quiet = TRUE),
      !!varsel
    ) %>%
      names() %>%
      unique()
  }
  if (drop_fixed) {
    sel_cols <- xpose::drop_fixed_cols(xpdb, .problem, cols = sel_cols, quiet = quiet)
  }
  if (is.null(sel_cols) || length(sel_cols) == 0) {
    cli::cli_abort("No usable {label} column found in the xpdb data index.")
  }
  if (any(!sel_cols %in% all_cols)) {
    cli::cli_abort("`{arg_name}` should only include {label} columns, which does not seem to apply to: {setdiff(sel_cols, all_cols)}")
  }
  sel_cols
}

# Build xplot_pairs()'s `*_opts` arguments from a user-supplied override
# list, keeping the package default for anything not overridden. Shared by
# eta_grid()/cov_grid()/eta_vs_cov_grid()/shk_grid()/shk_vs_cov_grid().
pairs_opts_defaults <- function(pairs_opts) {
  formals(xplot_pairs) %>%
    names() %>%
    stringr::str_subset("_opts$") %>%
    rlang::set_names(., .) %>%
    purrr::map(~ {
      if (.x %in% names(pairs_opts)) pairs_opts[[.x]] else list()
    })
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
  grDevices::pdf(file = NULL)
  out <- print(plot)
  grDevices::dev.off()
  out
}


#' Ensure consistent style with `GGally` functions
#'
#' @param fn <`character`> name of `GGally` function
#' @param ... <`any`> additional arguments to pass to `GGally` function
#' @param xp_theme theme to use
#'
#' @return `ggplot2` function
#' @export
#'
#'
wrap_xp_ggally <- function(fn, xp_theme, ...) {
  checkmate::assertString(fn)
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

#####
# Individual plots
#####

#' Allocate a stratified sample size across strata
#'
#' @description
#' Proportionally allocates `n` draws across strata of the given `sizes`,
#' using the largest-remainder method so the allocation always sums to `n`
#' (capped at `sum(sizes)`). Internal helper for [`ind_plots_sample()`].
#'
#' @param sizes <`integer`> Number of units available in each stratum
#' @param n <`integer`> Total number of units to allocate
#'
#' @return An `integer` vector, same length as `sizes`, each entry no
#' greater than the corresponding entry of `sizes`, summing to
#' `min(n, sum(sizes))`.
#' @noRd
stratified_alloc <- function(sizes, n) {
  total <- sum(sizes)
  n <- min(n, total)
  raw <- sizes / total * n
  alloc <- floor(raw)
  capacity <- sizes - alloc
  remainder <- n - sum(alloc)
  frac <- raw - alloc

  # Largest fractional remainder first, but a non-empty stratum that
  # rounded down to zero jumps the queue - otherwise a small stratum could
  # be entirely excluded from the sample while capacity to include it exists.
  unrepresented <- alloc == 0 & sizes > 0
  ord <- order(-(unrepresented + frac), -capacity)
  i <- 1L
  while (remainder > 0) {
    idx <- ord[(i - 1L) %% length(ord) + 1L]
    if (capacity[idx] > 0) {
      alloc[idx] <- alloc[idx] + 1L
      capacity[idx] <- capacity[idx] - 1L
      remainder <- remainder - 1L
    }
    i <- i + 1L
  }
  as.integer(alloc)
}

#' Individual plots for a (stratified) sample of individuals
#'
#' @description
#' A wrapper around [`ind_plots`][xpose::ind_plots] that first draws a
#' sample of `n` individuals (9 by default, enough to fill a 3x3 page)
#' rather than plotting every individual in the dataset. If `stratify` is
#' provided, the sample is drawn proportionally from each level (or
#' combination of levels) of the `tidyselect`-ed column(s), so the sample
#' remains as representative as the data and `n` allow.
#'
#' @details
#' When `stratify` is used, the stratifying column(s) are appended to the
#' facet formula (in addition to the id column that [`ind_plots`][xpose::ind_plots]
#' already facets by), so that the stratum each sampled individual belongs
#' to is visible in the plot.
#'
#' Stratified sample sizes are allocated proportionally to stratum size
#' using the largest-remainder method, so the total sampled always equals
#' `min(n, sum(individuals available across all strata))`.
#'
#' @param xpdb <`xp_xtras`> or <`xpose_data`> object
#' @param n <`integer`> Number of individuals to sample. Defaults to 9. If
#' fewer individuals than `n` are available, all of them are used.
#' @param stratify <`tidyselect`> Optional column(s), other than the id
#' column, to stratify the sample by.
#' @param seed <`integer`> Optional seed, set (and restored on exit) for
#' reproducible sampling.
#' @param facets As in [`ind_plots`][xpose::ind_plots]. Defaults to the id
#' column (and `stratify` column(s), if given) added to
#' `xpdb$xp_theme$facets`.
#' @param .problem <`numeric`> Problem number to use.
#' @param quiet <`logical`> Silence extra output.
#' @param ... Passed on to [`ind_plots`][xpose::ind_plots]
#'
#' @return The desired plot
#' @export
#'
#' @seealso [ind_roc()]
#'
#' @examples
#' xpdb_x %>% ind_plots_sample(n = 6)
#' xpdb_x %>% ind_plots_sample(n = 6, stratify = SEX)
ind_plots_sample <- function(xpdb,
                              n = 9,
                              stratify = NULL,
                              seed = NULL,
                              facets,
                              .problem,
                              quiet,
                              ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = "data")
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  xpa("count", n, positive = TRUE)

  id_col <- xp_var(xpdb, .problem, type = "id")$col[1]
  data <- xpose::get_data(xpdb, .problem = .problem, quiet = quiet)

  strat_quo <- rlang::enquo(stratify)
  if (rlang::quo_is_null(strat_quo)) {
    strat_cols <- character(0)
  } else {
    strat_cols <- dplyr::select(data, {{ stratify }}) %>% names() %>% unique()
    strat_cols <- setdiff(strat_cols, id_col)
    if (length(strat_cols) == 0) {
      cli::cli_abort("`stratify` did not resolve to any columns other than the id column ({id_col}).")
    }
  }

  # One row per individual (using their first record for strata membership)
  id_tbl <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(id_col))) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c(id_col, strat_cols)))

  if (!is.null(seed)) {
    if (!exists(".Random.seed", envir = .GlobalEnv)) {
      on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
    } else {
      old_seed <- .GlobalEnv$.Random.seed
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
    }
    set.seed(seed)
  }

  if (n >= nrow(id_tbl)) {
    sampled_ids <- as.character(id_tbl[[id_col]])
  } else if (length(strat_cols) == 0) {
    sampled_ids <- id_tbl %>%
      dplyr::slice_sample(n = n) %>%
      dplyr::pull(dplyr::all_of(id_col)) %>%
      as.character()
  } else {
    strata <- id_tbl %>% dplyr::count(dplyr::across(dplyr::all_of(strat_cols)), name = "n_avail")
    alloc <- stratified_alloc(strata$n_avail, n)
    sampled_ids <- purrr::map(seq_len(nrow(strata)), function(i) {
      if (alloc[i] == 0) return(character(0))
      grp <- dplyr::inner_join(id_tbl, strata[i, strat_cols, drop = FALSE], by = strat_cols)
      grp %>%
        dplyr::slice_sample(n = alloc[i]) %>%
        dplyr::pull(dplyr::all_of(id_col)) %>%
        as.character()
    }) %>%
      unlist(use.names = FALSE)
  }

  xpdb <- dplyr::filter(xpdb, !!rlang::sym(id_col) %in% !!sampled_ids, .problem = .problem)

  if (missing(facets)) {
    facets <- xpose::add_facet_var(facets = xpdb$xp_theme$facets, variable = id_col)
    for (strat_col in strat_cols) {
      facets <- xpose::add_facet_var(facets = facets, variable = strat_col)
    }
  }

  xpose::ind_plots(xpdb, facets = facets, .problem = .problem, quiet = quiet, ...)
}
