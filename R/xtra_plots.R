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
#' cutomized `xp_theme`. There is are only a few
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
  )

  # May rarely have these xp_theme elements already defined for an xpose
  # object being based_on, so don't want to overwrite.
  already_covered <- names(new_defs) %in% names(base_on)

  utils::modifyList(
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
