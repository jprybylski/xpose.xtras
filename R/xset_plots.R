########
# Simple Plots (these may spin into their own script like shark_plots)
########

#' Objective function changes across models
#'
#' @description
#' Another visualization of how individual objective functions change over
#' the course of model development.
#'
#'
#' @inheritParams xplot_boxplot
#' @param xpdb_s <`xpose_set`> object
#' @param ... <`tidyselect`> of models in set. If empty, all models are
#' used in order of their position in the set. May also use a formula,
#' which will just be processed with `all.vars()`.
#' @param .lineage <`logical`> where if `TRUE`, `...` is processed
#' @param auto_backfill <`logical`> If `TRUE`, apply <[`backfill_iofv()`]>
#' automatically. `FALSE` by default to encourage data control as a
#' separate process to plotting control.
#' @param type Passed to <[`xplot_boxplot`]>
#' @param axis.text What to label the model. This is parsed on a per-model
#' basis.
#' @param facets Additional facets
#' @param .problem Problem number
#' @param quiet Silence output
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' pheno_set %>%
#'   focus_qapply(backfill_iofv) %>%
#'   iofv_vs_mod()
#'
#' pheno_set %>%
#'   focus_qapply(backfill_iofv) %>%
#'   iofv_vs_mod(run3,run11,run14,run15)
#'
#' pheno_set %>%
#'   focus_qapply(backfill_iofv) %>%
#'   iofv_vs_mod(.lineage = TRUE)
#'
#' }
iofv_vs_mod <- function(
    xpdb_s,
    ...,
    .lineage = FALSE,
    auto_backfill = FALSE,
    mapping  = NULL,
    orientation = "x",
    type     = 'bjc',
    title    = 'Individual OFVs across models',
    subtitle = 'Based on @nind individuals, Initial OFV: @ofv',
    caption  = 'Initial @dir',
    tag      = NULL,
    axis.text = "@run",
    facets,
    .problem,
    quiet
) {
  check_xpose_set(xpdb_s)

  # Make sure dots are unnamed
  rlang::check_dots_unnamed()

  n_set_dots(xpdb_s, ..., .lineage=.lineage) # makes `mods`

  pre_process <- function(x) unfocus_xpdb(x)
  if (auto_backfill==TRUE) pre_process <- function(x) focus_qapply(x, backfill_iofv)
  xpose_subset <- xpdb_s %>% pre_process() %>% select(!!mods)

  # extra checks
  if (missing(.problem))
    .problem <- xpose::default_plot_problem(xpose_subset[[1]]$xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet))
    quiet <- xpose_subset[[1]]$xpdb$options$quiet
  if (missing(facets))
    facets <- xpose_subset[[1]]$xpdb$xp_theme$facets



  xpdb_l <- purrr::map(xpose_subset, ~.x$xpdb)
  rlang::try_fetch(
    ofv_cols <- purrr::map_chr(xpdb_l,
                               ~xp_var(.x, type="iofv", silent=TRUE)$col[1]),
    error = function(s) {
      rlang::abort(auto_backfill_suggestion, parent = s)
    }
  )
  ofv_frk_cols <- paste0(ofv_cols,"_",seq_along(ofv_cols))
  nicer_labs <- purrr::map_chr(xpdb_l,~xpose::parse_title(axis.text,.x, .problem))

  # Get combined xpdb
  xpdb_f <- franken_xpdb(
    !!!xpdb_l,
    .types = "iofv",
    problem = .problem
  )

  post_processing <- function(df) {
    df %>%
      # Relabel variable
      dplyr::mutate(
        variable = nicer_labs[match(variable,ofv_frk_cols)] %>%
          forcats::as_factor() %>%
          forcats::fct_inorder()
      )
  }
  opt <- xpose::data_opt(.problem = .problem,
                         filter = xpose::only_distinct(xpdb_f, .problem, facets, quiet),
                         tidy = TRUE, value_col = ofv_frk_cols, post_processing = post_processing)

  if (orientation=="x") {
    vars <- xpose::aes_c(aes(
      x = .data[["variable"]],
      y = .data[["value"]]), mapping)
    xscale = "discrete"
    yscale = xpose::check_scales('y', NULL)
  } else {
    vars <- xpose::aes_c(aes(
      x = .data[["value"]],
      y = .data[["variable"]]), mapping)
    yscale = "discrete"
    xscale = xpose::check_scales('x', NULL)
  }

  really_quiet <- function(x) x
  if (quiet) really_quiet <- function(x) suppressWarnings(x) # <- trivial reshape warning silenced

  really_quiet(xplot_boxplot(
    xpdb = xpdb_f,
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
    plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::")))

}

#' Specific waterfall plots
#'
#' @description
#' Differences are second listed model minus first listed. Eg, in
#' `eta_waterfall(run1,run2)`, the when etas in run2 are greater than
#' those in run1, the difference will be positive.
#'
#'
#' @rdname waterfalls
#'
#' @inheritParams xset_waterfall
#'
#' @return <`xpose_plot`> object
#' @export
#'
#'
#' @details
#' For type-based customization of plots:
#' \itemize{
#'   \item `b` bar plot (from `geom_bar`)
#'   \item `h` hline at 0 (from `geom_hline`)
#'   \item `t` text of change value (from `geom_text`)
#' }
#'
#'
#' @examples
#'
#' # Parameter value changes
#' pheno_set %>%
#'   # Ensure param is set
#'   focus_qapply(set_var_types, param=c(CL,V)) %>%
#'   prm_waterfall(run5,run6)
#'
#'
#' # EBE value changes
#' pheno_set %>%
#'   eta_waterfall(run5,run6)
#'
#' # iOFV changes
#' pheno_set %>%
#'   focus_qapply(backfill_iofv) %>%
#'   # Note the default scaling is flipped here
#'   iofv_waterfall(run5,run6)
#'
#'
prm_waterfall <- function(
    xpdb_s,
    ...,
    .inorder=FALSE,
    type = "bh",
    max_nind = 0.7,
    scale_diff = TRUE,
    show_n = TRUE,
    title    = "Parameter changes between models | @run",
    subtitle = "Based on @nobs observations in @nind individuals",
    caption  = "@dir",
    tag      = NULL,
    facets = NULL,
    facet_scales = "free_x",
    .problem,
    .subprob,
    .method,
    quiet
) {

  # process dots (this is also done in forwarded model, but need to do here. Plus it's fast enough)
  two_set_dots(xpdb_s, ..., .inorder=.inorder)
  # Now have mod1 and mod2

  # Get params
  waterfall_helper("param", "parameter")

  xset_waterfall(
    xpdb_s = xpdb_s,
    ...,
    .inorder=.inorder,
    type = type,
    .cols = dplyr::all_of(c(m1col)),
    max_nind = max_nind,
    scale_diff = scale_diff,
    show_n = show_n,
    title    = title,
    subtitle = subtitle,
    caption  = caption,
    tag      = tag,
    plot_name = 'prm_waterfall',
    facets = facets,
    facet_scales = facet_scales,
    .problem=.problem,
    .subprob=.subprob,
    .method=.method,
    quiet=quiet
  )
}
#' @rdname waterfalls
#' @export
eta_waterfall <- function(
    xpdb_s,
    ...,
    .inorder=FALSE,
    type = "bh",
    max_nind = 0.7,
    scale_diff = TRUE,
    show_n = TRUE,
    title    = "Eta changes between models | @run",
    subtitle = "Based on @nobs observations in @nind individuals",
    caption  = "@dir",
    tag      = NULL,
    facets = NULL,
    facet_scales = "free_x",
    .problem,
    .subprob,
    .method,
    quiet
) {

  # process dots (this is also done in forwarded model, but need to do here. Plus it's fast enough)
  two_set_dots(xpdb_s, ..., .inorder=.inorder)
  # Now have mod1 and mod2

  # Get etas
  waterfall_helper("eta", "eta")


  # Eta label consistency
  if (xpose::software(mod1$xpdb) == 'nonmem') {
    post_processing <-  function(x) {
      x %>%
        dplyr::mutate(
          variable = stringr::str_replace(variable, "^ET(A?)(\\d+)$", "ETA(\\2)")
        )
    }
  } else {
    post_processing <- function(x) x
  }

  xset_waterfall(
    xpdb_s = xpdb_s,
    ...,
    .inorder=.inorder,
    type = type,
    .cols = dplyr::all_of(c(m1col)),
    max_nind = max_nind,
    scale_diff = scale_diff,
    show_n = show_n,
    title    = title,
    subtitle = subtitle,
    caption  = caption,
    tag      = tag,
    plot_name = 'eta_waterfall',
    facets = facets,
    facet_scales = facet_scales,
    opt = xpose::data_opt(post_processing = post_processing),
    .problem=.problem,
    .subprob=.subprob,
    .method=.method,
    quiet=quiet
  )
}
#' @rdname waterfalls
#' @export
iofv_waterfall <- function(
    xpdb_s,
    ...,
    .inorder=FALSE,
    type = "bh",
    max_nind = 0.7,
    scale_diff = FALSE,
    show_n = TRUE,
    title    = "iOFV changes between models | @run",
    subtitle = "Based on @nobs observations in @nind individuals",
    caption  = "@dir",
    tag      = NULL,
    facets = NULL,
    facet_scales = "free_x",
    .problem,
    .subprob,
    .method,
    quiet
) {

  # process dots (this is also done in forwarded model, but need to do here. Plus it's fast enough)
  two_set_dots(xpdb_s, ..., .inorder=.inorder)
  # Now have mod1 and mod2

  # Get values
  waterfall_helper("iofv", "iOFV")
  xset_waterfall(
    xpdb_s = xpdb_s,
    ...,
    .inorder=.inorder,
    type = type,
    .cols = dplyr::all_of(c(m1col)),
    max_nind = max_nind,
    scale_diff = scale_diff,
    show_n = show_n,
    title    = title,
    subtitle = subtitle,
    caption  = caption,
    tag      = tag,
    plot_name = 'ofv_waterfall',
    facets = facets,
    facet_scales = facet_scales,
    .problem=.problem,
    .subprob=.subprob,
    .method=.method,
    quiet=quiet
  )
}

#' Compare model predictions
#'
#' @rdname pred_vs_pred
#'
#' @description
#' For two models in an `xpose_set`, these functions are useful in comparing individual
#' and population predictions
#'
#' @param xpdb_s <`xpose_set`> object
#' @param ... See <[`two_set_dots`]>
#' @param .inorder See <[`two_set_dots`]>
#' @param type Passed to `xplot_scatter`
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param log Log scale covariate value?
#' @param guide Add guide line?
#' @param facets Additional facets
#' @param .problem Problem number
#' @param quiet Silence output
#'
#' @export
#'
#' @examples
#'
#' pheno_set %>%
#'   ipred_vs_ipred(run5,run15)
#'
#' pheno_set %>%
#'   pred_vs_pred(run5,run15)
#'
ipred_vs_ipred <- function(
    xpdb_s,
    ...,
    .inorder=FALSE,
    type = "pls",
    title    = "Individual prediction comparison | @run",
    subtitle = "Ofv: @ofv, Eps shrink: @epsshk",
    caption  = "@dir",
    tag      = NULL,
    log = NULL,
    guide = TRUE,
    facets,
    .problem,
    quiet) {

  # process dots
  two_set_dots(xpdb_s, ..., .inorder=.inorder)
  # Now have mod1 and mod2

  # extra checks
  if (missing(.problem))
    .problem <- xpose::default_plot_problem(mod1$xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet))
    quiet <- mod1$xpdb$options$quiet
  if (missing(facets))
    facets <- mod1$xpdb$xp_theme$facets

  # Use waterfall helper to make sure ipred is available and column names match
  waterfall_helper("ipred", "IPRED")


  # Get combined xpdb
  xpdb_f <- franken_xpdb(
    mod1$xpdb,
    mod2$xpdb,
    .types = "ipred",
    problem = .problem,
    prop_transforms = function(xpdb_f, xpdb_list, problem) {
      # Combine OFV
      updated <- franken_prop(
        xpdb_f = xpdb_f,
        xpdb_list = xpdb_list,
        prop = "ofv",
        problem = problem,
        glue_cmd = franken_numprop)
      # Combine epsshk
      updated <- franken_prop(
        xpdb_f = updated,
        xpdb_list = xpdb_list,
        prop = "epsshk",
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

  # TODO: add post-processing for levels labels etc of facets
  new_col_names <- paste0(c(
    m1col, m2col # should be the same, still
  ), " (", c(
    get_prop(mod1$xpdb, "run"),
    get_prop(mod2$xpdb, "run")
  ), ")")
  post_processing <- function(df) {
    df %>%
      dplyr::rename_with(~dplyr::case_when(
        grepl(sprintf("^%s_1$",m1col), .x) ~ new_col_names[1],
        grepl(sprintf("^%s_2$",m2col), .x) ~ new_col_names[2],
        TRUE ~ .x
      ))
  }

  xpose::xplot_scatter(
    xpdb = xpdb_f,
    mapping = aes(
      x = .data[[new_col_names[1]]],
      y = .data[[new_col_names[2]]]
    ),
    opt = xpose::data_opt(.problem = .problem, filter = xpose::only_obs(xpdb_f, .problem, quiet),
                          post_processing = post_processing),
    guide = guide,
    guide_slope = 1,
    facets = facets,
    xscale = xpose::check_scales("x", log),
    yscale = xpose::check_scales("y", log),
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    quiet = quiet,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]),"(\\w+\\.*)+::")
  )
}
#' @rdname pred_vs_pred
#' @export
pred_vs_pred <- function(
    xpdb_s,
    ...,
    .inorder=FALSE,
    type = "pls",
    title    = "Population prediction comparison | @run",
    subtitle = "Ofv: @ofv, Eps shrink: @epsshk",
    caption  = "@dir",
    tag      = NULL,
    log = NULL,
    guide = TRUE,
    facets,
    .problem,
    quiet) {

  # process dots
  two_set_dots(xpdb_s, ..., .inorder=.inorder)
  # Now have mod1 and mod2

  # extra checks
  if (missing(.problem))
    .problem <- xpose::default_plot_problem(mod1$xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet))
    quiet <- mod1$xpdb$options$quiet
  if (missing(facets))
    facets <- mod1$xpdb$xp_theme$facets

  # Use waterfall helper to make sure ipred is available and column names match
  waterfall_helper("pred", "PRED")


  # Get combined xpdb
  xpdb_f <- franken_xpdb(
    mod1$xpdb,
    mod2$xpdb,
    .types = "pred",
    problem = .problem,
    prop_transforms = function(xpdb_f, xpdb_list, problem) {
      # Combine OFV
      updated <- franken_prop(
        xpdb_f = xpdb_f,
        xpdb_list = xpdb_list,
        prop = "ofv",
        problem = problem,
        glue_cmd = franken_numprop)
      # Combine epsshk
      updated <- franken_prop(
        xpdb_f = updated,
        xpdb_list = xpdb_list,
        prop = "epsshk",
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

  # TODO: add post-processing for levels labels etc of facets
  new_col_names <- paste0(c(
    m1col, m2col # should be the same, still
  ), " (", c(
    get_prop(mod1$xpdb, "run"),
    get_prop(mod2$xpdb, "run")
  ), ")")
  post_processing <- function(df) {
    df %>%
      dplyr::rename_with(~dplyr::case_when(
        grepl(sprintf("^%s_1$",m1col), .x) ~ new_col_names[1],
        grepl(sprintf("^%s_2$",m2col), .x) ~ new_col_names[2],
        TRUE ~ .x
      ))
  }

  xpose::xplot_scatter(
    xpdb = xpdb_f,
    mapping = aes(
      x = .data[[new_col_names[1]]],
      y = .data[[new_col_names[2]]]
    ),
    opt = xpose::data_opt(.problem = .problem, filter = xpose::only_obs(xpdb_f, .problem, quiet),
                          post_processing = post_processing),
    guide = guide,
    guide_slope = 1,
    facets = facets,
    xscale = xpose::check_scales("x", log),
    yscale = xpose::check_scales("y", log),
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    quiet = quiet,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]),"(\\w+\\.*)+::")
  )
}

# This would also just be an in situ xpdb, but there may need to be a function
# for model-averaging
#' Model average plots
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This is for use when the model averaging of a set
#' is planned.
#'
#'
#' @inheritParams modavg_xpdb
#' @param .fun <`function`> For slightly more convenient
#' piping of model-averaged `xpose_data` into a plotting
#' function.
#' @param .funargs <`list`> Extra args to pass to function.
#' If passing `tidyselect` arguments, be mindful of where
#' quosures might be needed. See Examples.
#'
#' @seealso [modavg_xpdb()]
#'
#' @rdname modavg_plots
#' @export
#'
#' @examples
#' \dontrun{
#'
#' pheno_set %>%
#'   dv_vs_ipred_modavg(run8,run9,run10, auto_backfill = TRUE)
#'
#' pheno_set %>%
#'   dv_vs_pred_modavg(run8,run9,run10, auto_backfill = TRUE)
#'
#' pheno_set %>%
#'   ipred_vs_idv_modavg(run8,run9,run10, auto_backfill = TRUE)
#'
#' pheno_set %>%
#'   pred_vs_idv_modavg(run8,run9,run10, auto_backfill = TRUE)
#'
#' # Model averaged ETA covariates
#' pheno_set %>%
#'   plotfun_modavg(run8,run9,run10, auto_backfill = TRUE,
#'      avg_by_type = "eta",.fun = eta_vs_catcov,
#'      # Note quosure
#'      .funargs = list(etavar=rlang::quo(ETA1)))
#'
#' }
dv_vs_ipred_modavg <- function(
    xpdb_s,
    ...,
    .lineage = FALSE,
    algorithm = c("maa","msa"),
    weight_type = c("individual","population"),
    auto_backfill = FALSE,
    weight_basis = c("ofv","aic","res"),
    res_col = "RES",
    quiet) {
  plotfun_modavg(
    xpdb_s=xpdb_s,
    ...,
    .lineage = .lineage,
    avg_by_type = "ipred",
    algorithm = algorithm,
    weight_type = weight_type,
    auto_backfill = auto_backfill,
    weight_basis = weight_basis,
    res_col = res_col,
    .fun = xpose::dv_vs_ipred,
    quiet=quiet
  )
}
#' @rdname modavg_plots
#' @export
dv_vs_pred_modavg <- function(
    xpdb_s,
    ...,
    .lineage = FALSE,
    algorithm = c("maa","msa"),
    weight_type = c("individual","population"),
    auto_backfill = FALSE,
    weight_basis = c("ofv","aic","res"),
    res_col = "RES",
    quiet) {
  plotfun_modavg(
    xpdb_s=xpdb_s,
    ...,
    .lineage = .lineage,
    avg_by_type = "pred",
    algorithm = algorithm,
    weight_type = weight_type,
    auto_backfill = auto_backfill,
    weight_basis = weight_basis,
    res_col = res_col,
    .fun = xpose::dv_vs_pred,
    quiet=quiet
  )
}
#' @rdname modavg_plots
#' @export
ipred_vs_idv_modavg <- function(
    xpdb_s,
    ...,
    .lineage = FALSE,
    algorithm = c("maa","msa"),
    weight_type = c("individual","population"),
    auto_backfill = FALSE,
    weight_basis = c("ofv","aic","res"),
    res_col = "RES",
    quiet) {
  plotfun_modavg(
    xpdb_s=xpdb_s,
    ...,
    .lineage = .lineage,
    avg_by_type = "ipred",
    algorithm = algorithm,
    weight_type = weight_type,
    auto_backfill = auto_backfill,
    weight_basis = weight_basis,
    res_col = res_col,
    .fun = xpose::ipred_vs_idv,
    quiet=quiet
  )
}
#' @rdname modavg_plots
#' @export
pred_vs_idv_modavg <- function(
    xpdb_s,
    ...,
    .lineage = FALSE,
    algorithm = c("maa","msa"),
    weight_type = c("individual","population"),
    auto_backfill = FALSE,
    weight_basis = c("ofv","aic","res"),
    res_col = "RES",
    quiet) {
  plotfun_modavg(
    xpdb_s=xpdb_s,
    ...,
    .lineage = .lineage,
    avg_by_type = "pred",
    algorithm = algorithm,
    weight_type = weight_type,
    auto_backfill = auto_backfill,
    weight_basis = weight_basis,
    res_col = res_col,
    .fun = xpose::pred_vs_idv,
    quiet=quiet
    )
}

#' @rdname modavg_plots
#' @export
plotfun_modavg <- function(
    xpdb_s,
    ...,
    .lineage = FALSE,
    avg_cols = NULL,
    avg_by_type = NULL,
    algorithm = c("maa","msa"),
    weight_type = c("individual","population"),
    auto_backfill = FALSE,
    weight_basis = c("ofv","aic","res"),
    res_col = "RES",
    .fun = NULL,
    .funargs = list(),
    quiet) {
  if (!is.function(.fun))
    cli::cli_abort("`.fun` must be a function, not a {.emphasis {class(.fun)[1]}}")
  # Prevent potentially common partial_match confusion
  if ("xpdb" %in% rlang::call_args_names(rlang::frame_call())) {
    if (xpose::is.xpdb(xpdb_s))
      rlang::abort("An `xpose_set` is required, not an `xpose_data` item.")
  }
  # Any .funargs in dots?
  dots_args <- rlang::enquos(..., .ignore_empty = "all", .unquote_names = TRUE)
  dots_names <- names(dots_args)
  ignore_check <- dots_names %in% c("xpdb","...",".")
  funarg_check <- dots_names %in% names(formals(.fun))
  funarg_dots <- dots_args[funarg_check]# & !ignore_check]
  if (length(funarg_dots)>0) .funargs <- modifyList(
    .funargs,
    purrr::map(funarg_dots, rlang::eval_tidy)
  )
  select_dots <- dots_args[!funarg_check]# & !ignore_check]
  maXPDB <- rlang::inject(modavg_xpdb(
    xpdb_s=xpdb_s,
    !!!select_dots,
    .lineage = .lineage,
    avg_cols = avg_cols,
    avg_by_type = avg_by_type,
    algorithm = algorithm,
    weight_type = weight_type,
    auto_backfill = auto_backfill,
    weight_basis = weight_basis,
    res_col = res_col,
    quiet=quiet
  ))
  if (!missing(quiet)) .funargs$quiet <- quiet
  if (!"title" %in% names(.funargs)) .funargs$title <- paste(
    "Model averaged",
    formals(.fun)$title
  )
  approach_that_works <- function(...) {
    .fun(
      maXPDB,
      ...
    )
  }
  # works but doesn't have the same flexibility
  # .fun(
  #   maXPDB,
  #   title = paste(
  #     "Model averaged",
  #     formals(.fun)$title
  #   )
  # )
  # Along with do.call, seemingly doesn't work
  # rlang::exec(
  #   .fun,
  #   xpdb = maXPDB,
  #   title = paste(
  #     "Model averaged",
  #     formals(.fun)$title
  #   ),
  #   !!!.funargs
  # )
  rlang::exec(
    approach_that_works,
    !!!.funargs
  )
}



########
# Helper functions (general)
########


#' Typical processing for plots of 2 sets
#'
#' @param xpdb_s <`xpose_set`> object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Passed to <[`select_subset`]>
#' @param .inorder <`logical`> Regardless of base model or parentage, use the
#' two plots in order of how they are in arguments. First plot listed is treated
#' as base or parent.
#' @param envir Where to assign `mod1` and `mod2` <`xpose_set_item`>s
#'
#' @return Into environment specified by `envir`, <`xpose_set_item`> `mod1` and `mod2`
#' @keywords internal
#'
#' @details
#' Note that this function does not return valid `xpdb`-like objects (<`xpose_data`>
#' or <`xp_xtras`>). The necessary objects for most functions can be retrieved using
#' `mod1$xpdb` and `mod2$xpdb`.
#'
#'
two_set_dots <- function(
    xpdb_s, ...,
    .inorder=FALSE,
    envir = parent.frame()
    ) {
  check_xpose_set(xpdb_s, .warn = FALSE)

  if (length(xpdb_s)<2) {
    cli::cli_abort("Need at least two models in set.")
  }

  tidyselect_check <- xpdb_s %>% select_subset(...) %>% length()

  # Process dots
  if (rlang::dots_n(...)==0 && length(xpdb_s)!=2) {
    cli::cli_abort("Need to select at least two models in set or use a length 2 set.")
  }  else if (rlang::dots_n(...)==0) {
    return(two_set_dots(
      xpdb_s,
      !!names(xpdb_s)[1],
      !!names(xpdb_s)[2],
      .inorder = .inorder,
      envir = envir))
  } else if (rlang::dots_n(...)!=2 && tidyselect_check!=2) {
    cli::cli_abort("Need to select exactly two models in set.")
  } else if (rlang::dots_n(...)==2 ||  tidyselect_check==2) {
    dot_mods <- select_subset(xpdb_s, ...)
    base_name <- get_base_model(xpdb_s)
    parent_name <- names(dot_mods)[c(
      xpdb_s[[dot_mods[1]]] %p% xpdb_s[[dot_mods[2]]],
      xpdb_s[[dot_mods[2]]] %p% xpdb_s[[dot_mods[1]]]
    )]

    base_mod_irrelevant <- length(base_name)==0 || !base_name %in% names(dot_mods)
    neither_is_parent <- length(parent_name)==0
    no_base_or_parent <- base_mod_irrelevant && neither_is_parent
    if (no_base_or_parent ||
        isTRUE(.inorder)) {
      mod1 <- xpdb_s[[dot_mods[1]]]
      mod2 <- xpdb_s[[dot_mods[2]]]
      assign("mod1", mod1, envir = envir)
      assign("mod2", mod2, envir = envir)
      return()
    }
    if (!base_mod_irrelevant) {
      mod1 <- xpdb_s[[base_name]]
      mod2 <- xpdb_s[[names(dot_mods)[names(dot_mods)!=base_name]]]
      assign("mod1", mod1, envir = envir)
      assign("mod2", mod2, envir = envir)
      return()
    }
    if (!neither_is_parent) {
      mod1 <- xpdb_s[[parent_name]]
      mod2 <- xpdb_s[[names(dot_mods)[names(dot_mods)!=parent_name]]]
      assign("mod1", mod1, envir = envir)
      assign("mod2", mod2, envir = envir)
      return()
    }
    cli::cli_abort()
  }
}

# Generalization of two_set_dots, only used in a few functions
n_set_dots <- function(
    xpdb_s,
    ...,
    .lineage=FALSE,
    envir = parent.frame()
) {
  tidyselect_check <- FALSE
  try_tidy <- try(select_subset(xpdb_s, ...), silent = TRUE)
  if (!"try-error" %in% class(try_tidy)) tidyselect_check <- TRUE

  if (.lineage == TRUE) {
    mods <- xset_lineage(xpdb_s, ..., .spinner = FALSE)
    if (is.list(mods)) {
      rlang::abort(paste(
        "`xset_lineage()` returned a list.",
        "If requesting to process `...` as a lineage, cannot request multiple lineages.",
        "Specifically, `...` should be empty or a single model name."
      ))
    }
  } else if (rlang::dots_n(...) == 0) {
    mods <- names(xpdb_s)
  } else if (rlang::dots_n(...) == 1 && !tidyselect_check && suppressWarnings(is_formula_list(rlang::dots_list(...)))) {
    # Warning is meaningless for this case, is using tidyselect in dots (eg, all_of(charcater list))
    mods <- all.vars(rlang::dots_list(...)[[1]])
  } else {
    mods <- select_subset(xpdb_s, ...) %>% names()
  }
  if (any(!mods %in% names(xpdb_s)) && !tidyselect_check) {
    cli::cli_abort("Selected models not in set: {.strong {setdiff(mods, names(xpdb_s))}}")
  }
  assign("mods",mods,envir = envir)
  NULL
}

#' Combine several `xpose_data` objects into one
#'
#' @description
#' This is an internal function designed to meet the needs
#' of specific plotting functions
#'
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> `xpose_data` or `xp_xtra` objects
#' @param .cols <`tidyselect`> of data columns
#' @param .types <`character`> of data types in addition to columns
#' @param prop_transforms <`function`> Extra processing using <[`franken_prop`]>
#' @param problem <`numeric`> Problems to look for `cols` and `types` (defaults all)
#' @param quiet Prevents extra output.
#'
#' @return The first `xpose_data` object with new data columns
#' @keywords internal
#'
#' @examples
#'
#'
#' xpose.xtras:::franken_xpdb(pheno_base, pheno_final, .types="catcov") %>%
#'   xpose::get_data() %>%
#'   select(starts_with("APGR"))
#'
franken_xpdb <-  function(
    ...,
    .cols = NULL,
    .types = NULL,
    prop_transforms = NULL,
    problem,
    quiet = TRUE
) {
  rlang::check_dots_unnamed()
  if (length(rlang::list2(...))<2) {
    cli::cli_abort("Need at least two `xpose_data` or `xp_xtra` objects.")
  }

  if (is.null(.types) && rlang::quo_is_null(rlang::enquo(.cols)))
    rlang::abort("Need `.cols` and/or `.types` to add into new object.")
  # See if .cols is character
  char_cols_check <- suppressWarnings(try(.cols,silent = TRUE))
  if (!rlang::quo_is_null(rlang::enquo(.cols)) &&
      !"try-error" %in% class(char_cols_check)) {
    .cols <- suppressWarnings(all_of(.cols))
  }

  xpdb_list <- rlang::dots_list(...)

  if (missing(problem)) problem <- xpose::all_data_problem(xpdb_list[[1]])

  if (is.null(prop_transforms)) {
    prop_transforms = function(xpdb_f, xpdb_list, problem) {
      xpdb_f
    }
  }

  # Make sure all data are compatible while extracting data
  prob_data <- purrr::map(problem, ~vector(mode="list", length = length(xpdb_list)))
  for (index in seq_along(xpdb_list)) {
    db <- xpdb_list[[index]]
    xpose::check_xpdb(db, check="data")

    for (probi in seq_along(problem)) {
      prob <- problem[probi]
      if (!prob %in% xpose::all_data_problem(db))
        cli::cli_abort("No prob no.{prob} in {.strong {get_prop(db, 'run')}}")

      # Types check (and get cols)
      tcols <- NULL
      if (!is.null(.types))
        tcols <- rlang::try_fetch(
          xp_var(db, .problem = prob, type = .types)$col,
          error = function(s) {
            rlang::abort(
              paste("Error with `xpose_data` for prob no.",prob,"in",get_prop(db, 'run')),
              parent = s
            )
          })


      # Get data
      this_data <- xpose::get_data(db, .problem = prob, quiet=quiet)
      # nrow check
      prev_nrow <- nrow(this_data)
      if (index>1) prev_nrow <- prob_data[[prob]] %>% .[[index-1]] %>% nrow()
      if (nrow(this_data)!=prev_nrow)
        cli::cli_abort(paste0("{.strong {get_prop(db, 'run')}} has {nrow(this_data)} rows",
                              ", which does not match previous {prev_nrow} rows."))

      # ID check
      this_ids <- this_data[[xp_var(db, .problem = prob, type = "id")$col]]
      prev_ids <- this_ids
      if (index>1) prev_ids <- xpdb_list[[index-1]] %>%
        xpose::get_data(.problem = problem, quiet=quiet) %>%
        .[[xp_var(xpdb_list[[index-1]], .problem = prob, type = "id")$col]]
      if (!identical(this_ids,prev_ids))
        cli::cli_abort(paste0("{.strong {get_prop(db, 'run')}} IDs do not match previous runs.",
                              " This function expects data rows and order to be identical."))

      # cols extract
      cols_set <- rlang::try_fetch(
        dplyr::select(this_data, c({{.cols}}, dplyr::any_of(tcols))),
        error = function(s) {
          rlang::abort(
            paste("Error with `xpose_data` for prob no.",prob,"in",get_prop(db, 'run')),
            parent = s
          )
        })

      # Fill data
      prob_data[[probi]][[index]] <- cols_set

    }
  }

  ## Create xpose_data type object which combines info
  new_xpdb <- xpdb_list[[1]]  %>%
    # Ensure this commonly called property is updated
    franken_prop(xpdb_list, "run")
  for (prob in problem) {
    new_data_list <- purrr::map2_dfc(prob_data[[prob]], seq_along(xpdb_list), ~{
      # .x is the df of columns from the data
      # .y is the numerical index of the object, to append to column names
      .x %>%
        dplyr::rename_with(function(nm) paste0(nm,"_",.y))
    }) %>%
      as.list()

    new_xpdb <- new_xpdb %>%
      xpose::mutate(!!!new_data_list, .problem = prob, .source = "data")
  }
  new_xpdb %>%
    prop_transforms(xpdb_list,problem) %>%
    as_xp_xtras()
}

#' Combine a property from all components of a `franken_xpdb`
#'
#' @param xpdb_f A product of [`franken_xpdb`]
#' @param xpdb_list List of the source `xpose_data` objects.
#' @param prop <`character`> of the property to combine
#' @param glue_cmd Any special transformation to the properties,
#' including how to collapse.
#' @param problem <`numeric`> If necessary to specify
#' @param indices <`numeric`> Index values `1:length(xpdb_list)`
#' to include in the property collapse.
#'
#' @details
#' This function is meant to be called within [`franken_xpdb`].
#' It is expected to be ready to handle cases where, for example,
#' multiple props are being set in a pipe, or a problem-associated
#' property is being set while a problem=0 property is also being
#' set.
#'
#' This is a *low-level* function, so its use outside of internal
#' functions is not intended.
#'
#' @return Same as `xpdb_f` with new properties.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # This is designed to be called in a function environment which
#' # would provide something like the following:
#'
#' xpdb_f <- xpose.xtras:::franken_xpdb(pheno_base, pheno_final, .types="catcov")
#'
#' xpdb_list <- list(pheno_base, pheno_final)
#'
#' # The following would be inside the function
#' xpdb_f %>%
#'   franken_prop(xpdb_list, "run",
#'     glue_cmd = function(x) paste(x, collapse="+"))
#'
#' # xpdb_f may have to be written to a few times if
#' # and problem-specific combinations are needed:
#'
#' updated <- xpdb_f %>%
#'   franken_prop(xpdb_list, "run",
#'     glue_cmd = function(x) paste(x, collapse="+"))
#'
#' # problem will also be available. Assume there's
#' # no reason to loop here, but that may be needed
#' problem <- 1
#' updated <- updated %>%
#'   franken_prop(xpdb_list, "ofv",  problem=problem,
#'     glue_cmd = function(x) paste(x, collapse="&"))
#' }
franken_prop <- function(
    xpdb_f,
    xpdb_list,
    prop,
    problem=NULL,
    glue_cmd = function(x) glue::glue_collapse(x, ", ", last = " and "),
    indices = seq_along(xpdb_list)
) {
  xpdb_f %>%
    # Ensure this commonly called property is updated
    set_prop(
      !!prop := purrr::map_chr(
        xpdb_list[indices],
        get_prop,
        prop,
        .problem = problem) %>%
        glue_cmd()
    )
}

# Numeric properties like etashk are already
# comma-separated, and have a specific format.
# Need to try to match without creating eyesore.
franken_numprop <- function(x) {
  purrr::map2_chr(seq_along(x), x, ~{
    if (grepl("\\[\\d+\\]",.y)) return(sprintf("(%i): %s", .x, .y))
    sprintf("%s [%i]", .y, .x)
  }) %>%
    # Semi-colon instead of comma, even if bracketed
    paste(collapse="; ")
}
