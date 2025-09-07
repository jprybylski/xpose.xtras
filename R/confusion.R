
#' Calculate all binary confusion matrix indices
#'
#' @param test_vec <`double`> Vector of probabilities
#' @param true_vec <`integer`> Vector true values
#' @param ... For future extension
#' @param threshold <`double`> Number that defines probability as positive.
#' @param pos_val <`integer`> Positive value in `true_vec`
#' @param prepend <`character`> Prepend column names with this to prevent name conflicts
#'
#' @returns dataframe of confusion matrix indices
#' @keywords internal
confmatr_by_threshold <- function(
    test_vec,
    true_vec,
    ...,
    threshold=0.5,
    pos_val=1,
    prepend = "",
    cols = dplyr::everything()) {
  checkmate::assert_numeric(test_vec)
  checkmate::assert_integerish(true_vec)
  checkmate::assert_numeric(threshold)
  checkmate::assert_count(pos_val)
  checkmate::assert_string(prepend)
  dplyr::bind_rows(
    purrr::map(threshold, function(.x) {
      dplyr::tibble(
        threshold = .x,
        P = sum(true_vec==pos_val),
        TP = sum(test_vec>=.x & true_vec==pos_val),
        FN = sum(test_vec<.x & true_vec==pos_val),
        N = sum(true_vec!=pos_val),
        FP = sum(test_vec>=.x & true_vec!=pos_val),
        TN = sum(test_vec<.x & true_vec!=pos_val)
      )
    })
  ) %>%
    dplyr::mutate(
      TPR = TP/P,
      SEN = TPR,
      FNR = 1 - TPR,
      FPR = FP/N,
      TNR = 1-FPR,
      SPC = TNR,
      BM = TPR+TNR-1,
      PT = (sqrt(TPR*FPR) - FPR)/(TPR - FPR),
      Prevalence = P/(P+N),
      PPV = TP/(TP + FP),
      NPV = TN/(TN+FN),
      LRp=TPR/FPR,
      LRn=FNR/TNR,
      ACC = (TP+TN)/(P+N),
      FDR = 1 - PPV,
      FOR = 1 - NPV,
      MK = PPV + NPV - 1,
      deltaP = MK,
      DOR = LRp/LRn,
      BA = (TPR + TNR)/2,
      F_1 = (2*PPV*TPR)/(PPV + TPR),
      FM = sqrt(PPV*TPR),
      MCC = sqrt(TPR*TNR*PPV*NPV) - sqrt(FNR*FPR*FOR*FDR),
      TS = TP/(TP+FN+FP),
      CSI = TS
    ) %>%
    dplyr::select({{ cols }}) %>%
    dplyr::rename_with(~paste0(prepend,.x))
}

# pre-planned plots

#' ROC Plot for categorical DVs
#'
#' @inheritParams catdv_vs_dvprobs
#' @param group Variable by which to group points or text
#' @param guide Include unity line?
#'
#'
#' @inherit xplot_rocplot details
#'
#' @returns A desired plot
#' @export
#'
#' @seealso [catdv_vs_dvprobs()]
#'
#' @examples
#' # Note these examples are similar to catdv_vs_dvprobs
#'
#' \dontrun{
#' # Test M3 model
#' pkpd_m3 %>%
#'   # Need to ensure var types are set
#'   set_var_types(catdv=BLQ,dvprobs=LIKE) %>%
#'   # Set probs
#'   set_dv_probs(1, 1~LIKE, .dv_var = BLQ) %>%
#'   # Optional, but useful to set levels
#'   set_var_levels(1, BLQ = lvl_bin()) %>%
#'   # Generate typical ROC curve
#'   roc_plot()
#'
#' # Test categorical model
#' vismo_xpdb <- vismo_pomod  %>%
#'   set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
#'   set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23)
#'
#' # Various cutpoints (note axes labels and texts)
#' vismo_xpdb %>%
#'   roc_plot(type = "p") # space plot
#' vismo_xpdb %>%
#'   roc_plot(cutpoint=2, type = "cak") # with area and key point
#' vismo_xpdb %>%
#'   roc_plot(cutpoint=3, type = "cak")
#'
#' # alternative model example
#' vismo_xpdb2  <- vismo_dtmm   %>%
#'   set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
#'   set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23)
#' vismo_xpdb2 %>%
#'   roc_plot(cutpoint=2, type = "cak")
#' }
#'
roc_plot <- function(xpdb,
                        mapping  = NULL,
                        cutpoint = 1,
                        group    = 'ID',
                        type     = 'ca',
                        title    = 'ROC curve @dvcol~@probcol | @run',
                        subtitle = 'Ofv: @ofv, Eps shrink: @epsshk',
                        caption  = '@dir',
                        tag      = NULL,
                        guide    = TRUE,
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
  dvprob_cols <- xp_var(xpdb, .problem, type = 'dvprobs')$col
  catdv_cols <- xp_var(xpdb, .problem, type = 'catdv')$col
  if (length(catdv_cols)>1) {
    cli::cli_warn("Only one categorical DV will be used ({catdv_cols[1]}).")
    catdv_cols <- catdv_cols[1]
  }

  cp <- make_catdv_cutpoint(xpdb, .problem, catdv_cols, cutpoint)
  # Use catdv_vs function but with a slight modification
  post_processing <- function (df) {
    cp$post_process(df) %>%
      dplyr::mutate(!!catdv_cols := as.numeric(.data[[catdv_cols]])-1)
  }


  ## For some reason the summary data for the xpdb is getting lost
  ## Not @dir, but @ofv and @epsshk
  xplot_rocplot(
    xpdb = xpdb, group = group, quiet = quiet,
    opt = xpose::data_opt(
      .problem = .problem,
      filter = xpose::only_obs(xpdb, .problem, quiet),
      post_processing = post_processing
    ),
    mapping = mapping,
    type = type, facets = facets,
    title = stringr::str_replace_all(title, c(
      "@dvcol" = catdv_cols,
      "@probcol" = cp$prob_col
    )),
    subtitle = subtitle, caption = caption,
    tag = tag, plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    like_col = cp$prob_col,
    obs_col = catdv_cols,
    obs_target = 1,
    ...
  )
}

#' Individual ROC plots
#'
#' @description
#' To identify any individual likelihood predictions that may
#' be more influential or unusual.
#'
#' Note this function may have a long runtime.
#'
#' @inheritParams  roc_plot
#'
#' @inherit xplot_rocplot details
#'
#' @returns The desired plot
#' @export
#'
#' @examples
#' \dontrun{
#' vismo_pomod  %>%
#'   set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
#'   set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23) %>%
#'   ind_roc()
#' }
ind_roc <- function(xpdb,
                    mapping = NULL,
                    cutpoint = 1,
                    type = "ca",
                    title = "Individual ROC curves | @run",
                    subtitle = "Ofv: @ofv, Eps shrink: @epsshk",
                    caption = "@dir | Page @page of @lastpage",
                    tag = NULL,
                    facets,
                    .problem,
                    quiet,
                    ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = "data")
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (missing(facets)) {
    facets <- xpose::add_facet_var(
      facets = xpdb$xp_theme$facets,
      variable = xp_var(xpdb, .problem, type = "id")$col
    )
  }

  extra_args <- list(...)
  if (!any(names(extra_args) == "nrow")) extra_args$nrow <- 3
  if (!any(names(extra_args) == "ncol")) extra_args$ncol <- 3

  # Get relevant columns (or throw error)
  dvprob_cols <- xp_var(xpdb, .problem, type = "dvprobs")$col
  catdv_cols <- xp_var(xpdb, .problem, type = "catdv")$col
  if (length(catdv_cols) > 1) {
    cli::cli_warn("Only one categorical DV will be used ({catdv_cols[1]}).")
    catdv_cols <- catdv_cols[1]
  }



  cp <- make_catdv_cutpoint(xpdb, .problem, catdv_cols, cutpoint)
  # Use catdv_vs function but with a slight modification
  post_processing <- function(df) {
    cp$post_process(df) %>%
      dplyr::mutate(!!catdv_cols := as.numeric(.data[[catdv_cols]]) - 1)
  }


  rlang::exec(xplot_rocplot,
    !!!extra_args,
    xpdb = xpdb,
    group = NULL,
    quiet = quiet,
    opt = xpose::data_opt(
      .problem = .problem,
      filter = xpose::only_obs(xpdb, .problem, quiet),
      post_processing = post_processing
    ),
    mapping = mapping,
    type = type,
    facets = facets,
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    like_col = cp$prob_col,
    obs_col = catdv_cols,
    obs_target = 1
  )
}

#' ROC curve across models
#'
#' Faceted display of ROC curves across models in a set.
#'
#' @inheritParams xplot_rocplot
#' @inheritParams iofv_vs_mod
#' @param cutpoint <`numeric`> Of defined probabilities, which one to
#' use in plots.
#' @param roc_args Additional arguments to pass to [xplot_rocplot()]
#'
#' @export
#'
#' @inherit xplot_rocplot details
#'
#' @examples
#'
#' pkpd_m3 <- pkpd_m3 %>%
#'   # Need to ensure var types are set
#'   set_var_types(catdv=BLQ,dvprobs=LIKE) %>%
#'   # Set probs
#'   set_dv_probs(1, 1~LIKE, .dv_var = BLQ) %>%
#'   # Optional, but useful to set levels
#'   set_var_levels(1, BLQ = lvl_bin())
#'
#' m3_set <- xpose_set(
#'   run1=set_prop(pkpd_m3,run="run1"),
#'   run2=set_prop(pkpd_m3,run="run2"),
#'   run3=set_prop(pkpd_m3,run="run3")
#' )
#'
#' roc_by_mod(m3_set, type = "ck", quiet = TRUE)
#'
roc_by_mod <- function(
    xpdb_s,
    ...,
    .lineage = FALSE,
    mapping  = NULL,
    cutpoint = 1,
    type     = 'ca',
    title    = 'ROC curves across models | @dvcol~@probcol',
    subtitle = 'Based on @nind individuals, Ofvs: @ofv',
    caption  = '@dir',
    tag      = NULL,
    axis.text = "@run",
    facets,
    .problem,
    quiet,
    roc_args = NULL
) {
  check_xpose_set(xpdb_s)

  # Make sure dots are unnamed
  rlang::check_dots_unnamed()

  n_set_dots(xpdb_s, ..., .lineage=.lineage) # makes `mods`

  pre_process <- function(x) unfocus_xpdb(x)
  xpose_subset <- xpdb_s %>% pre_process() %>% select(!!mods)

  # extra checks
  if (missing(.problem))
    .problem <- xpose::default_plot_problem(xpose_subset[[1]]$xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet))
    quiet <- xpose_subset[[1]]$xpdb$options$quiet



  xpdb_l <- purrr::map(xpose_subset, ~.x$xpdb)
  # Make sure each has the needed columns
  dvprob_cols <- purrr::map(
    xpdb_l, ~ xpa("character",
      xp_var(.x, .problem, type = "dvprobs")$col,
      "Each `xpose_data` should only have at least 1 `dvprobs` column"
    )
  ) %>%
    unname() %>%
    unique() %>%
    Reduce(intersect, .) %>%
    xpa("character", ., "Need common `dvprobs` in all models.")
  catdv_col <- purrr::imap(
    xpdb_l, ~ {
      cols <- xp_var(.x, .problem, type = "catdv")$col
      xpa("character", cols, "Each `xpose_data` needs a `catdv` column.")
      # if (length(cols) > 1) {
      #   cli::cli_warn("Only one categorical DV will be used for `{.y}`: `{cols[1]}`.")
      #   cols <- cols[1]
      # }
      cols
    }
  ) %>%
    unname() %>%
    unique() %>%
    Reduce(intersect, .) %>%
    xpa("string", ., "Need the same, singular `catdv_col` in all models.")

  nicer_labs <- purrr::map_chr(xpdb_l,~xpose::parse_title(axis.text,.x, .problem))
  if (any(duplicated(nicer_labs))) {
    cli::cli_alert_warning("Duplicate values for default {.code axis.text}. Making result unique.")
    if (any(purrr::map_lgl(xpose_subset, ~ xpose::software(.x$xpdb)=="nlmixr2")))
      cli::cli_alert_info("For {.strong nlmixr2} models, sometimes '@file' is a better {.code axis.text}")
    nicer_labs <- make.unique(nicer_labs)
  }
  nicer_labs <- nlmixr2_duplicate_axis_text_helper(nicer_labs, xpose_subset, axis.text)

  # Get combined xpdb
  xpdb_f <- franken_xpdb(
    !!!xpdb_l,
    .cols = dplyr::all_of(c(catdv_col, dvprob_cols)),
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

  # Pivot longer post_processing
  if (missing(facets)) {
    facets <- xpose::add_facet_var(
      facets = xpdb_f$xp_theme$facets,
      variable = "model"
    )
  }
  cp <- make_catdv_cutpoint(xpdb_l[[1]], .problem, catdv_col, cutpoint)
  # Use catdv_vs function but with modification
  # This assumes same CATDV~DVPROBS in each problem, but assuming otherwise may not be worth doing
  # Will have to note in the documentation that this is for comparing models that meet that assumption
  post_processing <- function(df) {
    # First, drop index 1 cardv and dvprob columns
    df %>%
      dplyr::select(
        -dplyr::all_of(c(catdv_col, dvprob_cols))
      ) %>%
      # pivot longer to get 1 row per each separated by model number
      tidyr::pivot_longer(
        cols = dplyr::starts_with(paste0(c(catdv_col, dvprob_cols),"_")),
        names_to = c(".value","model"),
        names_sep = "_"
      ) %>%
      # normal catdv post-processing
      cp$post_process() %>%
      dplyr::mutate(!!catdv_col := as.numeric(.data[[catdv_col]]) - 1) %>%
      # use axis.text label
      dplyr::mutate(
        model = nicer_labs[as.numeric(model)] %>%
          factor(levels = nicer_labs)
      )
  }



  rlang::exec(xplot_rocplot,
              !!!roc_args,
              xpdb = xpdb_f,
              group = NULL,
              quiet = quiet,
              opt = xpose::data_opt(
                .problem = .problem,
                filter = xpose::only_obs(xpdb_f, .problem, quiet),
                post_processing = post_processing
              ),
              mapping = mapping,
              type = type,
              facets = facets,
              title = stringr::str_replace_all(title, c(
                "@dvcol" = catdv_col,
                "@probcol" = cp$prob_col
              )),
              subtitle = subtitle,
              caption = caption,
              tag = tag,
              plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
              like_col = cp$prob_col,
              obs_col = catdv_col,
              obs_target = 1
  )
}

### Other confusion matrix ideas
### Confusion indexes as new var type
### Confusion index versus cov or eta
