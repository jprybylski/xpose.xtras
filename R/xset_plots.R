

# This is specific enough to not need a generic
shark_plot <- function(
    xpdb_s,
    ...,
    .inorder=FALSE,
    type = "pclt",
    alpha = 0.05,
    df = "guess",
    title    = 'Eta covariate correlations | @run',
    subtitle = 'Based on @nind individuals, Eta shrink: @etashk',
    caption  = '@dir',
    tag      = NULL,
    opt,
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
  iofv1 <- xp_var(mod1$xpdb, .problem = .problem, type = "iofv")$col
  iofv2 <- xp_var(mod2$xpdb, .problem = .problem, type = "iofv")$col

  # Get combined xpdb
  xpdb_f <- franken_xpdb(
    mod1$xpdb,
    mod2$xpdb,
    .types = "iofv",
    problem = .problem,
    prop_transforms = function(xpdb_f, xpdb_list, problem) {
      franken_prop(
        xpdb_f = xpdb_f,
        xpdb_list = xpdb_list,
        prop = "ofv",
        problem = problem,
        glue_cmd = franken_numprop)
    }
  )

  # Data options
  ofv_columns <- paste0(c(iofv1,iofv2),"_",1:2)
  # TODO: mod1ofv should actually be sum of iOFVs in mod1 to be more dynamic and allow faceting
  mod1ofv <- get_prop(mod1$xpdb, "ofv", .problem = .problem) %>% as.numeric()
  post_processing <- function(df) {
    df %>%
      dplyr::mutate(
        dOFV = .data[[ofv_columns[2]]] - .data[[ofv_columns[1]]]
      ) %>%
      dplyr::arrange(-abs(dOFV)) %>%
      dplyr::group_by(dOFV>0) %>%
      dplyr::mutate(
        nind = 1:dplyr::n(),
        OFV = mod1ofv - cumsum(dOFV)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        nind = ifelse(dOFV==0, 0, nind),
        OFV = ifelse(dOFV==0, mod1ofv, OFV)
      )
  }
  def_opt <- xpose::data_opt(.problem = .problem, .subprob = .subprob, .method = .method,
                             filter = xpose::only_distinct(xpdb_f, .problem, NULL, quiet),
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

  pdofv <- data %>% dplyr::filter(dOFV>=0)
  ndofv <- data %>% dplyr::filter(dOFV<=0)

  # Significance of dOFV
  if (df == "guess") {
    prm1 <- xpose::get_prm(mod1$xpdb, .problem = .problem, .subprob = .subprob, .method = .method, quiet = quiet)
    prm2 <- xpose::get_prm(mod2$xpdb, .problem = .problem, .subprob = .subprob, .method = .method, quiet = quiet)

    nfit1 <- sum(!prm1$fixed)
    nfit2 <- sum(!prm2$fixed)

    df <- nfit2 - nfit1
  }
  df <- rlang::try_fetch(as.numeric(df), warning=function(s) {
    rlang::warn("df can only be 'guess' or an integer greater than 0.")
    1
  })
  if (df<1) {
    cli::cli_warn(paste("Guessing df merely uses the difference in unfixed parameters.",
                        "For these models, that is {df}. Changed to 1."))
    df <- 1
  }
  sigOFV <- mod1ofv + stats::qchisq(1-alpha, df)

  # Check type
  allow_types <-  c("p","c","l","t")
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
    xp <- xp + xpose::xp_geoms(mapping  = NULL,
                               xp_theme = xpdb_f$xp_theme,
                               name     = "hline",
                               ggfun    = "geom_hline",
                               hline_yintercept = mod1ofv
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
                               name     = "point",
                               ggfun    = "geom_point",
                               point_data = pdofv,
                               point_color = "blue"
                               )
    xp <- xp + xpose::xp_geoms(mapping  = NULL,
                               xp_theme = xpdb_f$xp_theme,
                               name     = "point",
                               ggfun    = "geom_point",
                               point_data = ndofv,
                               point_color = "red"
                               )
  }


  xpose::as.xpose.plot(xp)


}
dofv_vs_id <- function() {shark_plot()} # < alias to match xpose4

# boxplot (etc) of all iOFVs in all models for a set
# ... is either models in set, child(ren) of parent formula, or empty (all models).
# if .lineage=TRUE, then ... is interpreted with xset_lineage
iofv_vs_mod <- function(xpdb_s, ..., .lineage = FALSE) {}

# There may need to be a waterfall generic: xset_waterfall
prm_waterfall <- function() {}
eta_waterfall <- function() {}
iofv_waterfall <- function() {}

# These would just create a new xpdb in situ, then mutate, and then use xplot_scatter
ipred_vs_ipred <- function() {}
prm_vs_prm <- function() {}
eta_vs_eta <- function() {}

# This would also just be an in situ xpdb, but there may need to be a function
# for model-averaging
ipred_vs_idv_modavg <- function() {}
pred_vs_idv_modavg <- function() {}







########
# Helper functions
########


#' Typical processing for plots of 2 sets
#'
#' @param xpdb_s <`xpose_set`> object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Passed to <[`select_subset`>
#' @param .inorder <`logical`> Regardless of base model or parentage, use the
#' two plots in order of how they are in arguments. First plot listed is treated
#' as base or parent.
#' @param envir Where to assign `mod1` and `mod2` <`xpose_set_item`>s
#'
#' @return Into `envir`, `mod1` and `mod2`
#'
two_set_dots <- function(xpdb_s, ..., .inorder=FALSE, envir = parent.frame()) {
  check_xpose_set(xpdb_s, .warn = FALSE)

  if (length(xpdb_s)<2) {
    cli::cli_abort("Need at least two models in set.")
  }

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
  } else if (rlang::dots_n(...)!=2) {
    cli::cli_abort("Need to select exactly two models in set.")
  } else if (rlang::dots_n(...)==2) {
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
  if (rlang::dots_n(...)<2) {
    cli::cli_abort("Need at least two `xpose_data` or `xp_xtra` objects.")
  }

  if (is.null(.types) && rlang::quo_is_null(.cols))
    rlang::abort("Need `.cols` and/or `.types` to add into new object.")

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

    for (prob in problem) {
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
      this_data <- xpose::get_data(db, .problem = problem, quiet=quiet)

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
        dplyr::select(this_data, {{.cols}}, !!tcols),
        error = function(s) {
          rlang::abort(
            paste("Error with `xpose_data` for prob no.",prob,"in",get_prop(db, 'run')),
            parent = s
          )
        })

      # Fill data
      prob_data[[prob]][[index]] <- cols_set

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
      xpose::mutate(!!!new_data_list)
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
#'
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
#'
#' @examples
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
#'
franken_prop <- function(
    xpdb_f,
    xpdb_list,
    prop,
    problem=NULL,
    glue_cmd = function(x) glue::glue_collapse(x, ", ", last = " and ")
) {
  xpdb_f %>%
    # Ensure this commonly called property is updated
    set_prop(
      !!prop := purrr::map_chr(
        xpdb_list,
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
