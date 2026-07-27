register_print_xpose_plot <- function(...) {
  registerS3method("print", "xpose_plot", print_xpose_plot_impl, envir = asNamespace("xpose.xtras"))
}

.onLoad <- function(...) {
  ## print.xpose_plot is deliberately *not* declared as an exported S3
  ## method in our own NAMESPACE (see the comment above
  ## print_xpose_plot_impl() in R/fixes.R for why): both xpose and
  ## xpose.xtras provide a print.xpose_plot, and if both declare it via
  ## NAMESPACE (S3method()), R prints a "Registered S3 method overwritten"
  ## startup message the moment the second one loads, regardless of load
  ## order (#72). Registering it here with registerS3method() updates the
  ## same underlying dispatch table silently.
  ##
  ## Imports are *usually* resolved before a package's own .onLoad() runs,
  ## so xpose's namespace (and its own, harmless, first-to-register
  ## print.xpose_plot) is normally already loaded by this point -- but that
  ## ordering isn't guaranteed (e.g. it can flip when 'conflicted' is loaded
  ## first), so also re-assert on xpose's own onLoad/attach events, same as
  ## the conflicted-preference hooks below, to be sure we always win.
  register_print_xpose_plot()
  setHook(packageEvent("xpose", "onLoad"), function(...) register_print_xpose_plot())
  setHook(packageEvent("xpose", "attach"), function(...) register_print_xpose_plot())
}

.onAttach <- function(...) {
  ## If (and only if) the user has 'conflicted' loaded, register our
  ## preferred side of the bugfix functions we deliberately override from
  ## xpose/stats. 'conflicted' is an optional Suggests dependency -- it only
  ## resolves conflicts among *currently attached* packages, and
  ## (re-)attaching conflicted itself resets what it has learned so far. Since
  ## xpose, conflicted and xpose.xtras can be attached in any order, a single
  ## one-shot call here is not enough (see #39) -- redo it whenever xpose or
  ## conflicted (re)attach/(re)load, in addition to doing it now.
  ##
  ## This only covers conflicts against xpose/stats (the packages we
  ## deliberately patch); conflicts against unrelated third-party packages
  ## (e.g. another package also exporting a dplyr verb) are the user's own
  ## responsibility to resolve, e.g. via conflicted::conflicts_prefer() (#72).
  set_conflict_prefs <- function(...) {
    if (is_loading_for_tests()) return(invisible())
    if (!has_conflicted()) return(invisible())
    conflicted::conflict_prefer_all("xpose.xtras", c("xpose","stats"), quiet=TRUE)
  }
  set_conflict_prefs()
  setHook(packageEvent("xpose", "onLoad"),      function(...) set_conflict_prefs())
  setHook(packageEvent("xpose", "attach"),      function(...) set_conflict_prefs())
  setHook(packageEvent("conflicted", "onLoad"), function(...) set_conflict_prefs())
  setHook(packageEvent("conflicted", "attach"), function(...) set_conflict_prefs())

  ## xpose.xtras only adds to xpose; it doesn't re-export xpose's own
  ## functions (dv_vs_ipred() and friends), so if xpose isn't attached those
  ## simply aren't available yet. Worth a short reminder every time that's
  ## the case. Separately, plain masked functions (unlike print.xpose_plot
  ## above, which is fixed for good via .onLoad()) do depend on load order:
  ## whichever of xpose/xpose.xtras attaches last wins the name on the
  ## search path, so mention any bugfix currently at stake too, when
  ## conflicted isn't loaded to paper over it.
  if (!is_attached("xpose")) {
    bullets <- c("i" = "{.pkg xpose} isn't attached; its own functions (e.g. {.fn dv_vs_ipred}) won't be available until you load it.")
    fixes <- active_bugfixes()
    if (length(fixes) > 0 && !has_conflicted()) {
      bullets <- c(bullets, " " = "Load it {.strong before} {.pkg xpose.xtras} (or load {.pkg conflicted}) so its {.fn {fixes}} bugfix{?es} take{?s/} effect too.")
    }
    cli::cli_inform(bullets, class = "packageStartupMessage")
  }
}

is_loading_for_tests <- function() {
  !rlang::is_interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "xpose.xtras")
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

# 'conflicted' is Suggests-only: only register preferences with it if the
# user has actually loaded it themselves, rather than pulling it in just
# because it happens to be installed (#72).
has_conflicted <- function() {
  isNamespaceLoaded("conflicted")
}

# Bugfix functions (masked, plain-function overrides of xpose/stats) that
# are still actually a fix for the installed xpose version -- i.e. still
# order-dependent without conflicted's help. Some fixes are only needed for
# a range of xpose versions and can be gated on utils::packageVersion("xpose")
# here as they're added/retired; irep() isn't currently gated since xpose
# fixed it upstream around 0.5.0, then reverted that fix, so there's no
# known-safe version range left to defer to xpose::irep() for.
active_bugfixes <- function() {
  c("irep")
}


# Remove CRAN note on no visible binding for global variable
utils::globalVariables(c(
  '.',
  ".result",
  "problem",
  "subprob",
  "method",
  ".env",
  ".data",
  "variable",
  "rn",
  "this_lvls",
  "value",
  "old_name",
  "everything",
  "where",
  "index",
  "extension",
  "data",
  ":=",
  "type",
  "xpdb_set",
  "na.omit",
  "label",
  "ofv",
  "..ofv",
  "mod1",
  "mod2",
  "m1col",
  "m2col",
  "focus",
  "param",
  "omega",
  "thnums",
  "cv",
  "m",
  "n",
  "pdf",
  "dev.off",
  "mods",
  "fixed",
  "across",
  "model",
  "extension",
  "modifyList",
  "sd",
  "value",
  "variable",
  "value_weight",
  "id_order",
  "probs",
  "xpdb",
  "dOFV",
  "total_dOFV",
  "nind",
  "OFV",
  "parent",
  "nn",
  "grp_key",
  "new_variable",
  "%RSE",
  "SE",
  "diagonal",
  "est",
  "eta",
  "etatrans",
  "fix",
  "formula",
  "ignore",
  "n",
  "x",
  "name",
  "neta1",
  "neta2",
  "ntheta",
  "plogis",
  "prm_assoc_formula",
  "probit",
  "probitInv",
  "qlogis",
  "rse",
  "se",
  "theta",
  "thetatrans",
  "xpdb_x",
  "n.x",
  "...threshold...",
  "FDR",
  "FN",
  "FNR",
  "FOR",
  "FP",
  "FPR",
  "LRn",
  "LRp",
  "MK",
  "N",
  "NPV",
  "P",
  "PPV",
  "TN",
  "TNR",
  "TP",
  "TPR",
  "TS",
  "confmatr",
  "head",
  "threshold",
  # rxode2/nlmixr2 model DSL symbols used in .nlmixr_example_* helpers
  "ini", "eta.ka", "eta.cl", "eta.v",
  "eta.ktr", "eta.emax", "eta.ec50", "eta.kout", "eta.e0",
  "depot", "cent", "center", "gut", "effect",
  "LLOQ", "CENS",
  "PAR1", "PAR2",
  # covariate association / forest plot (xtra_pars.R, covariates.R)
  "covariate", "covtype", "is_ref", "ci_low", "ci_high",
  "level", "row_label", "draws"
))
