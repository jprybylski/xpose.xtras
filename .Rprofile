if (Sys.info()[["sysname"]] == "Darwin") {
  omp <- "/Library/Frameworks/R.framework/Resources/lib/libomp.dylib"
  if (file.exists(omp)) try(dyn.load(omp, local = FALSE), silent = TRUE)
}

source("renv/activate.R")
