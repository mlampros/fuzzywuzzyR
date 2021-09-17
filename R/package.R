

# https://github.com/rstudio/reticulate


FUZZ <- NULL; DIFFLIB <- NULL; EXTRACT <- NULL; UTILS <- NULL; BUILTINS <- NULL;


.onLoad <- function(libname, pkgname) {

  try({
    if (reticulate::py_available(initialize = FALSE)) {

      try({
        BUILTINS <<- reticulate::import_builtins(convert = FALSE)                      # 'buildins' are used in non-ascii languages (see issue https://github.com/mlampros/fuzzywuzzyR/issues/3) where the R-function accepts a python object as input [ convert = FALSE ]
      }, silent=TRUE)

      try({
        FUZZ <<- reticulate::import("fuzzywuzzy.fuzz", delay_load = TRUE)              # delay load foo module ( will only be loaded when accessed via $ )
      }, silent=TRUE)

      try({
        UTILS <<- reticulate::import("fuzzywuzzy.utils", delay_load = TRUE)
      }, silent=TRUE)

      try({
        DIFFLIB <<- reticulate::import("difflib", delay_load = TRUE)
      }, silent=TRUE)

      try({
        EXTRACT <<- reticulate::import("fuzzywuzzy.process", delay_load = TRUE)
      }, silent=TRUE)
    }
  }, silent=TRUE)
}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("If the 'fuzzywuzzyR' package gives the following error: 'attempt to apply non-function' then make sure to open a new R session and run 'reticulate::py_config()' before loading the package!")
}

