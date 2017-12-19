

# https://github.com/rstudio/reticulate


FUZZ <- NULL; DIFFLIB <- NULL; EXTRACT <- NULL; UTILS <- NULL; BUILTINS <- NULL;


.onLoad <- function(libname, pkgname) {

  if (reticulate::py_available(initialize = TRUE)) {
    
    BUILTINS <<- reticulate::import_builtins(convert = FALSE)                      # 'buildins' are used in non-ascii languages (see issue https://github.com/mlampros/fuzzywuzzyR/issues/3) where the R-function accepts a python object as input [ convert = FALSE ]

    if (reticulate::py_module_available("fuzzywuzzy")) {

      FUZZ <<- reticulate::import("fuzzywuzzy.fuzz", delay_load = TRUE)            # delay load foo module ( will only be loaded when accessed via $ )

      EXTRACT <<- reticulate::import("fuzzywuzzy.process", delay_load = TRUE)

      UTILS <<- reticulate::import("fuzzywuzzy.utils", delay_load = TRUE)
    }

    if (reticulate::py_module_available("difflib")) {

      DIFFLIB <<- reticulate::import("difflib", delay_load = TRUE)
    }
  }
}

