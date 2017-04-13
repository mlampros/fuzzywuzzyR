
# https://github.com/rstudio/reticulate


FUZZ <- NULL; DIFFLIB <- NULL; EXTRACT <- NULL; UTILS <- NULL


.onLoad <- function(libname, pkgname) {

  if (reticulate::py_available(initialize = TRUE)) {

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

