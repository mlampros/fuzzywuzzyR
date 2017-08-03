

FUZZ <- NULL; DIFFLIB <- NULL; EXTRACT <- NULL; UTILS <- NULL


.onLoad <- function(libname, pkgname) {

  if (reticulate::py_available(initialize = TRUE)) {

    FUZZ <<- reticulate::import("fuzzywuzzy.fuzz", delay_load =  list(

      on_error = function(e) { stop("the 'fuzzywuzzy' module is not available", call. = F) }

      )
    )

    EXTRACT <<- reticulate::import("fuzzywuzzy.process", delay_load = list(

      on_error = function(e) { stop("the 'fuzzywuzzy' module is not available", call. = F) }
      )
    )

    UTILS <<- reticulate::import("fuzzywuzzy.utils", delay_load = list(

      on_error = function(e) { stop("the 'fuzzywuzzy' module is not available", call. = F) }
      )
    )

    DIFFLIB <<- reticulate::import("difflib", delay_load = list(

      on_error = function(e) { stop("the 'difflib' module is not available", call. = F) }
      )
    )
  }
}

