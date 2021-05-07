

#' This function checks if all relevant python modules are available
#'
#' @export

check_availability = function() {

  builtins_av = fuzz = util_av = dif_av = extr_av = NULL

  try({
    builtins_av = reticulate::import_builtins(convert = FALSE)                      # 'buildins' are used in non-ascii languages (see issue https://github.com/mlampros/fuzzywuzzyR/issues/3) where the R-function accepts a python object as input [ convert = FALSE ]
  }, silent=TRUE)

  try({
    fuzz = reticulate::import("fuzzywuzzy.fuzz", delay_load = TRUE)              # delay load foo module ( will only be loaded when accessed via $ )
  }, silent=TRUE)

  try({
    util_av = reticulate::import("fuzzywuzzy.utils", delay_load = TRUE)
  }, silent=TRUE)

  try({
    dif_av = reticulate::import("difflib", delay_load = TRUE)
  }, silent=TRUE)

  try({
    extr_av = reticulate::import("fuzzywuzzy.process", delay_load = TRUE)
  }, silent=TRUE)


  if (any(c(is.null(builtins_av), is.null(fuzz), is.null(util_av), is.null(dif_av), is.null(extr_av)))) {
    FALSE
  }
  else {
    TRUE
  }
}




#' This function returns TRUE if python2 is installed and used in the OS
#'
#' @keywords internal

is_python2 = function() {

  vers = NULL

  try({
    vers <- reticulate::py_config()
  }, silent=TRUE)

  if (!is.null(vers)) {
    vers = (as.numeric(vers$version) < 3)
  }

  return(vers)
}




#' Character string sequence matching
#'
#'
#' @param string1 a character string.
#' @param string2 a character string.
#' @export
#' @details
#'
#' the \emph{ratio} method returns a measure of the sequences' similarity as a float in the range [0, 1]. Where T is the total number of elements in both sequences, and M is the number of matches, this
#' is 2.0*M / T. Note that this is 1.0 if the sequences are identical, and 0.0 if they have nothing in common. This is expensive to compute if getMatchingBlocks() or getOpcodes() hasn’t already been called,
#' in which case you may want to try quickRatio() or realQuickRatio() first to get an upper bound.
#'
#' the \emph{quick_ratio} method returns an upper bound on ratio() relatively quickly.
#'
#' the \emph{real_quick_ratio} method returns an upper bound on ratio() very quickly.
#'
#' the \emph{get_matching_blocks} method returns a list of triples describing matching subsequences. Each triple is of the form [i, j, n], and means that a[i:i+n] == b[j:j+n]. The triples are monotonically
#' increasing in i and j. The last triple is a dummy, and has the value [a.length, b.length, 0]. It is the only triple with n == 0. If [i, j, n] and [i', j', n'] are adjacent triples in the list, and the second
#' is not the last triple in the list, then i+n != i' or j+n != j'; in other words, adjacent triples always describe non-adjacent equal blocks.
#'
#' The \emph{get_opcodes} method returns a list of 5-tuples describing how to turn a into b. Each tuple is of the form [tag, i1, i2, j1, j2]. The first tuple has i1 == j1 == 0, and remaining tuples have i1 equal to
#' the i2 from the preceding tuple, and, likewise, j1 equal to the previous j2. The tag values are strings, with these meanings:
#' 'replace'   a[i1:i2] should be replaced by b[j1:j2].
#' 'delete'    a[i1:i2] should be deleted. Note that j1 == j2 in this case.
#' 'insert'    b[j1:j2] should be inserted at a[i1:i1]. Note that i1 == i2 in this case.
#' 'equal'     a[i1:i2] == b[j1:j2] (the sub-sequences are equal).
#'
#' @references  https://www.npmjs.com/package/difflib, http://stackoverflow.com/questions/10383044/fuzzy-string-comparison
#' @docType class
#' @importFrom R6 R6Class
#' @import reticulate
#' @section Methods:
#'
#' \describe{
#'  \item{\code{SequenceMatcher$new(string1 = NULL, string2 = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{ratio()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{quick_ratio()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{real_quick_ratio()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{get_matching_blocks()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{get_opcodes()}}{}
#'  }
#'
#' @usage # init <- SequenceMatcher$new(string1 = NULL, string2 = NULL)
#' @examples
#'
#' try({
#'   if (reticulate::py_available(initialize = FALSE)) {
#'
#'     if (check_availability()) {
#'
#'       library(fuzzywuzzyR)
#'
#'       s1 = ' It was a dark and stormy night. I was all alone sitting on a red chair.'
#'
#'       s2 = ' It was a murky and stormy night. I was all alone sitting on a crimson chair.'
#'
#'       init = SequenceMatcher$new(string1 = s1, string2 = s2)
#'
#'       init$ratio()
#'
#'       init$quick_ratio()
#'
#'       init$real_quick_ratio()
#'
#'       init$get_matching_blocks()
#'
#'       init$get_opcodes()
#'
#'     }
#'   }
#' }, silent=TRUE)


SequenceMatcher <- R6::R6Class("SequenceMatcher",

                               lock_objects = FALSE,

                               public = list(

                                 initialize = function(string1 = NULL, string2 = NULL) {

                                   self$string1 <- string1

                                   self$string2 <- string2

                                   if (is.null(self$string1) || is.null(self$string2)) {

                                     stop("both parameters 'string1' and 'string2' should be non-NULL", call. = F)
                                   }

                                   if (!inherits(self$string1, c('character', 'vector')) || !inherits(self$string2, c('character', 'vector'))) {

                                     stop("both parameters 'string1' and 'string2' should be of type character string", call. = F)
                                   }

                                   private$tmp = DIFFLIB$SequenceMatcher(NULL, self$string1, self$string2)

                                 },

                                 ratio = function() {

                                   return(private$tmp$ratio())
                                 },

                                 quick_ratio = function() {

                                   return(private$tmp$quick_ratio())
                                 },

                                 real_quick_ratio = function() {

                                   return(private$tmp$real_quick_ratio())
                                 },

                                 get_matching_blocks = function() {

                                   return(private$tmp$get_matching_blocks())
                                 },

                                 get_opcodes = function() {

                                   return(private$tmp$get_opcodes())
                                 }
                               ),

                               private = list(

                                 tmp = NULL
                               )
)



#' Matches of character strings
#'
#' @param string a character string.
#' @param sequence_strings a vector of character strings.
#' @param n an integer value specifying the maximum number of close matches to return; n must be greater than 0.
#' @param cutoff a float number in the range [0, 1], \emph{sequence_strings} that don't score at least that similar to \emph{string} are ignored.
#' @details
#' Returns a list of the best "good enough" matches. \emph{string} is a sequence for which close matches are desired (typically a string), and \emph{sequence_strings} is a list of sequences against
#' which to match \emph{string} (typically a list of strings).
#' @export
#' @references https://www.npmjs.com/package/difflib, http://stackoverflow.com/questions/10383044/fuzzy-string-comparison
#' @examples
#'
#' try({
#'   if (reticulate::py_available(initialize = FALSE)) {
#'
#'     if (check_availability()) {
#'
#'       library(fuzzywuzzyR)
#'
#'       vec = c('Frodo Baggins', 'Tom Sawyer', 'Bilbo Baggin')
#'
#'       str1 = 'Fra Bagg'
#'
#'       GetCloseMatches(string = str1, sequence_strings = vec, n = 2L, cutoff = 0.6)
#'
#'     }
#'   }
#' }, silent=TRUE)



GetCloseMatches = function(string = NULL, sequence_strings = NULL, n = 3L, cutoff = 0.6) {

  if (is.null(string) || is.null(sequence_strings)) { stop("both parameters 'string' and 'sequence_strings' should be non-NULL", call. = F) }

  if (!inherits(string, c('character', 'vector')) || !inherits(sequence_strings, c('character', 'vector'))) {

    stop("both parameters 'string' and 'sequence_strings' should be of type character string", call. = F)
  }

  if (!inherits(n, c('numeric', 'integer'))) { stop("the 'n' parameter should be of type integer", call. = F) }

  if (!inherits(cutoff, c('double', 'numeric'))) { stop("the 'cutoff' parameter should be of type numeric", call. = F) }

  if (cutoff > 1.0 || cutoff <= 0.0) { stop("the 'cutoff' parameter should be greater than 0.0 and less than or equal to 1.0", call. = F) }

  n = as.integer(n)

  tmp = DIFFLIB$get_close_matches(string, sequence_strings, n, cutoff)

  return(tmp)
}



#' Fuzzy character string matching ( ratios )
#'
#'
#' @param decoding either NULL or a character string. If not NULL then the \emph{decoding} parameter takes one of the standard python encodings (such as 'utf-8'). See the \emph{details} and \emph{references} link for more information.
#' @param string1 a character string.
#' @param string2 a character string.
#' @param force_ascii allow only ASCII characters (force convert to ascii)
#' @param full_process either TRUE or FALSE. If TRUE then it process the string by : 1. removing all but letters and numbers, 2. trim whitespace, 3. force to lower case
#' @export
#' @details
#'
#' the \emph{decoding} parameter is useful in case of non-ascii character strings. If this parameter is not NULL then the \emph{force_ascii} parameter (if applicable) is internally set to FALSE. Decoding applies only to python 2 configurations, as in python 3 character strings are decoded to unicode by default.
#'
#' the \emph{Partial_token_set_ratio} method works in the following way : 1. Find all alphanumeric tokens in each string, 2. treat them as a set, 3. construct two strings of the form, <sorted_intersection><sorted_remainder>, 4. take ratios of those two strings, 5. controls for unordered partial matches (HERE partial match is TRUE)
#'
#' the \emph{Partial_token_sort_ratio} method returns the ratio of the most similar substring as a number between 0 and 100 but sorting the token before comparing.
#'
#' the \emph{Ratio} method returns a ration in form of an integer value based on a SequenceMatcher-like class, which is built on top of the Levenshtein package (https://github.com/miohtama/python-Levenshtein)
#'
#' the \emph{QRATIO} method performs a quick ratio comparison between two strings. Runs full_process from utils on both strings. Short circuits if either of the strings is empty after processing.
#'
#' the \emph{WRATIO} method returns a measure of the sequences' similarity between 0 and 100, using different algorithms. Steps in the order they occur :
#' 1. Run full_process from utils on both strings, 2. Short circuit if this makes either string empty, 3. Take the ratio of the two processed strings (fuzz.ratio),
#' 4. Run checks to compare the length of the strings (If one of the strings is more than 1.5 times as long as the other use partial_ratio comparisons - scale partial results by 0.9 - this makes sure only full results can return 100 -
#' If one of the strings is over 8 times as long as the other instead scale by 0.6), 5. Run the other ratio functions (if using partial ratio functions call partial_ratio,
#' partial_token_sort_ratio and partial_token_set_ratio scale all of these by the ratio based on length otherwise call token_sort_ratio and token_set_ratio all token based comparisons are scaled by 0.95 - on top of any partial scalars)
#' 6. Take the highest value from these results round it and return it as an integer.
#'
#' the \emph{UWRATIO} method returns a measure of the sequences' similarity between 0 and 100, using different algorithms. Same as WRatio but preserving unicode
#'
#' the \emph{UQRATIO} method returns a Unicode quick ratio. It calls \emph{QRATIO} with force_ascii set to FALSE.
#'
#' the \emph{Token_sort_ratio} method returns a measure of the sequences' similarity between 0 and 100 but sorting the token before comparing
#'
#' the \emph{Partial_ratio} returns the ratio of the most similar substring as a number between 0 and 100.
#'
#' the \emph{Token_set_ratio} method works in the following way : 1. Find all alphanumeric tokens in each string, 2. treat them as a set, 3. construct two strings of the form, <sorted_intersection><sorted_remainder>, 4. take ratios of those two strings, 5. controls for unordered partial matches (HERE partial match is FALSE)
#'
#' @references  https://github.com/seatgeek/fuzzywuzzy/blob/master/fuzzywuzzy/fuzz.py, https://docs.python.org/3/library/codecs.html#standard-encodings
#' @docType class
#' @importFrom R6 R6Class
#' @import reticulate
#' @section Methods:
#'
#' \describe{
#'  \item{\code{FuzzMatcher$new(decoding = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Partial_token_set_ratio(string1 = NULL, string2 = NULL, force_ascii = TRUE, full_process = TRUE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Partial_token_sort_ratio(string1 = NULL, string2 = NULL, force_ascii = TRUE, full_process = TRUE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Ratio(string1 = NULL, string2 = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{QRATIO(string1 = NULL, string2 = NULL, force_ascii = TRUE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{WRATIO(string1 = NULL, string2 = NULL, force_ascii = TRUE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{UWRATIO(string1 = NULL, string2 = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{UQRATIO(string1 = NULL, string2 = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Token_sort_ratio(string1 = NULL, string2 = NULL, force_ascii = TRUE, full_process = TRUE)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Partial_ratio(string1 = NULL, string2 = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Token_set_ratio(string1 = NULL, string2 = NULL, force_ascii = TRUE, full_process = TRUE)}}{}
#'  }
#'
#' @usage # init <- FuzzMatcher$new(decoding = NULL)
#' @examples
#'
#' try({
#'   if (reticulate::py_available(initialize = FALSE)) {
#'
#'     if (check_availability()) {
#'
#'       library(fuzzywuzzyR)
#'
#'       s1 = "Atlanta Falcons"
#'
#'       s2 = "New York Jets"
#'
#'       init = FuzzMatcher$new()
#'
#'       init$Partial_token_set_ratio(string1 = s1,
#'                                    string2 = s2,
#'                                    force_ascii = TRUE,
#'                                    full_process = TRUE)
#'
#'       init$Partial_token_sort_ratio(string1 = s1,
#'                                     string2 = s2,
#'                                     force_ascii = TRUE,
#'                                     full_process = TRUE)
#'
#'       init$Ratio(string1 = s1, string2 = s2)
#'
#'       init$QRATIO(string1 = s1, string2 = s2, force_ascii = TRUE)
#'
#'       init$WRATIO(string1 = s1, string2 = s2, force_ascii = TRUE)
#'
#'       init$UWRATIO(string1 = s1, string2 = s2)
#'
#'       init$UQRATIO(string1 = s1, string2 = s2)
#'
#'       init$Token_sort_ratio(string1 = s1, string2 = s2, force_ascii = TRUE, full_process = TRUE)
#'
#'       init$Partial_ratio(string1 = s1, string2 = s2)
#'
#'       init$Token_set_ratio(string1 = s1, string2 = s2, force_ascii = TRUE, full_process = TRUE)
#'     }
#'   }
#' }, silent=TRUE)



FuzzMatcher <- R6::R6Class("FuzzMatcher",

                             lock_objects = FALSE,

                             public = list(

                               initialize = function(decoding = NULL) {

                                 self$decoding <- decoding

                                 if (!is.null(self$decoding)) {

                                   if (!inherits(self$decoding, 'character')) {

                                     stop("the 'decoding' parameter can be either NULL or a character string", call. = F)
                                   }
                                 }
                               },

                               Partial_token_set_ratio = function(string1 = NULL, string2 = NULL, force_ascii = TRUE, full_process = TRUE) {

                                 if (is.null(string1) || is.null(string2)) { stop("both parameters 'string1' and 'string2' should be non-NULL", call. = F) }

                                 if (!inherits(string1, c('character', 'vector')) || !inherits(string2, c('character', 'vector'))) {

                                   stop("both parameters 'string1' and 'string2' should be of type character string", call. = F)
                                 }

                                 if (!is.logical(force_ascii)) stop("the 'force_ascii' parameter should be of type boolean", call. = F)

                                 if (!is.logical(full_process)) stop("the 'full_process' parameter should be of type boolean", call. = F)

                                 if (!is.null(self$decoding)) {

                                   py_vers = is_python2()

                                   if (!is.null(py_vers)) {

                                     if (py_vers) {

                                       string1 <- BUILTINS$str(string1)$decode(self$decoding)

                                       string2 <- BUILTINS$str(string2)$decode(self$decoding)

                                       force_ascii = FALSE                                       # in case of 'decoding != NULL' force_ascii = FALSE
                                     }
                                   }
                                 }

                                 return(FUZZ$partial_token_set_ratio(string1, string2, force_ascii, full_process))
                               },

                               Partial_token_sort_ratio = function(string1 = NULL, string2 = NULL, force_ascii = TRUE, full_process = TRUE) {

                                 if (is.null(string1) || is.null(string2)) { stop("both parameters 'string1' and 'string2' should be non-NULL", call. = F) }

                                 if (!inherits(string1, c('character', 'vector')) || !inherits(string2, c('character', 'vector'))) {

                                   stop("both parameters 'string1' and 'string2' should be of type character string", call. = F)
                                 }

                                 if (!is.logical(force_ascii)) stop("the 'force_ascii' parameter should be of type boolean", call. = F)

                                 if (!is.logical(full_process)) stop("the 'full_process' parameter should be of type boolean", call. = F)

                                 if (!is.null(self$decoding)) {

                                   py_vers = is_python2()

                                   if (!is.null(py_vers)) {

                                     if (py_vers) {

                                       string1 <- BUILTINS$str(string1)$decode(self$decoding)

                                       string2 <- BUILTINS$str(string2)$decode(self$decoding)

                                       force_ascii = FALSE                                       # in case of 'decoding != NULL' force_ascii = FALSE
                                     }
                                   }
                                 }

                                 return(FUZZ$partial_token_sort_ratio(string1, string2, force_ascii, full_process))
                               },

                               Ratio = function(string1 = NULL, string2 = NULL) {

                                 if (is.null(string1) || is.null(string2)) { stop("both parameters 'string1' and 'string2' should be non-NULL", call. = F) }

                                 if (!inherits(string1, c('character', 'vector')) || !inherits(string2, c('character', 'vector'))) {

                                   stop("both parameters 'string1' and 'string2' should be of type character string", call. = F)
                                 }

                                 if (!is.null(self$decoding)) {

                                   py_vers = is_python2()

                                   if (!is.null(py_vers)) {

                                     if (py_vers) {

                                       string1 <- BUILTINS$str(string1)$decode(self$decoding)

                                       string2 <- BUILTINS$str(string2)$decode(self$decoding)
                                     }
                                   }
                                 }

                                 return(FUZZ$ratio(string1, string2))
                               },

                               QRATIO = function(string1 = NULL, string2 = NULL, force_ascii = TRUE) {

                                 if (is.null(string1) || is.null(string2)) { stop("both parameters 'string1' and 'string2' should be non-NULL", call. = F) }

                                 if (!inherits(string1, c('character', 'vector')) || !inherits(string2, c('character', 'vector'))) {

                                   stop("both parameters 'string1' and 'string2' should be of type character string", call. = F)
                                 }

                                 if (!is.logical(force_ascii)) stop("the 'force_ascii' parameter should be of type boolean", call. = F)

                                 if (!is.null(self$decoding)) {

                                   py_vers = is_python2()

                                   if (!is.null(py_vers)) {

                                     if (py_vers) {

                                       string1 <- BUILTINS$str(string1)$decode(self$decoding)

                                       string2 <- BUILTINS$str(string2)$decode(self$decoding)

                                       force_ascii = FALSE                                       # in case of 'decoding != NULL' force_ascii = FALSE
                                     }
                                   }
                                 }

                                 return(FUZZ$QRatio(string1, string2, force_ascii))
                               },

                               WRATIO = function(string1 = NULL, string2 = NULL, force_ascii = TRUE) {

                                 if (is.null(string1) || is.null(string2)) { stop("both parameters 'string1' and 'string2' should be non-NULL", call. = F) }

                                 if (!inherits(string1, c('character', 'vector')) || !inherits(string2, c('character', 'vector'))) {

                                   stop("both parameters 'string1' and 'string2' should be of type character string", call. = F)
                                 }

                                 if (!is.logical(force_ascii)) stop("the 'force_ascii' parameter should be of type boolean", call. = F)

                                 if (!is.null(self$decoding)) {

                                   py_vers = is_python2()

                                   if (!is.null(py_vers)) {

                                     if (py_vers) {

                                       string1 <- BUILTINS$str(string1)$decode(self$decoding)

                                       string2 <- BUILTINS$str(string2)$decode(self$decoding)

                                       force_ascii = FALSE                                       # in case of 'decoding != NULL' force_ascii = FALSE
                                     }
                                   }
                                 }

                                 return(FUZZ$WRatio(string1, string2, force_ascii))
                               },

                               UWRATIO = function(string1 = NULL, string2 = NULL) {

                                 if (is.null(string1) || is.null(string2)) { stop("both parameters 'string1' and 'string2' should be non-NULL", call. = F) }

                                 if (!inherits(string1, c('character', 'vector')) || !inherits(string2, c('character', 'vector'))) {

                                   stop("both parameters 'string1' and 'string2' should be of type character string", call. = F)
                                 }

                                 if (!is.null(self$decoding)) {

                                   py_vers = is_python2()

                                   if (!is.null(py_vers)) {

                                     if (py_vers) {

                                       string1 <- BUILTINS$str(string1)$decode(self$decoding)

                                       string2 <- BUILTINS$str(string2)$decode(self$decoding)
                                     }
                                   }
                                 }

                                 return(FUZZ$UWRatio(string1, string2))
                               },

                               UQRATIO = function(string1 = NULL, string2 = NULL) {

                                 if (is.null(string1) || is.null(string2)) { stop("both parameters 'string1' and 'string2' should be non-NULL", call. = F) }

                                 if (!inherits(string1, c('character', 'vector')) || !inherits(string2, c('character', 'vector'))) {

                                   stop("both parameters 'string1' and 'string2' should be of type character string", call. = F)
                                 }

                                 if (!is.null(self$decoding)) {

                                   py_vers = is_python2()

                                   if (!is.null(py_vers)) {

                                     if (py_vers) {

                                       string1 <- BUILTINS$str(string1)$decode(self$decoding)

                                       string2 <- BUILTINS$str(string2)$decode(self$decoding)
                                     }
                                   }
                                 }

                                 return(FUZZ$UQRatio(string1, string2))
                               },

                               Token_sort_ratio = function(string1 = NULL, string2 = NULL, force_ascii = TRUE, full_process = TRUE) {

                                 if (is.null(string1) || is.null(string2)) { stop("both parameters 'string1' and 'string2' should be non-NULL", call. = F) }

                                 if (!inherits(string1, c('character', 'vector')) || !inherits(string2, c('character', 'vector'))) {

                                   stop("both parameters 'string1' and 'string2' should be of type character string", call. = F)
                                 }

                                 if (!is.logical(force_ascii)) stop("the 'force_ascii' parameter should be of type boolean", call. = F)

                                 if (!is.logical(full_process)) stop("the 'full_process' parameter should be of type boolean", call. = F)

                                 if (!is.null(self$decoding)) {

                                   py_vers = is_python2()

                                   if (!is.null(py_vers)) {

                                     if (py_vers) {

                                       string1 <- BUILTINS$str(string1)$decode(self$decoding)

                                       string2 <- BUILTINS$str(string2)$decode(self$decoding)

                                       force_ascii = FALSE                                       # in case of 'decoding != NULL' force_ascii = FALSE
                                     }
                                   }
                                 }

                                 return(FUZZ$token_sort_ratio(string1, string2, force_ascii, full_process))
                               },

                               Partial_ratio = function(string1 = NULL, string2 = NULL) {

                                 if (is.null(string1) || is.null(string2)) { stop("both parameters 'string1' and 'string2' should be non-NULL", call. = F) }

                                 if (!inherits(string1, c('character', 'vector')) || !inherits(string2, c('character', 'vector'))) {

                                   stop("both parameters 'string1' and 'string2' should be of type character string", call. = F)
                                 }

                                 if (!is.null(self$decoding)) {

                                   py_vers = is_python2()

                                   if (!is.null(py_vers)) {

                                     if (py_vers) {

                                       string1 <- BUILTINS$str(string1)$decode(self$decoding)

                                       string2 <- BUILTINS$str(string2)$decode(self$decoding)
                                     }
                                   }
                                 }

                                 return(FUZZ$partial_ratio(string1, string2))
                               },

                               Token_set_ratio = function(string1 = NULL, string2 = NULL, force_ascii = TRUE, full_process = TRUE) {

                                 if (is.null(string1) || is.null(string2)) { stop("both parameters 'string1' and 'string2' should be non-NULL", call. = F) }

                                 if (!inherits(string1, c('character', 'vector')) || !inherits(string2, c('character', 'vector'))) {

                                   stop("both parameters 'string1' and 'string2' should be of type character string", call. = F)
                                 }

                                 if (!is.logical(force_ascii)) stop("the 'force_ascii' parameter should be of type boolean", call. = F)

                                 if (!is.logical(full_process)) stop("the 'full_process' parameter should be of type boolean", call. = F)

                                 if (!is.null(self$decoding)) {

                                   py_vers = is_python2()

                                   if (!is.null(py_vers)) {

                                     if (py_vers) {

                                       string1 <- BUILTINS$str(string1)$decode(self$decoding)

                                       string2 <- BUILTINS$str(string2)$decode(self$decoding)

                                       force_ascii = FALSE                                       # in case of 'decoding != NULL' force_ascii = FALSE
                                     }
                                   }
                                 }

                                 return(FUZZ$token_set_ratio(string1, string2, force_ascii, full_process))
                               }
                       )
)


#' Utility functions
#'
#'
#' @param decoding either NULL or a character string. If not NULL then the \emph{decoding} parameter takes one of the standard python encodings (such as 'utf-8'). See the \emph{details} and \emph{references} link for more information (in this class it applies only to the \emph{Full_process} function)
#' @param string  a character string.
#' @param string1 a character string.
#' @param string2 a character string.
#' @param input any kind of data type (applies to the \emph{Asciidammit} method)
#' @param force_ascii allow only ASCII characters (force convert to ascii)
#' @param n a float number
#' @export
#' @details
#'
#' the \emph{decoding} parameter is useful in case of non-ascii character strings. If this parameter is not NULL then the \emph{force_ascii} parameter (if applicable) is internally set to FALSE. Decoding applies only to python 2 configurations, as in python 3 character strings are decoded to unicode by default.
#'
#' the \emph{Full_process} processes a string by : 1. removing all but letters and numbers, 2. trim whitespace, 3. force to lower case and 4. if force_ascii == TRUE, force convert to ascii
#'
#' the \emph{INTR} method returns a correctly rounded integer
#'
#' the \emph{Make_type_consistent} method converts both objects if they aren't either both string or unicode instances to unicode
#'
#' the \emph{Asciidammit} performs ascii dammit using the following expression \emph{bad_chars = str("").join([chr(i) for i in range(128, 256)])}. Applies to any kind of R data type.
#'
#' the \emph{Asciionly} method returns the same result as the \emph{Asciidammit} method but for character strings using the python \emph{.translate()} function.
#'
#' the \emph{Validate_string} method checks that the input has length and that length is greater than 0
#'
#' Some of the utils functions are used as secondary methods in the \emph{FuzzExtract} class. See the examples of the \emph{FuzzExtract} class for more details.
#'
#' @references  https://github.com/seatgeek/fuzzywuzzy/blob/master/fuzzywuzzy/utils.py, https://docs.python.org/3/library/codecs.html#standard-encodings
#' @docType class
#' @importFrom R6 R6Class
#' @import reticulate
#' @section Methods:
#'
#' \describe{
#'  \item{\code{FuzzUtils$new()}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Full_process(string = NULL, force_ascii = TRUE, decoding = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{INTR(n = 2.0)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Make_type_consistent(string1 = NULL, string2 = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Asciidammit(input = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Asciionly(string = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Validate_string(string = NULL)}}{}
#'  }
#'
#' @usage # init <- FuzzUtils$new()
#' @examples
#'
#' try({
#'   if (reticulate::py_available(initialize = FALSE)) {
#'
#'     if (check_availability()) {
#'
#'       library(fuzzywuzzyR)
#'
#'       s1 = 'Frodo Baggins'
#'
#'       s2 = 'Bilbo Baggin'
#'
#'       init = FuzzUtils$new()
#'
#'       init$Full_process(string = s1, force_ascii = TRUE)
#'
#'       init$INTR(n = 2.0)
#'
#'       init$Make_type_consistent(string1 = s1, string2 = s2)
#'
#'       #------------------------------------
#'       # 'Asciidammit' with character string
#'       #------------------------------------
#'
#'       init$Asciidammit(input = s1)
#'
#'       #----------------------------------------------------------------
#'       # 'Asciidammit' with data.frame(123) [ or any kind of data type ]
#'       #----------------------------------------------------------------
#'
#'       init$Asciidammit(input = data.frame(123))
#'
#'       init$Asciionly(string = s1)
#'
#'       init$Validate_string(string = s2)
#'     }
#'   }
#' }, silent=TRUE)


FuzzUtils <- R6::R6Class("FuzzUtils",

                           lock_objects = FALSE,

                           public = list(

                             initialize = function() {

                             },

                             Full_process = function(string = NULL, force_ascii = TRUE, decoding = NULL) {

                               if (!is.null(decoding)) {

                                 if (!inherits(decoding, 'character')) {

                                   stop("the 'decoding' parameter can be either NULL or a character string", call. = F)
                                 }
                               }

                               if (is.null(string)) { stop("the 'string' parameter should be non-NULL", call. = F) }

                               if (!inherits(string, c('character', 'vector'))) {

                                 stop("the 'string' parameter should be of type character string", call. = F)
                               }

                               if (!is.logical(force_ascii)) stop("the 'force_ascii' parameter should be of type boolean", call. = F)

                               if (!is.null(decoding)) {

                                 py_vers = is_python2()

                                 if (!is.null(py_vers)) {

                                   if (py_vers) {

                                     string <- BUILTINS$str(string)$decode(decoding)

                                     force_ascii = FALSE                                       # in case of 'decoding != NULL' force_ascii = FALSE
                                   }
                                 }
                               }

                               return(UTILS$full_process(string, force_ascii))
                             },

                             INTR = function(n = 2.0) {

                               if (!inherits(n, c('double', 'numeric', 'integer'))) { stop("the 'n' parameter should be of type numeric", call. = F) }

                               return(UTILS$intr(n))
                             },

                             Make_type_consistent = function(string1 = NULL, string2 = NULL) {

                               if (is.null(string1) || is.null(string2)) { stop("both parameters 'string1' and 'string2' should be non-NULL", call. = F) }

                               if (!inherits(string1, c('character', 'vector')) || !inherits(string2, c('character', 'vector'))) {

                                 stop("both parameters 'string1' and 'string2' should be of type character string", call. = F)
                               }

                               return(UTILS$make_type_consistent(string1, string2))
                             },


                             Asciidammit = function(input = NULL) {

                               return(UTILS$asciidammit(input))
                             },


                             Asciionly = function(string = NULL) {

                               if (is.null(string)) { stop("the 'string' parameter should be non-NULL", call. = F) }

                               if (!inherits(string, c('character', 'vector'))) {

                                 stop("the 'string' parameter should be of type character string", call. = F)
                               }

                               return(UTILS$asciionly(string))
                             },


                             Validate_string = function(string = NULL) {

                               if (is.null(string)) { stop("the 'string' parameter should be non-NULL", call. = F) }

                               if (!inherits(string, c('character', 'vector'))) {

                                 stop("the 'string' parameter should be of type character string", call. = F)
                               }

                               return(UTILS$validate_string(string))
                             }
                           )
)



#' secondary function for the 'FuzzExtract' class
#'
#' @keywords internal

check_scorer = function(scorer, DECODING) {

  if (!is.null(scorer)) {

    if (!inherits(scorer, 'function')) { stop("the 'scorer' parameter should be of type function", call. = F) }

    tmp_sc = scorer
  }

  else {

    tmp_init = FuzzMatcher$new(decoding = DECODING)

    tmp_sc = tmp_init$WRATIO            # defaults to 'FuzzMatcher.WRATIO()'
  }

  return(tmp_sc)
}



#' Fuzzy extraction from a sequence
#'
#'
#' @param decoding either NULL or a character string. If not NULL then the \emph{decoding} parameter takes one of the standard python encodings (such as 'utf-8'). See the \emph{details} and \emph{references} link for more information.
#' @param string  a character string.
#' @param sequence_strings a character string vector
#' @param contains_dupes a vector of strings that we would like to dedupe
#' @param processor either NULL or a function of the form f(a) -> b, where a is the query or individual choice and b is the choice to be used in matching. See the examples for more details.
#' @param scorer a function for scoring matches between the query and an individual processed choice. This should be a function of the form f(query, choice) -> int. By default, FuzzMatcher.WRATIO() is used and expects both query and choice to be strings. See the examples for more details.
#' @param limit An integer value for the maximum number of elements to be returned. Defaults to 5L
#' @param score_cutoff an integer value for the score threshold. No matches with a score less than this number will be returned. Defaults to 0
#' @param threshold the numerical value (0, 100) point at which we expect to find duplicates. Defaults to 70 out of 100
#' @export
#' @details
#'
#' the \emph{decoding} parameter is useful in case of non-ascii character strings. If this parameter is not NULL then the \emph{force_ascii} parameter (if applicable) is internally set to FALSE. Decoding applies only to python 2 configurations, as in python 3 character strings are decoded to unicode by default.
#'
#' the \emph{Extract} method selects the best match of a character string vector. It returns a list with the match and it's score.
#'
#' the \emph{ExtractBests} method returns a list of the best matches for a sequence of character strings.
#'
#' the \emph{ExtractWithoutOrder} method returns the best match of a character string vector (in python it returns a generator of tuples containing the match and it's score).
#'
#' the \emph{ExtractOne} method finds the single best match above a score for a character string vector. This is a convenience method which returns the single best choice.
#'
#' the \emph{Dedupe} is a convenience method which takes a character string vector containing duplicates and uses fuzzy matching to identify and remove duplicates. Specifically, it uses the \emph{Extract} method
#' to identify duplicates that score greater than a user defined threshold. Then, it looks for the longest item in the duplicate vector since we assume this item contains the most entity information and returns that.
#' It breaks string length ties on an alphabetical sort. Note: as the threshold DECREASES the number of duplicates that are found INCREASES. This means that the returned deduplicated list will likely be shorter.
#' Raise the threshold for fuzzy_dedupe to be less sensitive.
#'
#' @references  https://github.com/seatgeek/fuzzywuzzy/blob/master/fuzzywuzzy/process.py, https://docs.python.org/3/library/codecs.html#standard-encodings
#' @docType class
#' @importFrom R6 R6Class
#' @import reticulate
#' @section Methods:
#'
#' \describe{
#'  \item{\code{FuzzExtract$new(decoding = NULL)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Extract(string = NULL, sequence_strings = NULL, processor = NULL, scorer = NULL, limit = 5L)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{ExtractBests(string = NULL, sequence_strings = NULL, processor = NULL, scorer = NULL, score_cutoff = 0L, limit = 5L)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{ExtractWithoutOrder(string = NULL, sequence_strings = NULL, processor = NULL, scorer = NULL, score_cutoff = 0L)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{ExtractOne(string = NULL, sequence_strings = NULL, processor = NULL, scorer = NULL, score_cutoff = 0L)}}{}
#'
#'  \item{\code{--------------}}{}
#'
#'  \item{\code{Dedupe(contains_dupes = NULL, threshold = 70L, scorer = NULL)}}{}
#'
#'  }
#'
#' @usage # init <- FuzzExtract$new(decoding = NULL)
#' @examples
#'
#' try({
#'   if (reticulate::py_available(initialize = FALSE)) {
#'
#'     if (check_availability()) {
#'
#'       library(fuzzywuzzyR)
#'
#'       word = "new york jets"
#'
#'       choices = c("Atlanta Falcons", "New York Jets", "New York Giants", "Dallas Cowboys")
#'
#'       duplicat = c('Frodo Baggins', 'Tom Sawyer', 'Bilbo Baggin', 'Samuel L. Jackson',
#'
#'                    'F. Baggins', 'Frody Baggins', 'Bilbo Baggins')
#'
#'       #------------
#'       # processor :
#'       #------------
#'
#'       init_proc = FuzzUtils$new()
#'
#'       PROC = init_proc$Full_process    # class process-method
#'
#'       PROC1 = tolower                  # base R function
#'
#'       #---------
#'       # scorer :
#'       #---------
#'
#'       init_scor = FuzzMatcher$new()
#'
#'       SCOR = init_scor$WRATIO
#'
#'
#'       init <- FuzzExtract$new()
#'
#'       init$Extract(string = word, sequence_strings = choices, processor = PROC, scorer = SCOR)
#'
#'       init$ExtractBests(string = word, sequence_strings = choices, processor = PROC1,
#'
#'                         scorer = SCOR, score_cutoff = 0L, limit = 2L)
#'
#'       init$ExtractWithoutOrder(string = word, sequence_strings = choices, processor = PROC,
#'
#'                                scorer = SCOR, score_cutoff = 0L)
#'
#'       init$ExtractOne(string = word, sequence_strings = choices, processor = PROC,
#'
#'                       scorer = SCOR, score_cutoff = 0L)
#'
#'       init$Dedupe(contains_dupes = duplicat, threshold = 70L, scorer = SCOR)
#'
#'     }
#'   }
#' }, silent=TRUE)


FuzzExtract <- R6::R6Class("FuzzExtract",

                           lock_objects = FALSE,

                           public = list(

                             initialize = function(decoding = NULL) {

                               self$decoding <- decoding

                               if (!is.null(self$decoding)) {

                                 if (!inherits(self$decoding, 'character')) {

                                   stop("the 'decoding' parameter can be either NULL or a character string", call. = F)
                                 }
                               }
                             },

                             Extract = function(string = NULL, sequence_strings = NULL, processor = NULL, scorer = NULL, limit = 5L) {

                               if (is.null(string) || is.null(sequence_strings)) { stop("both parameters 'string' and 'sequence_strings' should be non-NULL", call. = F) }

                               if (!inherits(string, c('character', 'vector')) || !inherits(sequence_strings, c('character', 'vector'))) {

                                 stop("both parameters 'string' and 'sequence_strings' should be of type character string", call. = F)
                               }

                               if (!is.null(processor)) {

                                 if (!inherits(processor, 'function')) { stop("the 'processor' parameter should be of type function", call. = F) }
                               }

                               tmp_sc = check_scorer(scorer, self$decoding)

                               if (!inherits(limit, c('numeric', 'integer'))) { stop("the 'limit' parameter should be of type integer", call. = F) }

                               limit = as.integer(limit)

                               if (!is.null(self$decoding)) {

                                 py_vers = is_python2()

                                 if (!is.null(py_vers)) {

                                   if (py_vers) {

                                     string <- BUILTINS$str(string)$decode(self$decoding)

                                     sequence_strings = unlist(lapply(1:length(sequence_strings), function(item) BUILTINS$str(sequence_strings[item])$decode(self$decoding)))    # add parallelization in case of big vectors [ if requested ]
                                   }
                                 }
                               }

                               tmp = EXTRACT$extract(string, sequence_strings, processor, tmp_sc, limit)

                               return(tmp)
                             },

                             ExtractBests = function(string = NULL, sequence_strings = NULL, processor = NULL, scorer = NULL, score_cutoff = 0L, limit = 5L) {

                               if (is.null(string) || is.null(sequence_strings)) { stop("both parameters 'string' and 'sequence_strings' should be non-NULL", call. = F) }

                               if (!inherits(string, c('character', 'vector')) || !inherits(sequence_strings, c('character', 'vector'))) {

                                 stop("both parameters 'string' and 'sequence_strings' should be of type character string", call. = F)
                               }

                               if (!is.null(processor)) {

                                 if (!inherits(processor, 'function')) { stop("the 'processor' parameter should be of type function", call. = F) }
                               }

                               tmp_sc = check_scorer(scorer, self$decoding)

                               if (!inherits(score_cutoff, c('numeric', 'integer'))) { stop("the 'score_cutoff' parameter should be of type integer", call. = F) }

                               if (!inherits(limit, c('numeric', 'integer'))) { stop("the 'limit' parameter should be of type integer", call. = F) }

                               limit = as.integer(limit)

                               score_cutoff = as.integer(score_cutoff)

                               if (!is.null(self$decoding)) {

                                 py_vers = is_python2()

                                 if (!is.null(py_vers)) {

                                   if (py_vers) {

                                     string <- BUILTINS$str(string)$decode(self$decoding)

                                     sequence_strings = unlist(lapply(1:length(sequence_strings), function(item) BUILTINS$str(sequence_strings[item])$decode(self$decoding)))       # add parallelization in case of big vectors [ if requested ]
                                   }
                                 }
                               }

                               tmp = EXTRACT$extractBests(string, sequence_strings, processor, tmp_sc, score_cutoff, limit)

                               return(tmp)
                             },


                             ExtractWithoutOrder = function(string = NULL, sequence_strings = NULL, processor = NULL, scorer = NULL, score_cutoff = 0L) {

                               if (is.null(string) || is.null(sequence_strings)) { stop("both parameters 'string' and 'sequence_strings' should be non-NULL", call. = F) }

                               if (!inherits(string, c('character', 'vector')) || !inherits(sequence_strings, c('character', 'vector'))) {

                                 stop("both parameters 'string' and 'sequence_strings' should be of type character string", call. = F)
                               }

                               if (!is.null(processor)) {

                                 if (!inherits(processor, 'function')) { stop("the 'processor' parameter should be of type function", call. = F) }
                               }

                               tmp_sc = check_scorer(scorer, self$decoding)

                               if (!inherits(score_cutoff, c('numeric', 'integer'))) { stop("the 'score_cutoff' parameter should be of type integer", call. = F) }

                               score_cutoff = as.integer(score_cutoff)

                               if (!is.null(self$decoding)) {

                                 py_vers = is_python2()

                                 if (!is.null(py_vers)) {

                                   if (py_vers) {

                                     string <- BUILTINS$str(string)$decode(self$decoding)

                                     sequence_strings = unlist(lapply(1:length(sequence_strings), function(item) BUILTINS$str(sequence_strings[item])$decode(self$decoding)))       # add parallelization in case of big vectors [ if requested ]
                                   }
                                 }
                               }

                               iter_gen = EXTRACT$extractWithoutOrder(string, sequence_strings, processor, tmp_sc, score_cutoff)           # iterator-generator use the iterate() function to traverse

                               iter_out = reticulate::iterate(iter_gen, f = base::identity, simplify = TRUE)

                               return(iter_out)
                             },


                             ExtractOne = function(string = NULL, sequence_strings = NULL, processor = NULL, scorer = NULL, score_cutoff = 0L) {

                               if (is.null(string) || is.null(sequence_strings)) { stop("both parameters 'string' and 'sequence_strings' should be non-NULL", call. = F) }

                               if (!inherits(string, c('character', 'vector')) || !inherits(sequence_strings, c('character', 'vector'))) {

                                 stop("both parameters 'string' and 'sequence_strings' should be of type character string", call. = F)
                               }

                               if (!is.null(processor)) {

                                 if (!inherits(processor, 'function')) { stop("the 'processor' parameter should be of type function", call. = F) }
                               }

                               tmp_sc = check_scorer(scorer, self$decoding)

                               if (!inherits(score_cutoff, c('numeric', 'integer'))) { stop("the 'score_cutoff' parameter should be of type integer", call. = F) }

                               score_cutoff = as.integer(score_cutoff)

                               if (!is.null(self$decoding)) {

                                 py_vers = is_python2()

                                 if (!is.null(py_vers)) {

                                   if (py_vers) {

                                     string <- BUILTINS$str(string)$decode(self$decoding)

                                     sequence_strings = unlist(lapply(1:length(sequence_strings), function(item) BUILTINS$str(sequence_strings[item])$decode(self$decoding)))       # add parallelization in case of big vectors [ if requested ]
                                   }
                                 }
                               }

                               tmp = EXTRACT$extractOne(string, sequence_strings, processor, tmp_sc, score_cutoff)

                               return(tmp)
                             },

                             Dedupe = function(contains_dupes = NULL, threshold = 70L, scorer = NULL) {

                               if (is.null(contains_dupes)) { stop("the 'contains_dupes' parameter should be non-NULL", call. = F) }

                               if (!inherits(contains_dupes, c('character', 'vector'))) {

                                 stop("the 'contains_dupes' parameter should be of type character string vector", call. = F)
                               }

                               tmp_sc = check_scorer(scorer, self$decoding)

                               if (!inherits(threshold, c('numeric', 'integer'))) { stop("the 'threshold' parameter should be of type integer", call. = F) }

                               threshold = as.integer(threshold)

                               if (!is.null(self$decoding)) {

                                 py_vers = is_python2()

                                 if (!is.null(py_vers)) {

                                   if (py_vers) {

                                     contains_dupes = unlist(lapply(1:length(contains_dupes), function(item) BUILTINS$str(contains_dupes[item])$decode(self$decoding)))       # add parallelization in case of big vectors [ if requested ]
                                   }
                                 }
                               }

                               tmp = EXTRACT$dedupe(contains_dupes, threshold, tmp_sc)

                               return(tmp)                        # returns : dict_keys([...]) , use reticulate::py_str() to get the output in form of a string
                             }
                           )
)

