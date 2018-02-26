
#========================================================================================
# helper function to skip tests if we don't have the 'foo' module
# https://github.com/rstudio/reticulate


skip_test_if_no_module <- function(MODULE) {                   # MODULE is of type character string

  module_exists <- reticulate::py_module_available(MODULE)

  if (!module_exists) {

    testthat::skip(paste0(MODULE, " is not available for testthat-testing"))
  }
}


#===========================================================================================

# run the 'decoding' tests only in python 2 (not in python 3)

tests_python2 = function() {
  
  vers = reticulate::py_config()
  
  out = (as.numeric(vers$version) < 3)
  
  return(out)
}


#===========================================================================================

# data

s1 = ' It was a dark and stormy night. I was all alone sitting on a red chair.'

s2 = ' It was a murky and stormy night. I was all alone sitting on a crimson chair.'

word = 'Fra Bagg'

vec_getclose = c('Frodo Baggins', 'Tom Sawyer', 'Bilbo Baggin')

st1 = "Atlanta Falcons"

st2 = "New York Jets"


# non-ascii character strings [ https://github.com/mlampros/fuzzywuzzyR/issues/3 ]
# tests begin from line 1745 [ excluded the 'SequenceMatcher()' and 'GetCloseMatches()', which belong to the 'difflib' python package and character strings do not need to be encoded ]

one_word = "一个词"

seq_words = c("两个字", "序列词")

valid_one_word = "abc"

valid_seq_words = c("dbe", "fgc")

#===========================================================================================



context('all fuzzywuzzy R6 classes')


#---------------------------
# 'SequenceMatcher' r6 class
#---------------------------


testthat::test_that("both parameters are non-NULL input data types", {

  skip_test_if_no_module("difflib")

  testthat::expect_error( SequenceMatcher$new(string1 = s1, string2 = NULL) )
})


testthat::test_that("both inputs are of type character string", {

  skip_test_if_no_module("difflib")

  testthat::expect_error( SequenceMatcher$new(string1 = 1, string2 = s2) )
})


testthat::test_that("both inputs are of type character string", {

  skip_test_if_no_module("difflib")

  testthat::expect_error( SequenceMatcher$new(string1 = s1, string2 = 1) )
})


testthat::test_that("it returns the correct output for all ratios of the SequenceMatcher class", {

  skip_test_if_no_module("difflib")

  init = SequenceMatcher$new(string1 = s1, string2 = s2)

  all_ratios = c(init$ratio(), init$quick_ratio(), init$real_quick_ratio())

  res_RATIOS = all(all_ratios > 0 || all_ratios <= 1.0)

  bl_lst = init$get_matching_blocks()

  RES_MATCH = is.list(bl_lst) && length(bl_lst) > 0

  opc_lst = init$get_opcodes()

  RES_OPC = is.list(opc_lst) && length(opc_lst) > 0

  testthat::expect_true( sum(c(res_RATIOS, RES_MATCH, RES_OPC)) == 3  )
})


#----------------------------
# 'GetCloseMatches' function
#----------------------------


testthat::test_that("the string argument is not NULL", {

  skip_test_if_no_module("difflib")

  testthat::expect_error( GetCloseMatches(string = NULL, sequence_strings = vec_getclose, n = 3L, cutoff = 0.6) )
})


testthat::test_that("the sequence_strings argument is not NULL", {

  skip_test_if_no_module("difflib")

  testthat::expect_error( GetCloseMatches(string = word, sequence_strings = NULL, n = 3L, cutoff = 0.6) )
})



testthat::test_that("the string argument is a character string", {

  skip_test_if_no_module("difflib")

  testthat::expect_error( GetCloseMatches(string = 1, sequence_strings = vec_getclose, n = 3L, cutoff = 0.6) )
})


testthat::test_that("the sequence_strings argument is a character vector", {

  skip_test_if_no_module("difflib")

  testthat::expect_error( GetCloseMatches(string = word, sequence_strings = 1, n = 3L, cutoff = 0.6) )
})


testthat::test_that("the n argument is an integer", {

  skip_test_if_no_module("difflib")

  testthat::expect_error( GetCloseMatches(string = word, sequence_strings = vec_getclose, n = list(), cutoff = 0.6) )
})


testthat::test_that("the cutoff argument is numeric", {

  skip_test_if_no_module("difflib")

  testthat::expect_error( GetCloseMatches(string = word, sequence_strings = vec_getclose, n = 3L, cutoff = list()) )
})


testthat::test_that("the cutoff argument is in range [0, 1]", {

  skip_test_if_no_module("difflib")

  testthat::expect_error( GetCloseMatches(string = word, sequence_strings = vec_getclose, n = 3L, cutoff = 1.1) )
})


testthat::test_that("the cutoff argument is in range [0, 1]", {

  skip_test_if_no_module("difflib")

  testthat::expect_true( GetCloseMatches(string = word, sequence_strings = vec_getclose, n = 3L, cutoff = 0.6) == "Frodo Baggins" )
})



#---------------------------
# 'FuzzMatcher' r6 class
#---------------------------


#-----------------------------------  Partial_token_set_ratio


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_token_set_ratio(string1 = NULL, string2 = st2, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_token_set_ratio(string1 = st1, string2 = NULL, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_token_set_ratio(string1 = 1, string2 = st2, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_token_set_ratio(string1 = st1, string2 = 1, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("the force_ascii parameter is boolean", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_token_set_ratio(string1 = st1, string2 = st2, force_ascii = 'TRUE', full_process = TRUE) )
})


testthat::test_that("the full_process parameter is boolean", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_token_set_ratio(string1 = st1, string2 = st2, force_ascii = TRUE, full_process = 'TRUE') )
})


# testthat::test_that("the method returns a numeric value", {
#
#   skip_test_if_no_module("fuzzywuzzy.fuzz")
#
#   init = FuzzMatcher$new()
#
#   testthat::expect_true( init$Partial_token_set_ratio(string1 = st1, string2 = st2, force_ascii = TRUE, full_process = TRUE) == 31 )
# })


#-----------------------------------  Partial_token_sort_ratio


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_token_sort_ratio(string1 = NULL, string2 = st2, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_token_sort_ratio(string1 = st1, string2 = NULL, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_token_sort_ratio(string1 = 1, string2 = st2, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_token_sort_ratio(string1 = st1, string2 = 1, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("the force_ascii parameter is boolean", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_token_sort_ratio(string1 = st1, string2 = st2, force_ascii = 'TRUE', full_process = TRUE) )
})


testthat::test_that("the full_process parameter is boolean", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_token_sort_ratio(string1 = st1, string2 = st2, force_ascii = TRUE, full_process = 'TRUE') )
})


# testthat::test_that("the method returns a numeric value", {
#
#   skip_test_if_no_module("fuzzywuzzy.fuzz")
#
#   init = FuzzMatcher$new()
#
#   testthat::expect_true( init$Partial_token_sort_ratio(string1 = st1, string2 = st2, force_ascii = TRUE, full_process = TRUE) == 31 )
# })


#-----------------------------------  Ratio


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Ratio(string1 = NULL, string2 = st2) )
})


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Ratio(string1 = st1, string2 = NULL) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Ratio(string1 = 1, string2 = st2) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Ratio(string1 = st1, string2 = 1) )
})

# testthat::test_that("the method returns a numeric value", {
#
#   skip_test_if_no_module("fuzzywuzzy.fuzz")
#
#   init = FuzzMatcher$new()
#
#   testthat::expect_true( init$Ratio(string1 = st1, string2 = st2) == 21 )
# })


#-----------------------------------  QRATIO


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$QRATIO(string1 = NULL, string2 = st2, force_ascii = TRUE) )
})


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$QRATIO(string1 = st1, string2 = NULL, force_ascii = TRUE) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$QRATIO(string1 = 1, string2 = st2, force_ascii = TRUE) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$QRATIO(string1 = st1, string2 = 1, force_ascii = TRUE) )
})


testthat::test_that("the force_ascii parameter is boolean", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$QRATIO(string1 = st1, string2 = st2, force_ascii = 'TRUE') )
})


# testthat::test_that("the method returns a numeric value", {
#
#   skip_test_if_no_module("fuzzywuzzy.fuzz")
#
#   init = FuzzMatcher$new()
#
#   testthat::expect_true( init$QRATIO(string1 = st1, string2 = st2, force_ascii = TRUE) == 29 )
# })



#-----------------------------------  WRATIO


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$WRATIO(string1 = NULL, string2 = st2, force_ascii = TRUE) )
})


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$WRATIO(string1 = st1, string2 = NULL, force_ascii = TRUE) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$WRATIO(string1 = 1, string2 = st2, force_ascii = TRUE) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$WRATIO(string1 = st1, string2 = 1, force_ascii = TRUE) )
})


testthat::test_that("the force_ascii parameter is boolean", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$WRATIO(string1 = st1, string2 = st2, force_ascii = 'TRUE') )
})


# testthat::test_that("the method returns a numeric value", {
#
#   skip_test_if_no_module("fuzzywuzzy.fuzz")
#
#   init = FuzzMatcher$new()
#
#   testthat::expect_true( init$WRATIO(string1 = st1, string2 = st2, force_ascii = TRUE) == 29 )
# })



#-----------------------------------  UWRATIO


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$UWRATIO(string1 = NULL, string2 = st2) )
})


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$UWRATIO(string1 = st1, string2 = NULL) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$UWRATIO(string1 = 1, string2 = st2) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$UWRATIO(string1 = st1, string2 = 1) )
})

# testthat::test_that("the method returns a numeric value", {
#
#   skip_test_if_no_module("fuzzywuzzy.fuzz")
#
#   init = FuzzMatcher$new()
#
#   testthat::expect_true( init$UWRATIO(string1 = st1, string2 = st2) == 29 )
# })




#-----------------------------------  UQRATIO


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$UQRATIO(string1 = NULL, string2 = st2) )
})


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$UQRATIO(string1 = st1, string2 = NULL) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$UQRATIO(string1 = 1, string2 = st2) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$UQRATIO(string1 = st1, string2 = 1) )
})

# testthat::test_that("the method returns a numeric value", {
#
#   skip_test_if_no_module("fuzzywuzzy.fuzz")
#
#   init = FuzzMatcher$new()
#
#   testthat::expect_true( init$UQRATIO(string1 = st1, string2 = st2) == 29 )
# })



#-----------------------------------  Token_sort_ratio


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Token_sort_ratio(string1 = NULL, string2 = st2, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Token_sort_ratio(string1 = st1, string2 = NULL, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Token_sort_ratio(string1 = 1, string2 = st2, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Token_sort_ratio(string1 = st1, string2 = 1, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("the force_ascii parameter is boolean", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Token_sort_ratio(string1 = st1, string2 = st2, force_ascii = 'TRUE', full_process = TRUE) )
})


testthat::test_that("the full_process parameter is boolean", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Token_sort_ratio(string1 = st1, string2 = st2, force_ascii = TRUE, full_process = 'TRUE') )
})


testthat::test_that("the method returns a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_true( init$Token_sort_ratio(string1 = st1, string2 = st2, force_ascii = TRUE, full_process = TRUE) == 29 )
})





#-----------------------------------  Partial_ratio


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_ratio(string1 = NULL, string2 = st2) )
})


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_ratio(string1 = st1, string2 = NULL) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_ratio(string1 = 1, string2 = st2) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Partial_ratio(string1 = st1, string2 = 1) )
})

# testthat::test_that("the method returns a numeric value", {
#
#   skip_test_if_no_module("fuzzywuzzy.fuzz")
#
#   init = FuzzMatcher$new()
#
#   testthat::expect_true( init$Partial_ratio(string1 = st1, string2 = st2) == 23 )
# })



#-----------------------------------  Token_set_ratio


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Token_set_ratio(string1 = NULL, string2 = st2, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Token_set_ratio(string1 = st1, string2 = NULL, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Token_set_ratio(string1 = 1, string2 = st2, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Token_set_ratio(string1 = st1, string2 = 1, force_ascii = TRUE, full_process = TRUE) )
})


testthat::test_that("the force_ascii parameter is boolean", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Token_set_ratio(string1 = st1, string2 = st2, force_ascii = 'TRUE', full_process = TRUE) )
})


testthat::test_that("the full_process parameter is boolean", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_error( init$Token_set_ratio(string1 = st1, string2 = st2, force_ascii = TRUE, full_process = 'TRUE') )
})


testthat::test_that("the method returns a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_true( init$Token_set_ratio(string1 = st1, string2 = st2, force_ascii = TRUE, full_process = TRUE) == 29 )
})



#----------------------
# 'FuzzUtils' r6 class
#----------------------


#-----------------------------------  Full_process


testthat::test_that("input is non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_error( init$Full_process(string = NULL, force_ascii = TRUE) )
})


testthat::test_that("input is character string", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_error( init$Full_process(string = 1, force_ascii = TRUE) )
})


testthat::test_that("the force_ascii parameter is boolean", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_error( init$Full_process(string1 = st1, string2 = st2, force_ascii = 'TRUE') )
})


testthat::test_that("the method returns the expected output", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_true( init$Full_process(string = st1, force_ascii = TRUE) == "atlanta falcons" )
})


#-----------------------------------  INTR


testthat::test_that("input is numeric", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_error( init$INTR(n = list() ) )
})


testthat::test_that("input is numeric", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_true( init$INTR(n = 1.23 ) == 1 )
})



#-----------------------------------  Make_type_consistent


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_error( init$Make_type_consistent(string1 = NULL, string2 = st2) )
})


testthat::test_that("both inputs are non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_error( init$Make_type_consistent(string1 = st1, string2 = NULL) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_error( init$Make_type_consistent(string1 = 1, string2 = st2) )
})


testthat::test_that("both inputs are character strings", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_error( init$Make_type_consistent(string1 = st1, string2 = 1) )
})


testthat::test_that("the method returns the expected output", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  res_lst = init$Make_type_consistent(string1 = st1, string2 = st2)

  testthat::expect_true(is.list(res_lst) && length(res_lst) == 2 && length(match(c("Atlanta Falcons", "New York Jets"), unlist(lapply(res_lst, function(x) x[[1]])))) == 2 )
})



#-----------------------------------  Asciidammit [ RETURNS ANY KIND OF DATA TYPE ]


testthat::test_that("the method takes as input a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_true( init$Asciidammit(input = 123) == "123.0" )
})



testthat::test_that("the method takes as input a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_true( init$Asciidammit(input = list(123)) == "[123.0]" )
})



testthat::test_that("the method takes as input a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_true( init$Asciidammit(input = data.frame(123)) == "{'X123': 123.0}" )
})




testthat::test_that("the method takes as input a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_true( init$Asciidammit(input = numeric(0)) == "[]" )
})




testthat::test_that("the method takes as input a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_true( init$Asciidammit(input = NULL) == "None" )
})




#-----------------------------------  Asciionly


testthat::test_that("input is non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_error( init$Asciionly(string = NULL) )
})


testthat::test_that("input is character string", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_error( init$Asciionly(string = 1) )
})



testthat::test_that("the method returns the expected output", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_true( init$Asciionly(string = st1) == "Atlanta Falcons" )
})




#-----------------------------------  Validate_string


testthat::test_that("input is non-NULL", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_error( init$Validate_string(string = NULL) )
})


testthat::test_that("input is character string", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_error( init$Validate_string(string = 1) )
})



testthat::test_that("the method returns the expected output , the input is of length greater than 0", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_true( init$Validate_string(string = st1) == TRUE )
})



testthat::test_that("the method returns the expected output , the input is of length equal to 0", {

  skip_test_if_no_module("fuzzywuzzy.utils")

  init = FuzzUtils$new()

  testthat::expect_true( init$Validate_string(string = character(0)) == FALSE )
})




#------------------------
# 'FuzzExtract' r6 class
#------------------------


#-----------------------------------  Extract


testthat::test_that("the string argument is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$Extract(string = NULL, sequence_strings = vec_getclose, processor = NULL, scorer = NULL, limit = 5L) )
})


testthat::test_that("the sequence_strings argument is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$Extract(string = word, sequence_strings = NULL, processor = NULL, scorer = NULL, limit = 5L) )
})



testthat::test_that("the string argument is a character string", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$Extract(string = 1, sequence_strings = vec_getclose, processor = NULL, scorer = NULL, limit = 5L) )
})


testthat::test_that("the sequence_strings argument is a character vector", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$Extract(string = word, sequence_strings = 1, processor = NULL, scorer = NULL, limit = 5L) )
})



testthat::test_that("the processor argument is a function in case that is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  testthat::expect_error( init$Extract(string = word, sequence_strings = vec_getclose, processor = list(), scorer = SCOR, limit = 5L) )
})


testthat::test_that("the scorer argument is a function in case that is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  testthat::expect_error( init$Extract(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = list(), limit = 5L) )
})



testthat::test_that("the limit argument is a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  testthat::expect_error( init$Extract(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = NULL, limit = '5L') )
})


testthat::test_that("returns the expected output if scorer is NULL - processor takes default function", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  res_lst = init$Extract(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = NULL, limit = 5L)

  testthat::expect_true( inherits(unlist(lapply(res_lst, function(x) x[[1]])), c('character', 'vector')) && inherits(unlist(lapply(res_lst, function(x) x[[2]])), c('integer', 'vector')) )
})



testthat::test_that("returns the expected output if scorer is not NULL - processor takes default function", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  res_lst = init$Extract(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = SCOR, limit = 5L)

  testthat::expect_true( inherits(unlist(lapply(res_lst, function(x) x[[1]])), c('character', 'vector')) && inherits(unlist(lapply(res_lst, function(x) x[[2]])), c('integer', 'vector')) )
})




#-----------------------------------  ExtractBests


testthat::test_that("the string argument is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$ExtractBests(string = NULL, sequence_strings = vec_getclose, processor = NULL, scorer = NULL, limit = 5L) )
})


testthat::test_that("the sequence_strings argument is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$ExtractBests(string = word, sequence_strings = NULL, processor = NULL, scorer = NULL, limit = 5L) )
})



testthat::test_that("the string argument is a character string", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$ExtractBests(string = 1, sequence_strings = vec_getclose, processor = NULL, scorer = NULL, limit = 5L) )
})


testthat::test_that("the sequence_strings argument is a character vector", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$ExtractBests(string = word, sequence_strings = 1, processor = NULL, scorer = NULL, limit = 5L) )
})



testthat::test_that("the processor argument is a function in case that is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  testthat::expect_error( init$ExtractBests(string = word, sequence_strings = vec_getclose, processor = list(), scorer = SCOR, limit = 5L) )
})


testthat::test_that("the scorer argument is a function in case that is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  testthat::expect_error( init$ExtractBests(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = list(), limit = 5L) )
})


testthat::test_that("the score_cutoff argument is a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  testthat::expect_error( init$ExtractBests(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = NULL, score_cutoff = '0L', limit = 5L) )
})


testthat::test_that("the limit argument is a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  testthat::expect_error( init$ExtractBests(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = NULL, limit = '5L') )
})


testthat::test_that("returns the expected output if scorer is NULL - processor takes default function", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  res_lst = init$ExtractBests(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = NULL, limit = 1L)

  testthat::expect_true( res_lst[[1]][[1]] == "Frodo Baggins" && res_lst[[1]][[2]] == 67 )
})



testthat::test_that("returns the expected output if scorer is not NULL - processor takes default function", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  res_lst = init$ExtractBests(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = SCOR, limit = 5L)

  testthat::expect_true( res_lst[[1]][[1]] == "Frodo Baggins" && res_lst[[1]][[2]] == 67 )
})





#-----------------------------------  ExtractWithoutOrder


testthat::test_that("the string argument is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$ExtractWithoutOrder(string = NULL, sequence_strings = vec_getclose, processor = NULL, scorer = NULL) )
})


testthat::test_that("the sequence_strings argument is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$ExtractWithoutOrder(string = word, sequence_strings = NULL, processor = NULL, scorer = NULL) )
})



testthat::test_that("the string argument is a character string", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$ExtractWithoutOrder(string = 1, sequence_strings = vec_getclose, processor = NULL, scorer = NULL) )
})


testthat::test_that("the sequence_strings argument is a character vector", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$ExtractWithoutOrder(string = word, sequence_strings = 1, processor = NULL, scorer = NULL) )
})



testthat::test_that("the processor argument is a function in case that is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  testthat::expect_error( init$ExtractWithoutOrder(string = word, sequence_strings = vec_getclose, processor = list(), scorer = SCOR) )
})


testthat::test_that("the scorer argument is a function in case that is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  testthat::expect_error( init$ExtractWithoutOrder(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = list()) )
})


testthat::test_that("the score_cutoff argument is a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  testthat::expect_error( init$ExtractWithoutOrder(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = NULL, score_cutoff = '0L') )
})


testthat::test_that("returns the expected output if scorer is NULL - processor takes default function", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  res_lst = init$ExtractWithoutOrder(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = NULL)

  testthat::expect_true(  inherits(unlist(lapply(res_lst, function(x) x[[1]])), c('character', 'vector')) && inherits(unlist(lapply(res_lst, function(x) x[[2]])), c('integer', 'vector')) )
})



testthat::test_that("returns the expected output if scorer is not NULL - processor takes default function", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  res_lst = init$ExtractWithoutOrder(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = SCOR)

  testthat::expect_true(  inherits(unlist(lapply(res_lst, function(x) x[[1]])), c('character', 'vector')) && inherits(unlist(lapply(res_lst, function(x) x[[2]])), c('integer', 'vector')) )
})





#-----------------------------------  ExtractOne


testthat::test_that("the string argument is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$ExtractOne(string = NULL, sequence_strings = vec_getclose, processor = NULL, scorer = NULL) )
})


testthat::test_that("the sequence_strings argument is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$ExtractOne(string = word, sequence_strings = NULL, processor = NULL, scorer = NULL) )
})



testthat::test_that("the string argument is a character string", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$ExtractOne(string = 1, sequence_strings = vec_getclose, processor = NULL, scorer = NULL) )
})


testthat::test_that("the sequence_strings argument is a character vector", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$ExtractOne(string = word, sequence_strings = 1, processor = NULL, scorer = NULL) )
})



testthat::test_that("the processor argument is a function in case that is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  testthat::expect_error( init$ExtractOne(string = word, sequence_strings = vec_getclose, processor = list(), scorer = SCOR) )
})


testthat::test_that("the scorer argument is a function in case that is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  testthat::expect_error( init$ExtractOne(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = list()) )
})


testthat::test_that("the score_cutoff argument is a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  testthat::expect_error( init$ExtractOne(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = NULL, score_cutoff = '0L') )
})


testthat::test_that("returns the expected output if scorer is NULL - processor takes default function", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  res_lst = init$ExtractOne(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = NULL)

  testthat::expect_true( res_lst[[1]] == "Frodo Baggins" && res_lst[[2]] == 67 )
})



testthat::test_that("returns the expected output if scorer is not NULL - processor takes default function", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  init_scor = FuzzMatcher$new()

  SCOR = init_scor$WRATIO

  res_lst = init$ExtractOne(string = word, sequence_strings = vec_getclose, processor = NULL, scorer = SCOR)

  testthat::expect_true( res_lst[[1]] == "Frodo Baggins" && res_lst[[2]] == 67)
})




#-----------------------------------  Dedupe


testthat::test_that("the contains_dupes argument is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$Dedupe(contains_dupes = NULL, threshold = 70L, scorer = NULL) )
})



testthat::test_that("the contains_dupes argument is a character string", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$Dedupe(contains_dupes = 1, threshold = 70L, scorer = NULL) )
})


testthat::test_that("the scorer argument is a function in case that is not NULL", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$Dedupe(contains_dupes = c(vec_getclose, "Bilbo Baggin"), threshold = 70L, scorer = list()) )
})



testthat::test_that("the threshold argument is a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  testthat::expect_error( init$Dedupe(contains_dupes = c(vec_getclose, "Bilbo Baggin"), threshold = '70L', scorer = NULL) )
})


testthat::test_that("it returns the expected output", {

  skip_test_if_no_module("fuzzywuzzy.process")

  init = FuzzExtract$new()

  res_str = reticulate::py_str(init$Dedupe(contains_dupes = c(vec_getclose, "Bilbo Baggin"), threshold = 70L, scorer = NULL))

  testthat::expect_true( is.character(res_str) && length(res_str) > 0 )
})



#==========================================================================================================================================
#==========================================================================================================================================

#-----------------------------------------------------------------------------
# tests that fail (probably due to approximation) on r-devel-Windows (skipped)
#-----------------------------------------------------------------------------


if (.Platform$OS.type != "windows") {

  testthat::test_that("the method returns a numeric value", {

    skip_test_if_no_module("fuzzywuzzy.fuzz")

    init = FuzzMatcher$new()

    testthat::expect_true( init$Partial_token_set_ratio(string1 = st1, string2 = st2, force_ascii = TRUE, full_process = TRUE) == 31 )
  })


  testthat::test_that("the method returns a numeric value", {

    skip_test_if_no_module("fuzzywuzzy.fuzz")

    init = FuzzMatcher$new()

    testthat::expect_true( init$Partial_token_sort_ratio(string1 = st1, string2 = st2, force_ascii = TRUE, full_process = TRUE) == 31 )
  })


  testthat::test_that("the method returns a numeric value", {

    skip_test_if_no_module("fuzzywuzzy.fuzz")

    init = FuzzMatcher$new()

    testthat::expect_true( init$Ratio(string1 = st1, string2 = st2) == 21 )
  })


  testthat::test_that("the method returns a numeric value", {

    skip_test_if_no_module("fuzzywuzzy.fuzz")

    init = FuzzMatcher$new()

    testthat::expect_true( init$QRATIO(string1 = st1, string2 = st2, force_ascii = TRUE) == 29 )
  })


  testthat::test_that("the method returns a numeric value", {

    skip_test_if_no_module("fuzzywuzzy.fuzz")

    init = FuzzMatcher$new()

    testthat::expect_true( init$WRATIO(string1 = st1, string2 = st2, force_ascii = TRUE) == 29 )
  })



  testthat::test_that("the method returns a numeric value", {

    skip_test_if_no_module("fuzzywuzzy.fuzz")

    init = FuzzMatcher$new()

    testthat::expect_true( init$UWRATIO(string1 = st1, string2 = st2) == 29 )
  })


  testthat::test_that("the method returns a numeric value", {

    skip_test_if_no_module("fuzzywuzzy.fuzz")

    init = FuzzMatcher$new()

    testthat::expect_true( init$UQRATIO(string1 = st1, string2 = st2) == 29 )
  })


  testthat::test_that("the method returns a numeric value", {

    skip_test_if_no_module("fuzzywuzzy.fuzz")

    init = FuzzMatcher$new()

    testthat::expect_true( init$Partial_ratio(string1 = st1, string2 = st2) == 23 )
  })
}




#==================================================================================== non-ascii character strings  

#------------------------------------------------------------------------------------------
# tests for all functions excluding the 'SequenceMatcher()' and 'GetCloseMatches()'
# Both belong to the 'difflib' python package and character strings are internally encoded
#------------------------------------------------------------------------------------------


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FuzzMatcher [ methods ]


if (.Platform$OS.type != "windows") {                         # run the decoding tests only on unix OS's due to an exception-error on win32

  if (reticulate::py_available()) {                           # add this line otherwise solaris OS raises an error
  
    if (tests_python2()) {                                    # running the "test_python2()" function on win32 will raise an error  ["Your current architecture is 32bit however this version of Python is compiled for 64bit" -- on win64 works error-free ]
    
      testthat::test_that("if the 'decoding' parameter is equal to NULL it returns a 0 value (incorrect result) for the [ Partial_token_set_ratio ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = NULL)
        
        testthat::expect_true( init$Partial_token_set_ratio(string1 = one_word, string2 = seq_words[1]) == 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is NOT equal to NULL it returns a value greater than 0 for the [ Partial_token_set_ratio ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = 'utf-8')
        
        testthat::expect_true( init$Partial_token_set_ratio(string1 = one_word, string2 = seq_words[1]) > 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is equal to NULL it returns a 0 value (incorrect result) for the [ Partial_token_sort_ratio ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = NULL)
        
        testthat::expect_true( init$Partial_token_sort_ratio(string1 = one_word, string2 = seq_words[1]) == 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is NOT equal to NULL it returns a value greater than 0 for the [ Partial_token_sort_ratio ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = 'utf-8')
        
        testthat::expect_true( init$Partial_token_sort_ratio(string1 = one_word, string2 = seq_words[1]) > 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is equal to NULL it returns a 0 value (incorrect result) for the [ QRATIO ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = NULL)
        
        testthat::expect_true( init$QRATIO(string1 = one_word, string2 = seq_words[1]) == 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is NOT equal to NULL it returns a value greater than 0 for the [ QRATIO ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = 'utf-8')
        
        testthat::expect_true( init$QRATIO(string1 = one_word, string2 = seq_words[1]) > 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is equal to NULL it returns a 0 value (incorrect result) for the [ WRATIO ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = NULL)
        
        testthat::expect_true( init$WRATIO(string1 = one_word, string2 = seq_words[1]) == 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is NOT equal to NULL it returns a value greater than 0 for the [ WRATIO ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = 'utf-8')
        
        testthat::expect_true( init$WRATIO(string1 = one_word, string2 = seq_words[1]) > 0 )
      })
      
      
      
      testthat::test_that("if the 'decoding' parameter is equal to NULL it returns an error for the [ UWRATIO ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = NULL)
        
        testthat::expect_error( init$UWRATIO(string1 = one_word, string2 = seq_words[1]) )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is NOT equal to NULL it returns a value greater than 0 for the [ UWRATIO ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = 'utf-8')
        
        testthat::expect_true( init$UWRATIO(string1 = one_word, string2 = seq_words[1]) > 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is equal to NULL it returns an error for the [ UQRATIO ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = NULL)
        
        testthat::expect_error( init$UQRATIO(string1 = one_word, string2 = seq_words[1]) )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is NOT equal to NULL it returns a value greater than 0 for the [ UQRATIO ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = 'utf-8')
        
        testthat::expect_true( init$UQRATIO(string1 = one_word, string2 = seq_words[1]) > 0 )
      })
      
      
      
      testthat::test_that("if the 'decoding' parameter is equal to NULL it returns an error for the [ Token_sort_ratio ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = NULL)
        
        testthat::expect_true( init$Token_sort_ratio(string1 = one_word, string2 = seq_words[1]) == 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is NOT equal to NULL it returns a value greater than 0 for the [ Token_sort_ratio ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = 'utf-8')
        
        testthat::expect_true( init$Token_sort_ratio(string1 = one_word, string2 = seq_words[1]) > 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is equal to NULL it returns an error for the [ Token_set_ratio ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = NULL)
        
        testthat::expect_true( init$Token_set_ratio(string1 = one_word, string2 = seq_words[1]) == 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is NOT equal to NULL it returns a value greater than 0 for the [ Token_set_ratio ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = 'utf-8')
        
        testthat::expect_true( init$Token_set_ratio(string1 = one_word, string2 = seq_words[1]) > 0 )
      })
      
      
      # randomly pick a function and validate the score with an ascii character string
      #-------------------------------------------------------------------------------
      
      testthat::test_that("validate for a random method of the FuzzMatcher class that both ascii and non-ascii character strings give the same score", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
        
        init = FuzzMatcher$new(decoding = 'utf-8')
        
        non_ascii = init$Token_set_ratio(string1 = one_word, string2 = seq_words[1])
        
        init1 = FuzzMatcher$new(decoding = NULL)
        
        ascii = init$Token_set_ratio(string1 = valid_one_word, string2 = valid_seq_words[1])
        
        testthat::expect_true( non_ascii == ascii )
      })
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FuzzUtils [ methods ]
      
      
      testthat::test_that("if the 'decoding' parameter is equal to NULL it returns an empty string for the [ Full_process ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.utils")
        
        init = FuzzUtils$new()
        
        testthat::expect_true( init$Full_process(string = one_word, decoding = NULL) == "" )
      })
      
      
      
      testthat::test_that("if the 'decoding' parameter is NOT equal to NULL it returns the initial string after removing the special characters for the [ Full_process ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.utils")
        
        init = FuzzUtils$new()
        
        special_chars = paste0(one_word, "%$&-*")                    # add some special characters
        
        testthat::expect_true( init$Full_process(string = special_chars, decoding = 'utf-8') == one_word )
      })
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FuzzExtract [ methods ]
      
      
      testthat::test_that("if the 'decoding' parameter is equal to NULL it returns a 0 value for both sublists for the [ Extract ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.process")
        
        init = FuzzExtract$new(decoding = NULL)
        
        testthat::expect_true( sum(unlist(lapply(init$Extract(string = one_word, sequence_strings = seq_words), function(x) x[[2]]))) == 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is NOT equal to NULL it returns a value greater than 0 for both sublists for the [ Extract ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.process")
        
        init = FuzzExtract$new(decoding = 'utf-8')
        
        testthat::expect_true( sum(unlist(lapply(init$Extract(string = one_word, sequence_strings = seq_words), function(x) x[[2]] > 0))) == 2 )
      })
      
      
      
      testthat::test_that("if the 'decoding' parameter is equal to NULL it returns a 0 value for both sublists for the [ ExtractBests ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.process")
        
        init = FuzzExtract$new(decoding = NULL)
        
        testthat::expect_true( sum(unlist(lapply(init$ExtractBests(string = one_word, sequence_strings = seq_words), function(x) x[[2]]))) == 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is NOT equal to NULL it returns a value greater than 0 for both sublists for the [ ExtractBests ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.process")
        
        init = FuzzExtract$new(decoding = 'utf-8')
        
        testthat::expect_true( sum(unlist(lapply(init$ExtractBests(string = one_word, sequence_strings = seq_words), function(x) x[[2]] > 0))) == 2 )
      })
      
      
      
      testthat::test_that("if the 'decoding' parameter is equal to NULL it returns a 0 value for both sublists for the [ ExtractWithoutOrder ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.process")
        
        init = FuzzExtract$new(decoding = NULL)
        
        testthat::expect_true( sum(unlist(lapply(init$ExtractWithoutOrder(string = one_word, sequence_strings = seq_words), function(x) x[[2]]))) == 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is NOT equal to NULL it returns a value greater than 0 for both sublists for the [ ExtractWithoutOrder ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.process")
        
        init = FuzzExtract$new(decoding = 'utf-8')
        
        testthat::expect_true( sum(unlist(lapply(init$ExtractWithoutOrder(string = one_word, sequence_strings = seq_words), function(x) x[[2]] > 0))) == 2 )
      })
      
      
      
      testthat::test_that("if the 'decoding' parameter is equal to NULL it returns a 0 value for both sublists for the [ ExtractOne ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.process")
        
        init = FuzzExtract$new(decoding = NULL)
        
        testthat::expect_true( init$ExtractOne(string = one_word, sequence_strings = seq_words)[[2]] == 0 )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is NOT equal to NULL it returns a value greater than 0 for both sublists for the [ ExtractOne ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.process")
        
        init = FuzzExtract$new(decoding = 'utf-8')
        
        testthat::expect_true( init$ExtractOne(string = one_word, sequence_strings = seq_words)[[2]] > 0 )
      })
      
      
      
      testthat::test_that("if the 'decoding' parameter is equal to NULL it returns an error for the [ Dedupe ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.process")
        
        init = FuzzExtract$new(decoding = NULL)
        
        duplicate_sec = c(seq_words, seq_words)
        
        testthat::expect_error( init$Dedupe(contains_dupes = duplicate_sec) )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is NOT equal to NULL it returns a value greater than 0 for both sublists for the [ Dedupe ] function", {
        
        skip_test_if_no_module("fuzzywuzzy.process")
        
        init = FuzzExtract$new(decoding = 'utf-8')
        
        duplicate_sec = c(seq_words, seq_words)       # create a duplicate vector
        
        tmp_res = init$Dedupe(contains_dupes = duplicate_sec)
        
        testthat::expect_true( sum(seq_words %in% tmp_res) == length(seq_words) )
      })
      
      
      
      # randomly pick a function and validate the score with an ascii character string
      #-------------------------------------------------------------------------------
      
      testthat::test_that("validate for a random method of the FuzzExtract class that both ascii and non-ascii character strings give the same score", {
        
        skip_test_if_no_module("fuzzywuzzy.process")
        
        init = FuzzExtract$new(decoding = 'utf-8')
        
        non_ascii = unlist(lapply(init$ExtractBests(string = one_word, sequence_strings = seq_words), function(x) x[[2]]))
        
        init_valid = FuzzExtract$new(decoding = NULL)
        
        ascii = unlist(lapply(init$ExtractBests(string = valid_one_word, sequence_strings = valid_seq_words), function(x) x[[2]]))
      
        testthat::expect_true( sum(non_ascii == ascii) == 2 )
      })
      
      
      #========================================================================================== Error cases for all (three) initialization classes
      
      
      testthat::test_that("if the 'decoding' parameter is not a character string it returns an error for the [ FuzzMatcher ] class", {
        
        skip_test_if_no_module("fuzzywuzzy.fuzz")
      
        testthat::expect_error( FuzzMatcher$new(decoding = list()) )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is not a character string it returns an error for the [ FuzzUtils ] class", {
        
        skip_test_if_no_module("fuzzywuzzy.utils")
        
        init = FuzzUtils$new()
        
        testthat::expect_error( init$Full_process(string = one_word, decoding = list()) )
      })
      
      
      testthat::test_that("if the 'decoding' parameter is not a character string it returns an error for the [ FuzzExtract ] class", {
        
        skip_test_if_no_module("fuzzywuzzy.process")
        
        testthat::expect_error( FuzzExtract$new(decoding = list()) )
      })
    }
  }
}
