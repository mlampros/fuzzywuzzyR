
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

# data

s1 = ' It was a dark and stormy night. I was all alone sitting on a red chair.'

s2 = ' It was a murky and stormy night. I was all alone sitting on a crimson chair.'

word = 'Fra Bagg'

vec_getclose = c('Frodo Baggins', 'Tom Sawyer', 'Bilbo Baggin')

st1 = "Atlanta Falcons"

st2 = "New York Jets"

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


testthat::test_that("the method returns a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_true( init$Partial_token_set_ratio(string1 = st1, string2 = st2, force_ascii = TRUE, full_process = TRUE) == 31 )
})


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


testthat::test_that("the method returns a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_true( init$Partial_token_sort_ratio(string1 = st1, string2 = st2, force_ascii = TRUE, full_process = TRUE) == 31 )
})


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

testthat::test_that("the method returns a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_true( init$Ratio(string1 = st1, string2 = st2) == 21 )
})


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


testthat::test_that("the method returns a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_true( init$QRATIO(string1 = st1, string2 = st2, force_ascii = TRUE) == 29 )
})



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


testthat::test_that("the method returns a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_true( init$WRATIO(string1 = st1, string2 = st2, force_ascii = TRUE) == 29 )
})



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

testthat::test_that("the method returns a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_true( init$UWRATIO(string1 = st1, string2 = st2) == 29 )
})




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

testthat::test_that("the method returns a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_true( init$UQRATIO(string1 = st1, string2 = st2) == 29 )
})



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

testthat::test_that("the method returns a numeric value", {

  skip_test_if_no_module("fuzzywuzzy.fuzz")

  init = FuzzMatcher$new()

  testthat::expect_true( init$Partial_ratio(string1 = st1, string2 = st2) == 23 )
})



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

