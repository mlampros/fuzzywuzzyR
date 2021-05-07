
#...........
# Input data
#...........

s1 = ' It was a dark and stormy night. I was all alone sitting on a red chair.'

s2 = ' It was a murky and stormy night. I was all alone sitting on a crimson chair.'

word = 'Fra Bagg'

vec_getclose = c('Frodo Baggins', 'Tom Sawyer', 'Bilbo Baggin')

st1 = "Atlanta Falcons"

st2 = "New York Jets"


#............................
# non-ascii character strings     [ https://github.com/mlampros/fuzzywuzzyR/issues/3 ]
# tests begin from line 1745      [ excluded the 'SequenceMatcher()' and 'GetCloseMatches()', which belong to the 'difflib' python package and character strings do not need to be encoded ]
#............................

one_word = "一个词"

seq_words = c("两个字", "序列词")

valid_one_word = "abc"

valid_seq_words = c("dbe", "fgc")
