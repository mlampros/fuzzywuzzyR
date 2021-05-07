
## fuzzywuzzyR 1.0.4

* I've added the *CITATION* file in the *inst* directory


## fuzzywuzzyR 1.0.3

I added an exception in the additional tests, to avoid Solaris OS throw an error if python is not available


## fuzzywuzzyR 1.0.2

I added the *decoding* parameter to the following classes : *FuzzExtract*, *FuzzMatcher* and *FuzzUtils*. The *decoding* parameter does not apply to the *GetCloseMatches* and *SequenceMatcher* classes, because there isn't any *force_ascii* parameter in the *difflib* python library. The *decoding* parameter applies only to python 2 configurations, as in python 3 character strings are decoded to unicode by default.
For reference, see the following github issue : https://github.com/mlampros/fuzzywuzzyR/issues/3


## fuzzywuzzyR 1.0.1

I added links to the github repository (master repository, issues).


## fuzzywuzzyR 1.0.0




