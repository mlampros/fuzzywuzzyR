
## fuzzywuzzyR 1.0.6

* I re-generated the documentation as requested from the CRAN Team. Moreover, I modified the README.md file by removing Python 2 and also the installation instructions for Mac OSX and Windows which were outdated. Users of the `fuzzywuzzyR` package will find the required information for the Python configuration in the [reticulate](https://github.com/rstudio/reticulate) Github repository.


## fuzzywuzzyR 1.0.5

* I've added a 'packageStartupMessage' informing the user in case of the error 'attempt to apply non-function' that s/he has to use the 'reticulate::py_config()' before loading the package (in a new R session)


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

