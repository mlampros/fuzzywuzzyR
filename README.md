
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/fuzzywuzzyR)](http://cran.r-project.org/package=fuzzywuzzyR)
[![Travis-CI Build Status](https://travis-ci.org/mlampros/fuzzywuzzyR.svg?branch=master)](https://travis-ci.org/mlampros/fuzzywuzzyR)
[![codecov.io](https://codecov.io/github/mlampros/fuzzywuzzyR/coverage.svg?branch=master)](https://codecov.io/github/mlampros/fuzzywuzzyR?branch=master)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mlampros/fuzzywuzzyR?branch=master&svg=true)](https://ci.appveyor.com/project/mlampros/fuzzywuzzyR/branch/master)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/fuzzywuzzyR?color=blue)](http://www.r-pkg.org/pkg/fuzzywuzzyR)



## fuzzywuzzyR
<br>

The **fuzzywuzzyR** package is a fuzzy string matching implemenation of the [fuzzywuzzy](https://github.com/seatgeek/fuzzywuzzy) python package. It uses the [Levenshtein Distance](https://en.wikipedia.org/wiki/Levenshtein_distance) to calculate the differences between sequences. More details on the functionality of fuzzywuzzyR can be found in the [blog-post](http://mlampros.github.io/2017/04/13/fuzzywuzzyR_package/) and in the package Vignette.


<br>

### **System Requirements**

<br>

* Python (>= 2.4)

* difflib

* fuzzywuzzy ( >=0.15.0 )

* [python-Levenshtein](https://github.com/ztane/python-Levenshtein/) ( >=0.12.0, optional, provides a 4-10x speedup in String Matching, though may result in differing results for certain cases)

<br>

Before the installation of any python modules one should check the python-configuration using :

<br>

```R
reticulate::py_config()

```
<br>

All modules should be installed in the default python configuration (the configuration that the R-session displays as default), otherwise errors will occur during package installation. 

<br>

#### **Debian/Ubuntu/Fedora**

<br>

**Python2**

```R
sudo apt-get install python-pip
sudo pip install --upgrade pip
pip install fuzzywuzzy
pip install python-Levenshtein
```
<br>

**Python 3**

```R
sudo apt-get install python3-pip
sudo pip3 install --upgrade pip
pip3 install fuzzywuzzy
pip3 install python-Levenshtein
```
<br><br>



#### **Macintosh OSX** 
<br>

```R
sudo easy_install pip
sudo pip install fuzzywuzzy
sudo pip install python-Levenshtein
```
<br>

#### **Windows OS**

<br>

* Download of [get-pip.py](https://bootstrap.pypa.io/get-pip.py)
* Update of the Environment variables ( Control Panel >> System and Security >> System >> Advanced system settings >> Environment variables >> System variables >> Path >> Edit ) by adding ( for instance in case of python 2.7 ) : 
```R
C:\Python27;C:\Python27\Scripts
```

* Install the [Build Tools for Visual Studio](https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2017)
* Open the *Command prompt* and use the following commands:
```R
pip install fuzzywuzzy
pip install python-Levenshtein
```

<br>

### **Installation of the fuzzywuzzyR package**

<br>

To install the package from CRAN use, 

```R

install.packages('fuzzywuzzyR')


```
<br>

and to download the latest version from Github use the *install_github* function of the devtools package,
<br><br>

```R

devtools::install_github(repo = 'mlampros/fuzzywuzzyR')

```
<br>
Use the following link to report bugs/issues,
<br><br>

[https://github.com/mlampros/fuzzywuzzyR/issues](https://github.com/mlampros/fuzzywuzzyR/issues)

<br>
