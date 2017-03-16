[![Travis-CI Build Status](https://travis-ci.org/jmmark/FARScoursera.svg?branch=master)](https://travis-ci.org/jmmark/FARScoursera)

# FARScoursera

This package is submitted as the final assignment for the Building R Packages class
in the Advanced R Programming specialization offered on Coursera.  

The functions in the package were authored by the course professors and
administrators, while documentation, raw data, and associated package building was provided
by the students.  

FARScoursera itself, as a package, is quite simple (as building i it is a teaching tool).  It provides a suite 
of functions for interacting with the [FARS](https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars) 
database from the [NHTSA](https://www.nhtsa.gov/).  

The package includes three years of the FARS data in the `extdata` directory, as well as functions to help
load and analyze the data.  

See the included vignette for a detailed walkthrough of the package  

## Installation

You can install FARScoursera from github with:

```R
# install.packages("devtools")
devtools::install_github("FARScoursera/jmmark")
```


