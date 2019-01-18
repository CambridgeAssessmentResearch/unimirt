# Applications of unidimensional IRT using the R package mirt

Authors: Tom Benton

## About

An R package to fit unidimensional IRT models and use the results for reviewing items, linking tests and item selection.
The package is designed so that it is possible to fit and explore IRT models with a very small number of commands
in R. Results of analyses can be viewed using a selection of shiny apps.

Much of the functionality in this package is built upon the existing R package *mirt* (Chalmers, 2012; https://cran.r-project.org/web/packages/mirt/index.html).  

**References**

Chalmers, R.P. (2012). mirt: A Multidimensional Item Response Theory Package for the R Environment. 
*Journal of Statistical Software, 48*(6), 1-29.

## Installation
This package can be installed using the devtools package using the commands below.



library(devtools)



install_github("CambridgeAssessmentResearch/unimirt")


## License

The MIT License (MIT)

Copyright (c) 2019 Cambridge Assessment

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
