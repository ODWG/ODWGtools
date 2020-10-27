# ODWGtools

<!-- badges: start -->
[![R build status](https://github.com/ODWG/ODWGtools/workflows/R-CMD-check/badge.svg)](https://github.com/ODWG/ODWGtools/actions)
<!-- badges: end -->

An R package for common tasks associated with the
Outlier Detection working Group, including outlier detection,
unit conversion and time series smoothing.

## How to use this package

### Step 1: install R

This package requires R version 3.5 or higher. Visit the 
R homepage (https://cran.r-project.org/) to get the latest
version.

### Step 2: install the `remotes` package for R

The [`remotes`](https://cran.r-project.org/package=remotes) 
package simplifies the process of installing R packages from 
GitHub Repositories. To install the `remotes` package, open R 
and execute the following command:

```r
install.packages("remotes")
```

### Step 3: install the `ODWGtools` package (this repository)

The `ODWGtools` package (this repository) can be installed by
executing the following command in R:

```r
remotes::install_github("ODWG/ODWGtools")
```

### Step 4: (optional) install additional packages

While they are not strictly required, the `ODWGtools` package can use
tools from some additional packages:

- package `dbscan`: `install.packages("dbscan")`
- package `solitude`: `install.packages("solitude")`


### Step 5: load the `ODWGtools` package

The `ODWGtools` package can now be used just like any other R package.
You can load the package by executing the following R command:

```r
library(ODWGtools)
```
