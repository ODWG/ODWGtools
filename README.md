# WQPtools

[![Build Status](https://travis-ci.org/mkoohafkan/wqptools.svg)](https://travis-ci.org/mkoohafkan/wqptools)

## Installation instructions

1. Install the R package `devtools`:

```
install.packages("devtools")
```

2. Install the development version from Github:

```
devtools::install_github('mkoohafkan/wqptools')
```

*OR* Clone the repository on your machine and load it without installing:

```
devtools::load_all('path/to/repository/folder')
# also install dependencies
install.packages(c("dplyr", "purrr", "lubridate", "slider"))
```

3. Install optional dependencies and helper packages:

- package `dbscan`: `install.packages('dbscan')`
- package `solitude`: install.packages('solitude')

4. Play!

- R/outliers.r - main outlier detection functions.
- data/bld.rda - sample dataset, load with `data(bdl)`.
- vignettes/outlier-analysis.Rmd - reproducible example of using the outlier detection functions.
