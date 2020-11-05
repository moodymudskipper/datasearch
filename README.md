
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datasearch

*{datasearch}* is a one function package that can be used to find
datasets observing specific conditions, either in a specific package, a
list of packages, or all installed packages.

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/datasearch")
```

## Examples

``` r
library(datasearch)

#~~~~~~~~~~~~~ all data sets from package "dplyr" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dplyr_all <-
  datasearch("dplyr")

View(dplyr_all)
```

![dplyr](https://github.com/moodymudskipper/datasearch/raw/master/inst/images/datasearch1.gif)

``` r
#~~~~~~~~~~~~~ datasets from package "datasets" restricted by condition ~~~~~~~~

datasets_ncol5 <-
  datasearch("datasets", filter =  ~is.data.frame(.) && ncol(.) == 5)

View(datasets_ncol5)
```

![datasets](https://github.com/moodymudskipper/datasearch/raw/master/inst/images/datasearch2.gif)

``` r
#~~~~~~~~~~~~~ all datasets from all installed packages, no restriction ~~~~~~~~

# might take more or less time, depends what you have installed
all_datasets <- datasearch()

View(all_datasets)

# subsetting the output
my_subset <- subset(
  all_datasets, 
  class1 == "data.frame" &
    grepl("treatment", names_collapsed) &
    nrow < 100
)

View(my_subset)
```

![all](https://github.com/moodymudskipper/datasearch/raw/master/inst/images/datasearch3.gif)
