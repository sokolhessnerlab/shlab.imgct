<!-- space for pandocs in future -->

<!-- README.md is generated from README.Rmd. Please edit that file -->



# shlab.imgct

<!-- badges: start -->
<!-- badges: end -->

The goal of ``shlab.imgct`` is to streamline the processing and analysis
of participant category responses to blocks of images.

## Installation

You can install the ongoing development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sokolhessnerlab/shlab.imgct")
```
## Example

This is a basic example which shows you how to solve a common problem:


```r
library(shlab.imgct)

# Set your path to relevant task data
datapath <- "path/to/data"

# shlab.imgct::clean(datapath) # can be used as convenience
shlab.imgct::clean_qualtrics_export(datapath)

shlab.imgct::validate_all_participants(datapath)

shlab.imgct::categorize(datapath, threshold = 3)

shlab.imgct::analyze(datapath, "categorized_3_valid.tsv")

```
