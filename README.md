
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RF

<!-- badges: start -->

<!-- badges: end -->

In plant breeding, it is common to leave one population out when
training a machine learning model. This package allows the user to run
random forest using one population as the test set, as well as with
other, more common, CV strategies (e.g. leave-one-out, 5-fold, 10-fold,
etc.).

## Installation

You can install the RF package like so:

``` r
devtools::install_github("nad7wf/RF")
```

## Example

``` r
library(RF)
library(magrittr)

# Simulate some data.
set.seed(76123)
row_num <- 1000

ex <- data.frame(
    Y = runif(row_num, 30, 150),
    GE_ID = runif(row_num, 1e8, 2e8),
    Parent_A = sample(LETTERS[1:4], row_num, replace = TRUE),
    Parent_B = sample(LETTERS[5:8], row_num, replace = TRUE)
) %>%
    cbind(replicate(10, runif(row_num, 0, 1)) %>%
              as.data.frame() %>%
              magrittr::set_names(paste0("Marker", seq(10))))

# Run random forest with specified fold strategy.
result <- RF(ex, "family")
```

``` r
lapply(result, head)
#> $preds
#>        GE_ID Pop     Pred
#> 16 194823653  CH 97.65183
#> 19 103237470  CH 87.43266
#> 22 116675436  CH 99.11543
#> 24 121886422  CH 78.86422
#> 53 133080052  CH 93.98528
#> 55 195644271  CH 95.33380
#> 
#> $cc
#>   Fold          Cor
#> 1   CH -0.042957611
#> 2   DH -0.129824136
#> 3   AH  0.326906975
#> 4   DG -0.007008195
#> 5   DE -0.060222054
#> 6   AF -0.047599874
```
