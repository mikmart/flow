
<!-- README.md is generated from README.Rmd. Please edit that file -->
flow
====

The goal of **flow** is to make it easy to extract numbers for population defining flowcharts with minimal edits to existing **dplyr** workflows.

Note that this is an extremely early stage (practically idea concept) package.

Installation
------------

You can install the development version with:

``` r
# install.packages("devtools")
devtools::install_github("mikmart/flow")
```

Example
-------

You can `flow::track` the number of rows dropped with `dplyr::filter`:

``` r
library(dplyr)

filtered <- mtcars %>%
  flow::track("1974 Motor Trend cars") %>%
  filter(mpg >= 20, mpg <= 30) %>%
  filter(cyl < 6)
```

The result looks like a regular tbl\_df ...

``` r
filtered
#> # A tibble: 7 x 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#> 2  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#> 3  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 4  21.5     4  120.    97  3.7   2.46  20.0     1     0     3     1
#> 5  27.3     4   79     66  4.08  1.94  18.9     1     1     4     1
#> 6  26       4  120.    91  4.43  2.14  16.7     0     1     5     2
#> 7  21.4     4  121    109  4.11  2.78  18.6     1     1     4     2
```

... but the `filter` "history" can be extracted.

``` r
filtered %>% 
  flow::chart()
#> # A tibble: 3 x 3
#>   step                  included excluded
#>   <chr>                    <int>    <int>
#> 1 1974 Motor Trend cars       32       NA
#> 2 mpg >= 20 & mpg <= 30       10       22
#> 3 cyl < 6                      7        3
```
