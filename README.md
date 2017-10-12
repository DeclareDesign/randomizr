
[![Travis-CI Build Status](https://travis-ci.org/DeclareDesign/randomizr.svg?branch=master)](https://travis-ci.org/DeclareDesign/randomizr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/DeclareDesign/randomizr?branch=master&svg=true)](https://ci.appveyor.com/project/DeclareDesign/randomizr) [![Coverage Status](https://coveralls.io/repos/github/DeclareDesign/randomizr/badge.svg?branch=master)](https://coveralls.io/github/DeclareDesign/randomizr?branch=master)

randomizr is designed to make conducting field, lab, survey, or online experiments easier by automating the random assignment process.

Installation from CRAN is easy:

``` r
install.packages("randomizr")
```

If you'd like to install the most current development release, use the following code:

``` r
install.packages("devtools")
devtools::install_github("DeclareDesign/randomizr")
```

There are five main random assignment functions in randomizr: `simple_ra()`, `complete_ra()`, `block_ra()`, `cluster_ra()`, and `block_and_cluster_ra()`, which correspond to common experimental designs.

`complete_ra()` is the workhorse function that will be most appropriate for a large number of experimental situations: it assigns m of N units to treatment:

``` r
library(randomizr)
Z <- complete_ra(N = 100, m = 50)
table(Z)
#> Z
#>  0  1 
#> 50 50
```

A more complicated design that, for example, assigns different numbers of clusters to three different treatments can be accomodated like this:

``` r
# This makes a cluster variable: one unit in cluster "a", two in "b"...
clust_var <- rep(letters, times = 1:26)

Z <- cluster_ra(
  clust_var = clust_var,
  m_each = c(7, 7, 12),
  condition_names = c("control", "placebo", "treatment")
  )
table(Z, clust_var)
#>            clust_var
#> Z            a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u
#>   control    0  0  0  4  0  0  7  0  9  0 11  0 13  0  0  0 17 18  0  0  0
#>   placebo    0  2  0  0  5  6  0  8  0  0  0 12  0  0 15  0  0  0  0  0  0
#>   treatment  1  0  3  0  0  0  0  0  0 10  0  0  0 14  0 16  0  0 19 20 21
#>            clust_var
#> Z            v  w  x  y  z
#>   control    0  0  0  0  0
#>   placebo    0  0 24  0  0
#>   treatment 22 23  0 25 26
```

Happy randomizing!
