<!-- README.md is generated from README.Rmd. Please edit that file -->
randomizr is designed to make conducting field, lab, survey, or online experiments easier by automating the random assignment process.

Installation from CRAN is easy:

``` r
install.packages("randomizr")
```

If you'd like to install the most current development release, use the following code:

``` r
install.packages("devtools")
devtools::install_github("acoppock/randomizr")
```

There are four functions in randomizr: simple\_ra(), complete\_ra(), block\_ra(), and cluster\_ra(), corresponding to common experimental designs.

complete\_ra() is the workhorse function that will be most appropriate for a large number of experimental situations: it assigns m of N units to treatment:

``` r
library(randomizr)
Z <- complete_ra(N=100, m=50)
table(Z)
#> Z
#>  0  1 
#> 50 50
```

A more complicated design that, for example, assigns different numbers of clusters to three different treatments can be accomodated like this:

``` r
# This makes a cluster variable: one unit in cluster "a", two in "b"...
clust_var <- rep(letters, times=1:26)

Z <- cluster_ra(clust_var=clust_var, m_each=c(7, 7, 12),
                condition_names=c("control", "placebo", "treatment"))
table(Z, clust_var)
#>            clust_var
#> Z            a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u
#>   control    0  0  0  4  0  0  0  0  0  0  0 12  0  0  0  0  0 18 19  0  0
#>   placebo    0  2  3  0  5  0  7  0  0  0 11  0  0  0  0 16  0  0  0  0  0
#>   treatment  1  0  0  0  0  6  0  8  9 10  0  0 13 14 15  0 17  0  0 20 21
#>            clust_var
#> Z            v  w  x  y  z
#>   control   22  0 24  0 26
#>   placebo    0 23  0  0  0
#>   treatment  0  0  0 25  0
```

Happy randomizing!
