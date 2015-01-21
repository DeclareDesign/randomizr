<!-- README.md is generated from README.Rmd. Please edit that file -->
randomizr is designed to make conducting field, lab, survey, or online experiments easier by automating the random assignment process.

Installation from github is easy. In r:

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

Happy randomizing!
