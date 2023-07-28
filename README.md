randomizr: Tools for random assignment and random sampling
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN
status](https://www.r-pkg.org/badges/version/randomizr)](https://cran.r-project.org/package=randomizr)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/randomizr?color=green)](https://r-pkg.org/pkg/randomizr)
[![Build
status](https://github.com/DeclareDesign/randomizr/workflows/R-CMD-check/badge.svg)](https://github.com/DeclareDesign/randomizr/actions)
[![Code
coverage](https://codecov.io/gh/DeclareDesign/randomizr/branch/master/graph/badge.svg?token=wwi1lF13Se)](https://codecov.io/gh/DeclareDesign/randomizr)

**randomizr** is designed to make conducting field, lab, survey, or
online experiments easier by automating the random assignment process.
Social and lab scientists conducting experiments need a process to
assign individuals or units of observation to treatment or control
wings. Common designs include simple random assignment, complete
randomization, block randomization, cluster randomization, and blocked
cluster randomization. **randomizr** automates all of these processes
and assists scientists in doing transparent, replicable science. We
offer **randomizr** for both
[`R`](https://declaredesign.org/r/randomizr) and
[`Stata`](https://declaredesign.org/stata/randomizr).

### Installing randomizr for R

Installing the latest stable version of **randomizr** in `R`:.

``` r
install.packages("randomizr")
```

### Getting started with randomizr for R

**randomizr** has five main random assignment functions, corresponding
to the common experimental designs listed above. You can read more about
using each of these functions in our [reference
library](https://declaredesign.org/r/randomizr/reference/) or by
clicking on the function names: `simple_ra()`, `complete_ra()`,
`block_ra()`, `cluster_ra()`, and `block_and_cluster_ra()`.

`complete_ra()` (Complete randomization) is the function that will be
most appropriate for a large number of experimental situations: when you
want to assign a fixed `m` units out of a population of `N` units to
treatment:

``` r
library(randomizr)
Z <- complete_ra(N = 100, m = 50)
table(Z)
```

|   0 |   1 |
|----:|----:|
|  50 |  50 |

A more complicated design that, for example, assigns different numbers
of clusters to three different treatments, makes use of `cluster_ra()`
(Cluster randomization):

``` r
# This makes a cluster variable: one unit in cluster "a", two in "b"...
clust_var <- rep(letters[1:15], times = 1:15)

Z <- cluster_ra(
  clusters = clust_var,
  m_each = c(4, 4, 7),
  conditions = c("control", "placebo", "treatment")
  )
table(Z, clust_var)
```

|           |   a |   b |   c |   d |   e |   f |   g |   h |   i |   j |   k |   l |   m |   n |   o |
|:----------|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|
| control   |   1 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   9 |   0 |   0 |   0 |   0 |  14 |  15 |
| placebo   |   0 |   0 |   3 |   0 |   5 |   0 |   7 |   0 |   0 |   0 |   0 |  12 |   0 |   0 |   0 |
| treatment |   0 |   2 |   0 |   4 |   0 |   6 |   0 |   8 |   0 |  10 |  11 |   0 |  13 |   0 |   0 |

For more information about all of **randomizr**’s functionality, please
see our [online
tutorial](https://declaredesign.org/r/randomizr/articles/randomizr_vignette.html)

### randomizr for Stata

Installing the latest stable version of **randomizr** from ssc is easy:

``` r
ssc install randomizr
```

If you would like to install the latest development release directly
from GitHub, run the following code:

``` r
net install randomizr, from(https://raw.githubusercontent.com/DeclareDesign/strandomizr/master/) replace
```

Happy randomizing!
