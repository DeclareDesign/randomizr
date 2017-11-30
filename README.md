
randomizr: Easy to use tools for common forms of random assignment and sampling
===============================================================================

[![Travis-CI Build Status](https://travis-ci.org/DeclareDesign/randomizr.svg?branch=master)](https://travis-ci.org/DeclareDesign/randomizr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/DeclareDesign/randomizr?branch=master&svg=true)](https://ci.appveyor.com/project/DeclareDesign/randomizr) [![Coverage Status](https://coveralls.io/repos/github/DeclareDesign/randomizr/badge.svg?branch=master)](https://coveralls.io/github/DeclareDesign/randomizr?branch=master)

**randomizr** is designed to make conducting field, lab, survey, or online experiments easier by automating the random assignment process. Social and lab scientists conducting experiments need a process to assign individuals or units of observation to treatment or control wings. Common designs include simple random assignment, complete randomization, block randomization, cluster randomization, and blocked cluster randomization. **randomizr** automates all of these processes and assists scientists in doing transparent, replicable science. We offer **randomizr** for both `R` and `Stata`.

randomizr for R
---------------

### Installing randomizr for R

Installing the latest production-ready version of **randomizr** in `R` is simple.

``` r
install.packages("randomizr")
```

If you would like to use the current development release of **randomizr** (please be aware that development releases may include bugs or syntax changes), run the following code:

``` r
install.packages("devtools")
devtools::install_github("DeclareDesign/randomizr")
```

### Getting started with randomizr for R

**randomizr** has five main random assignment functions, corresponding to the common experimental designs listed above. You can read more about using each of these functions in our [reference library](http://randomizr.declaredesign.org/reference/index.html) or by clicking on the function names: `simple_ra()`, `complete_ra()`, `block_ra()`, `cluster_ra()`, and `block_and_cluster_ra()`.

`complete_ra()` (Complete randomization) is the function that will be most appropriate for a large number of experimental situations: when you want to assign a fixed `m` units out of a population of `N` units to treatment:

``` r
library(randomizr)
Z <- complete_ra(N = 100, m = 50)
table(Z)
```

|    0|    1|
|----:|----:|
|   50|   50|

A more complicated design that, for example, assigns different numbers of clusters to three different treatments, makes use of `cluster_ra()` (Cluster randomization):

``` r
# This makes a cluster variable: one unit in cluster "a", two in "b"...
clust_var <- rep(letters[1:15], times = 1:15)

Z <- cluster_ra(
  clust_var = clust_var,
  m_each = c(4, 4, 7),
  condition_names = c("control", "placebo", "treatment")
  )
table(Z, clust_var)
```

|           |    a|    b|    c|    d|    e|    f|    g|    h|    i|    j|    k|    l|    m|    n|    o|
|-----------|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|
| control   |    0|    0|    3|    0|    0|    0|    7|    8|    9|    0|    0|    0|    0|    0|    0|
| placebo   |    0|    2|    0|    0|    5|    0|    0|    0|    0|    0|    0|    0|   13|   14|    0|
| treatment |    1|    0|    0|    4|    0|    6|    0|    0|    0|   10|   11|   12|    0|    0|   15|

For more information about all of **randomizr**'s functionality, please see our [online tutorial](http://randomizr.declaredesign.org/articles/randomizr_vignette.html)

randomizr for Stata
-------------------

Installing the latest production-ready version of **randomizr** from ssc is easy:

``` r
ssc install randomizr
```

If you would like to install the latest development release directly from GitHub, run the following code:

``` r
net install randomizr, from(https://raw.githubusercontent.com/DeclareDesign/strandomizr/master/) replace
```

Getting started with randomizr for Stata
----------------------------------------

`complete_ra` (complete randomization) is the workhorse function that will be most appropriate for a large number of experimental situations: it assigns m of N units to treatment:

``` r
ssc install randomizr
set obs 100
complete_ra, m(50)
```

A Stata version of our [online tutorial](http://randomizr.declaredesign.org/articles/srandomizr_vignette.html) is also available.

Happy randomizing!
