randomizr: Tools for random assignment and random sampling
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![CRAN
status](https://www.r-pkg.org/badges/version/randomizr)](https://cran.r-project.org/package=randomizr)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/randomizr?color=green)](https://r-pkg.org/pkg/randomizr)
[![Build
status](https://github.com/DeclareDesign/randomizr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DeclareDesign/randomizr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/DeclareDesign/randomizr/graph/badge.svg)](https://app.codecov.io/gh/DeclareDesign/randomizr)
[![Replications](https://softwarecite.com/badge/randomizr)](https://softwarecite.com/package/randomizr)

**randomizr** generates random assignments for common experimental
designs, including simple random assignment, complete random assignment,
block random assignment, and cluster random assignment. A new function,
`balanced_ra()`, is experimental: it draws assignment with tight targets
while keeping each unit’s probability exact.

### Installing randomizr

Use the following to install the latest CRAN release of **randomizr**:

``` r
install.packages("randomizr")
```

### Getting started with randomizr

**randomizr** has four main random assignment functions, corresponding
to the common experimental designs listed above. You can read more about
using each of these functions in our [reference
library](https://declaredesign.org/r/randomizr/reference/) or by
clicking on the function names: `simple_ra()`, `complete_ra()`,
`block_ra()`, and `cluster_ra()`. An additional experimental function,
`balanced_ra()`, is included from version 2.0.1; see the [introduction
article](https://declaredesign.org/r/randomizr/articles/balanced_ra.html).

`complete_ra()`: Under complete random assignment, we assign a fixed `m`
units out of a population of `N` units to treatment:

``` r
library(randomizr)
Z <- complete_ra(N = 100, m = 50)
table(Z)
```

|   0 |   1 |
|----:|----:|
|  50 |  50 |

`cluster_ra()`: Under cluster random assignment, whole clusters of units
(like all the students in a classroom or everyone living in the same
household) are assigned to treatment conditions together.

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
| control   |   1 |   0 |   0 |   0 |   5 |   0 |   0 |   8 |   9 |   0 |   0 |   0 |   0 |   0 |   0 |
| placebo   |   0 |   2 |   0 |   0 |   0 |   6 |   0 |   0 |   0 |   0 |  11 |   0 |  13 |   0 |   0 |
| treatment |   0 |   0 |   3 |   4 |   0 |   0 |   7 |   0 |   0 |  10 |   0 |  12 |   0 |  14 |  15 |

`block_ra()`: Under block random assignment, complete random assignment
is used within blocks.

``` r
# This makes a cluster variable: one unit in cluster "a", two in "b"...
block_var <- rep(letters[1:10], times = 4)

Z <- block_ra(
  blocks = block_var
  )
table(Z, block_var)
```

|     |   a |   b |   c |   d |   e |   f |   g |   h |   i |   j |
|:----|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|
| 0   |   2 |   2 |   2 |   2 |   2 |   2 |   2 |   2 |   2 |   2 |
| 1   |   2 |   2 |   2 |   2 |   2 |   2 |   2 |   2 |   2 |   2 |

`balanced_ra()`: Under balanced assignment, units are assigned to ensure
expected totals are hit tightly.

``` r
# This assigns exactly three of six units to treatment with either 1 assigned in block 1 and 2 in block 2 or 2 in block 1 and 1 in block 2
set.seed(1)
blocks <- c("a", "a", "a", "b", "b", "b")

table(balanced_ra(blocks = blocks), blocks)
```

|     |   a |   b |
|:----|----:|----:|
| 0   |   2 |   1 |
| 1   |   1 |   2 |

For more information about all of **randomizr**’s functionality, please
see our [online
tutorial](https://declaredesign.org/r/randomizr/articles/randomizr_vignette.html)

Happy randomizing!
