## Submission Version: 2.0.1

Major version. Nothing is removed or renamed: every exported function,
argument, return type, S3 class and object field carried by 1.0.1 is present,
so existing code continues to work. The package exports 31 objects against
1.0.1's 29, the two additions being `balanced_ra()` and
`balanced_ra_probabilities()`, documented as experimental. They draw
assignment with tight targets: each condition's count lands at the floor or
ceiling of what the probabilities imply while each unit's probability stays
exact. That covers assignment when the probability varies from unit to unit,
which neither `simple_ra()` nor `complete_ra()` handles, and through a
`formula` argument it also holds covariate totals near their targets.
`declare_ra()`, `conduct_ra()` and `obtain_condition_probabilities()` gain
arguments (`prob_unit_each`, `ra_type`, `formula`, `data`), inserted mid-list,
so only callers passing `block_m` or later arguments by position are affected;
none of the reverse dependencies does.

The rest of the changes are internal: blocked assignment moves into C++, the
random sampling functions are unified with their random assignment
counterparts, 264 lines of hand-maintained S3 dispatch are replaced, and a
number of robustness defects are fixed (an NA in `blocks` no longer produces
an assignment shorter than the number of units, among others). NEWS.md
describes the release relative to 1.0.1 in full.

The version is bumped to 2.0.1 rather than 1.1.0 because of one user-visible
change. `strata_rs()` and `strata_and_cluster_rs()` return different (equally
valid) draws for a given seed, since `strata_rs()` now delegates to
`block_ra()`: 1.0.1 implemented the sampling and assignment families
separately and they did not agree with each other, so the draw moves where
1.0.1's own `strata_rs()` and `block_ra()` disagreed, which is strata of odd
size and any design where a per-stratum probability leaves a unit over.
Sampling probabilities are unchanged throughout, as are the realized count
distributions. Every other function reproduces randomizr 1.0.1's output for a
given seed exactly, which is checked in `tests/testthat/test_stream_compat.R`
against output recorded from 1.0.1.

`Rcpp` is added to `Imports` and `LinkingTo`; the package previously contained
plain C only. `Depends` rises to R (>= 3.6.0) for `R_unif_index()`.

## Test environments

* local macOS 26.5 (aarch64), R 4.6.0
* GitHub Actions: macOS release; Windows release; ubuntu devel, release, oldrel-1
* win-builder (devel, release)

## R CMD check results

0 errors | 0 warnings | 0 notes

The package ships three vignettes. Rebuilding them takes about 50 seconds.

## Reverse dependencies

We checked all reverse dependencies with revdepcheck, comparing R CMD check
results across the CRAN and development versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
