## Submission Version: 2.0.0

Major version. Nothing is removed or altered: every exported function,
argument, return type, S3 class and object field carried by 1.0.1 is present
and unchanged, so existing code continues to work. The package exports 31
objects against 1.0.1's 29, the two additions being `balanced_ra()` and
`balanced_ra_probabilities()`, documented as experimental. They assign units
when the probability of assignment varies from unit to unit, which neither
`simple_ra()` nor `complete_ra()` covers.

The rest of the changes are internal: blocked assignment moves into C++ for
two arms and for three or more, the random sampling functions are unified with
their random assignment counterparts, and 264 lines of hand-maintained S3
dispatch are replaced by `do.call()` over `mget()`.

The version is bumped to 2.0.0 rather than 1.1.0 because of one user-visible
change. `strata_rs()` and `strata_and_cluster_rs()` return different (equally
valid) assignments for a given seed, since `strata_rs()` now delegates to
`block_ra()`: 1.0.1 implemented the sampling and assignment families
separately and they did not agree with each other, so the draw moves where
1.0.1's own `strata_rs()` and `block_ra()` disagreed, which is strata of odd
size and any use of `strata_prob`. Assignment and sampling probabilities are
unchanged throughout, as are the realized count distributions. Every other
function reproduces randomizr 1.0.1's output for a given seed exactly, which is
checked in `tests/testthat/test_stream_compat.R` against output recorded from
1.0.1.

`Rcpp` is added to `Imports` and `LinkingTo`. The package previously contained
plain C only.

## Test environments

* local macOS (R release)
* win-builder (devel, release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

We checked 13 reverse dependencies (10 strong, 3 from Suggests), comparing
R CMD check results across the CRAN and development versions of this package.
The check predates `balanced_ra()` and the C++ assignment kernels. Neither
changed an export, a signature or a return type, and the kernels' output was
verified identical to randomizr 1.0.1's across 918 configurations of block
size, block count, probability, arm count and argument form, so a reverse
dependency sees the values 1.0.1 produced.

 * We saw 0 new problems
 * We failed to check 1 package

`estimatr` could not be checked locally because it links a Fortran runtime that
is not installed on the check machine; it fails to install identically under
the CRAN and development versions of randomizr, so the failure is unrelated to
this submission. Its test files that call randomizr were run separately against
this version, with 166 passing assertions and no failures.
