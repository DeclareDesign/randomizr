# randomizr 2.0.0

This release is a significant internal restructuring. Every public interface is unchanged: function names, parameter names, return types, S3 classes, and object field names. Existing code that works with randomizr 1.x continues to work without modification, and with one documented exception it returns the same assignments.

## New: assignment with heterogeneous probabilities (experimental)

`prob_ra()` and `prob_ra_probabilities()` assign units when the probability of assignment varies from unit to unit, holding the number assigned to each condition as close to its target as arithmetic allows. They fill the gap between `simple_ra()`, which honours unit-varying probabilities but lets the number treated wander, and `complete_ra()`, which fixes the number treated but requires every unit to share the same probability.

Three things hold at once, and all three are guaranteed rather than approached: every unit receives exactly one condition; each unit's probability of each condition is exactly the probability supplied; and each condition's count is the floor or the ceiling of its expected count. With `blocks`, the tight counts are the within-block ones. This closes issue #35, load balancing across blocks, which earlier releases listed as out of scope.

The assignment is drawn by the cube method of Deville and Tillé (2004), specialised to this problem. The function originates in Macartan Humphreys's `probra` package, which introduced the design and the two motivating examples in the documentation; the algorithm here is different, and holds the counts tight for any number of arms rather than for one.

`prob_ra()` is marked experimental: its interface may change, and it does not yet participate in `declare_ra()`, so `conduct_ra()` and `obtain_condition_probabilities()` do not accept a `prob_ra` design.

## Reproducibility of randomizr 1.x seeds

Assignments are reproducible across the 1.x boundary. `set.seed(s)` followed by any of the assignment or sampling functions returns what randomizr 1.0.1 returned for that seed, so pre-registrations, replication scripts, and archived analyses that pinned a seed are unaffected by upgrading.

This is not automatic for a rewrite, because the random number stream depends on how many uniforms are drawn and in what order, not only on the sampling design. Two-arm blocked assignment now runs in C++ (see Performance), and the implementation deliberately reproduces the draw sequence of `sample(rep(conditions, c(n - m, m)))` per block, including the separate draw that decides the treated count when `n * prob` is not an integer, in the order 1.x performed them. `tests/testthat/test_stream_compat.R` records the output of randomizr 1.0.1 for a range of designs and fails if any of it moves.

**The one exception is `strata_rs()` and `strata_and_cluster_rs()` with strata of odd size**, where assignments differ from 1.x. This follows from the RS/RA unification described below: `strata_rs()` now delegates to `block_ra()`, which uses `complete_ra()` where 1.x used `complete_rs()`, and those two functions disagreed with each other at odd sizes in 1.x itself. Sampling probabilities are unchanged and correct; only the particular draw for a given seed differs. Even-sized strata are unaffected.

## Performance

Two-arm blocked assignment is the hot path in simulation work, where the same design is redrawn thousands of times, and it now runs in a single C++ call rather than one R-level `complete_ra()` call per block. `randomizr` gains `Rcpp` in `Imports` and `LinkingTo`; `src/block_assign.cpp` is new.

Measured on an Apple M4 Pro under R 4.6.0, `block_ra()` on a two-arm design:

| design | 1.0.1 | 2.0.0 |
|---|---|---|
| N = 20,000, 2,000 blocks | 8.9 ms | 1.1 ms |
| N = 100,000, 10,000 blocks | 46.0 ms | 6.5 ms |

The gain comes from removing an R-level function call per block rather than from drawing fewer random numbers. Drawing the same numbers as 1.x costs roughly a third of the achievable speedup and is what makes seeds reproduce, which is the better trade for a package whose output is cited in pre-registrations.

## Internal restructuring

### RS functions unified with RA counterparts

The random sampling (RS) family has always been a two-condition special case of the random assignment (RA) family: sample/not-sampled is equivalent to assignment with `conditions = c(0, 1)`. The RS implementations previously duplicated RA logic in parallel. They now delegate directly:

- `strata_rs()` and `strata_rs_probabilities()` delegate to `block_ra()` and `block_ra_probabilities()` with `conditions = c(0, 1)`. The `strata`/`n`/`strata_n`/`strata_prob` parameters map to `blocks`/`m`/`block_m`/`block_prob` internally. This is the change responsible for the one reproducibility exception noted above: `complete_rs()` and `complete_ra()` selected different assignments at odd sizes in 1.x, and `strata_rs()` now takes the `complete_ra()` result.
- `simple_rs()` and `simple_rs_probabilities()` already delegated to `simple_ra()` in 1.x. No change.
- `cluster_rs()` and `strata_and_cluster_rs()` already delegated internally. No change.

### S3 dispatch boilerplate replaced

`generated_methods.R` previously contained 264 lines of hand-maintained S3 dispatch: each `ra_function.*`, `ra_probabilities.*`, `rs_function.*`, and `rs_probabilities.*` method explicitly unpacked every slot of the declaration object and re-passed them by name to the underlying function. These methods were originally auto-generated by a script in `zzz.R` (which was then commented out), meaning any parameter addition required manual updates in two places.

The 22 methods are now each a single `do.call()` using `mget()` to extract only the formals the target function accepts from the declaration environment:

```r
ra_function.ra_complete <- function(this)
  do.call(complete_ra, mget(names(formals(complete_ra)), this, ifnotfound = list(NULL)))
```

`mget()` fetches only the named bindings, so the `delayedAssign()` slots on declaration objects (`probabilities_matrix`, `ra_type`, `cleaned_arguments`) are not triggered as a side effect of dispatch. The commented-out generation script in `zzz.R` has been removed.

### Typo corrected

Error message "The probabilties of assignment..." → "The probabilities of assignment..." in `helper_functions.R`.

## GitHub issues addressed

**#99, correctness of unequal-probability `simple_ra`.** The issue claimed that `simple_ra()` uses `base::sample()` in a way that does not maintain correct selection probabilities for unequal-probability designs. This does not apply to the current implementation: `simple_ra()` uses `vsample()` (a C function added by Neal Fultz) which performs correct inverse-CDF multinomial sampling. Empirical simulation confirms that assignment probabilities match specified `prob_each` to within Monte Carlo error. The terminology concerns in #99 (the package's use of "complete random sampling" vs the probability-sampling-theory definition) are a documentation matter, not a code defect.

**#35, load balancing across blocks.** Out of scope for this release.

## ri2 compatibility

The ri2 package (>= 0.4.1) depends on randomizr for `declare_ra()`, `conduct_ra()`, `obtain_condition_probabilities()`, `obtain_permutation_matrix()`, `obtain_num_permutations()`, the five `ra_*` S3 class names, and the `probabilities_matrix`, `blocks`, and `clusters` fields on declaration objects. All are unchanged. The ri2 test suite passes without modification against randomizr 2.0.0.

# randomizr 1.0.1

* CRAN compliance

# randomizr 1.0.0

* Documentation fix

# randomizr 0.24.0

* Documentation fix

# randomizr 0.22.0

* Added a `NEWS.md` file to track changes to the package.
* Removed suggests dependency of blockTools per Prof. Ripley email (the package was removed from CRAN)
* Added Graeme Blair as a contributor
* Small bug fixes
* Added permutation support for random sampling functions
