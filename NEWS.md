# randomizr 2.0.1

`check_inputs = FALSE` works for blocked and clustered assignment. It never did: the validation also derived `num_arms` and `conditions` when the caller left them implicit, so skipping the validation skipped the derivation, and `block_ra()`, `cluster_ra()` and `block_and_cluster_ra()` all failed with "invalid first argument" or "invalid 'times' argument" whatever else was supplied. The derivation is separate now and runs on both paths. Draws are unaffected: across twenty-one designs, `check_inputs = FALSE` returns byte-identical assignments to `check_inputs = TRUE` on the same seed.

`block_assign_cpp()` refuses a block count or probability outside its range instead of writing outside its buffer. Each of its three modes fills the assignment vector by counting down from the block size, so `block_m` larger than a block started the loop at a negative index. The first call returned a wrong answer, and a later one brought R down with a bus error. Only reachable with `check_inputs = FALSE`, and reachable there since 1.0.1.

`declare_ra(formula = )` now resolves the balancing matrix when the design is declared, and carries it. Previously the declaration stored only the formula, and `conduct_ra()` looked its variables up on every draw by walking the call stack out to the global environment. An object of the same name anywhere on that stack, and the global environment is always on it, beat the environment the formula was written in, so a design declared inside a function could balance a covariate the analyst never named, without an error. `balanced_ra()` called directly now searches `environment(formula)` first, as `stats::lm()` does, before the caller's frame and the stack; a data mask still resolves, so `declare_assignment(Z = balanced_ra(formula = ~ x))` is unaffected.

Two-arm cube-on-X is linear in the number of units again. The flight phase rebuilt its whole queue of fractional units after every step, though only the units in the current window can have moved, which made it quadratic. At 16,000 units a draw takes about 8 milliseconds rather than about 104. Assignments are unchanged: the same seed gives the same draw as before.

`balanced_ra()` gains `formula` and `data` for two-arm cube-on-X (`~ x + B`). The intercept is the count constraint. `blocks` stays the partitioned count path; passing both errors. `formula` with `prob_unit_each` errors. `declare_ra(..., formula = )` selects the balanced path. When `formula` is `NULL` the count-tight C++ is unchanged. See `vignette("balanced_ra_covariates")`.

`declare_ra()` now accepts balanced assignment. Set `ra_type = "balanced"` or supply `prob_unit_each`. `conduct_ra()` and `obtain_condition_probabilities()` then dispatch to `balanced_ra()` and `balanced_ra_probabilities()`. Existing calls such as `declare_ra(N, prob = 0.5)` remain complete assignment. A varying `prob_unit` without `ra_type` still errors as it did for complete assignment; it does not silently take the cube path. Count arguments (`m`, `block_m`, and the rest) are refused on the balanced path. `num_arms` or `conditions` without probabilities expand to equal-probability balanced assignment, as they do for complete assignment. `obtain_num_permutations()` reports `Inf` for these designs (the cube support is not enumerated). `obtain_permutation_probabilities()` errors: the support is not listed and the assignments are not equally likely.

`vignette("balanced_ra")` walks through one two-arm draw as a four-panel figure whose titles state why a cell is driven to 0, not only the result, and one four-unit, three-arm draw from `prob_unit_each` rows (0.2, 0.4, 0.4), (0.4, 0.3, 0.3), (0.6, 0.2, 0.2), (0.8, 0.1, 0.1).

`vignette("balanced_ra_hc2")` re-simulates inverse-probability-weighted HC2 coverage under the current `balanced_ra()` implementation. Assignment is dependent; on the two-arm, blocked, and three-arm designs in that vignette, 95 percent intervals sit near 95 percent.

`vignette("balanced_ra_speed")` times `balanced_ra()` against `complete_ra()`, `block_ra()`, and `block_and_cluster_ra()` on the equal-probability cases those specialized functions already handle.

`%||%` is used internally when a scalar `prob_unit` needs `N` inferred from `blocks` or `clusters`. The operator is defined in the package so the call works on R < 4.4, where it is not yet in base.

`balanced_ra()` pairs leftovers across blocks so that two-arm blocked designs are tight within each block and overall. Two districts of three villages with `prob_unit = 0.5` always treat exactly three villages, and each district gets one or two. Independently landing each block had let the total wander. With three or more arms and `blocks`, tightness remains within each block; overall counts need not be tight.

`N` is the first argument and `prob_unit` defaults to 0.5, so `balanced_ra(4)` is complete assignment of four units. A vector passed as `N` is refused: use `prob_unit` for probabilities.

# randomizr 2.0.0

This release is a significant internal restructuring. Every public interface is unchanged: function names, parameter names, return types, S3 classes, and object field names. Existing code that works with randomizr 1.x continues to work without modification, and with one documented exception it returns the same assignments.

## New: assignment with heterogeneous probabilities (experimental)

`balanced_ra()` and `balanced_ra_probabilities()` assign units when the probability of assignment varies from unit to unit, holding the number assigned to each condition as close to its target as arithmetic allows. They fill the gap between `simple_ra()`, which honors unit-varying probabilities but lets the number treated wander, and `complete_ra()`, which fixes the number treated but requires every unit to share the same probability.

Three things hold at once, and all three are guaranteed rather than approached: every unit receives exactly one condition; each unit's probability of each condition is exactly the probability supplied; and each condition's count is the floor or the ceiling of its expected count. With `blocks`, the tight counts are the within-block ones. This closes issue #35, load balancing across blocks, which earlier releases listed as out of scope.

`clusters` assigns whole groups together, in which case the tight counts are counts of clusters rather than of units, and `blocks` and `clusters` may be used at the same time.

The assignment is drawn by the cube method of Deville and Tillé (2004), specialized to this problem. The algorithm holds the counts tight for any number of arms.

Both of `balanced_ra()`'s paths are linear in the number of units and written in C++: 2,000 units take about a tenth of a millisecond with two conditions and about a millisecond with four, measured with a different probability drawn for every unit.

`balanced_ra()` is marked experimental: its interface may change, and it does not yet participate in `declare_ra()`, so `conduct_ra()` and `obtain_condition_probabilities()` do not accept a `balanced_ra` design.

## Reproducibility of randomizr 1.x seeds

Assignments are reproducible across the 1.x boundary. `set.seed(s)` followed by any of the assignment or sampling functions returns what randomizr 1.0.1 returned for that seed, so pre-registrations, replication scripts, and archived analyses that pinned a seed are unaffected by upgrading.

This is not automatic for a rewrite, because the random number stream depends on how many uniforms are drawn and in what order, not only on the sampling design. Two-arm blocked assignment now runs in C++ (see Performance), and the implementation deliberately reproduces the draw sequence of `sample(rep(conditions, c(n - m, m)))` per block, including the separate draw that decides the treated count when `n * prob` is not an integer, in the order 1.x performed them. `tests/testthat/test_stream_compat.R` records the output of randomizr 1.0.1 for a range of designs and fails if any of it moves.

**The one exception is `strata_rs()` and `strata_and_cluster_rs()`**, where the draw for a given seed can differ from 1.x. It follows from the RS/RA unification described below, and the rule is exact: **2.0's `strata_rs()` returns what randomizr 1.x's `block_ra()` returned**. In 1.x the sampling and assignment families were written separately and did not agree with each other, so the draw moves precisely where 1.x's own `strata_rs()` and `block_ra()` disagreed. That is strata of odd size, and any use of `strata_prob`, including strata of even size. Sampling probabilities are unchanged and correct, and the realized count distributions are unchanged; only the particular draw for a given seed differs.

## Performance

Two-arm blocked assignment is the hot path in simulation work, where the same design is redrawn thousands of times, and it now runs in a single C++ call rather than one R-level `complete_ra()` call per block. `randomizr` gains `Rcpp` in `Imports` and `LinkingTo`; `src/block_assign.cpp` is new.

Measured on an Apple M4 Pro under R 4.6.0, `block_ra()` on a two-arm design:

| design | 1.0.1 | 2.0.0 |
|---|---|---|
| N = 20,000, 2,000 blocks | 8.9 ms | 1.1 ms |
| N = 100,000, 10,000 blocks | 46.0 ms | 6.5 ms |

The gain comes from removing an R-level function call per block rather than from drawing fewer random numbers. Drawing the same numbers as 1.x costs roughly a third of the achievable speedup and is what makes seeds reproduce, which is the better trade for a package whose output is cited in pre-registrations.

Assignment to three or more arms, and to two arms whenever the call reaches `complete_ra()` through `prob_each` or `m_each`, runs in `src/block_assign_multi.cpp` on the same principle. It reproduces 1.x's draw as well, which for these branches means reproducing R's own weighted sampling without replacement, `revsort()`'s descending sort included, since that is what decides which arm receives a leftover unit when a block does not divide evenly.

| design, N = 20,000 in 2,000 blocks | 1.0.1 | 2.0.0 |
|---|---|---|
| 3 arms | 21.8 ms | 1.2 ms |
| 4 arms | 22.4 ms | 1.3 ms |
| 3 arms via `prob_each` | 16.9 ms | 1.2 ms |
| 3 arms via `block_m_each` | 16.7 ms | 1.2 ms |
| 2 arms via `prob_each` | 8.7 ms | 1.1 ms |

`block_prob_each` gains less, because most of what it spends goes to validating that every row of the matrix sums to 1 rather than to assigning.

## Internal restructuring

### RS functions unified with RA counterparts

The random sampling (RS) family has always been a two-condition special case of the random assignment (RA) family: sample/not-sampled is equivalent to assignment with `conditions = c(0, 1)`. The RS implementations previously duplicated RA logic in parallel. They now delegate directly:

- `strata_rs()` and `strata_rs_probabilities()` delegate to `block_ra()` and `block_ra_probabilities()` with `conditions = c(0, 1)`. The `strata`/`n`/`strata_n`/`strata_prob` parameters map to `blocks`/`m`/`block_m`/`block_prob` internally. This is the change responsible for the one reproducibility exception noted above: `complete_rs()` and `complete_ra()` selected different assignments for the same seed in 1.x, at odd sizes and wherever a per-stratum probability left a unit over, and `strata_rs()` now takes the `complete_ra()` result.
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

**#35, load balancing across blocks.** Addressed by `balanced_ra()`, described above. Earlier releases listed it as out of scope.

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
