# randomizr 2.0.1

This release is a major internal restructuring plus one new capability, described here relative to randomizr 1.0.1, the version it replaces on CRAN. No exported function, argument, return type, S3 class, or object field carried by 1.0.1 is removed, renamed, or reordered. New arguments were added to `declare_ra()`, `conduct_ra()` and `obtain_condition_probabilities()` (`prob_unit_each` after `prob_each`; `ra_type` and `formula` before `permutation_matrix`; `data` last), so a caller passing `block_m` or later arguments by position must name them; `block_ra()` gains two arguments documented as internal (`.block_int`, `.N_per_block`). The package exports 31 objects against 1.0.1's 29, the two additions being `balanced_ra()` and `balanced_ra_probabilities()`.

## New: assignment with tight targets (experimental)

`balanced_ra()` and `balanced_ra_probabilities()` draw assignment with tight targets: condition counts at the floor or ceiling of what the probabilities imply, while each unit's probability stays exact. That is useful when probabilities vary across units, and also when they do not. With unit-varying probabilities they fill the gap between `simple_ra()`, which honors those probabilities but lets the number treated wander, and `complete_ra()`, which fixes the number treated but requires every unit to share the same probability.

Three things hold at once. Two need no qualification: every unit receives exactly one condition, and each unit's probability of each condition is exactly the probability supplied. The third, that each condition's count is the floor or the ceiling of its expected count, holds with one arithmetic exception. Each step is sized so that at least one unit lands exactly on 0 or on 1, and very occasionally floating-point rounding leaves every unit in a step a hair short of its bound; the function then settles one unit by a coin weighted by the value it holds. That coin keeps the unit's probability exact, so the probability guarantee is untouched, but it does not respect the count, and a draw that reaches it can finish one unit off. It did not arise in several thousand draws across dozens of randomly generated designs, and is recorded here because it is reachable in principle rather than because it is expected in practice. With `blocks`, counts are tight within each block, and two-arm designs are tight overall as well, because leftovers are paired across blocks; with three or more arms the overall count can wander. `clusters` assigns whole groups together, in which case the tight counts are counts of clusters, and `blocks` and `clusters` may be used at the same time.

The assignment is drawn by the cube method of Deville and Tillé (2004), specialized to this problem, with three C++ kernels: a two-arm pivotal pass, a multi-arm flight-and-land, and cube-on-X. All three are linear in the number of units. Two thousand units take about a tenth of a millisecond with two conditions and about a millisecond with four, measured with a different probability drawn for every unit.

`balanced_ra(formula = ~ x + B)` adds linear balancing constraints on covariates (cube-on-X): the flight keeps the treated totals of the model matrix near their expectations, with the intercept as the count constraint. Drop the intercept, as in `~ 0 + x`, and the count is no longer constrained and may wander. Two-arm only; `blocks` and `prob_unit_each` cannot be combined with it.

The probability arguments follow the package's convention: `prob` is the one-number slot, `prob_unit` the one-per-unit slot (a single number is refused, except at `N = 1`), and `prob_unit_each` the one-row-per-unit matrix; supply exactly one. `num_arms` or `conditions` without probabilities expand to equal-probability balanced assignment, and condition naming follows `complete_ra()`: two arms are 0 and 1 unless `num_arms` is supplied explicitly. The default is `prob = 0.5`, so `balanced_ra(4)` is complete assignment of four units.

`declare_ra()` accepts balanced assignment: set `ra_type = "balanced"`, or supply `prob_unit_each` or `formula`. `conduct_ra()` and `obtain_condition_probabilities()` then dispatch to `balanced_ra()` and `balanced_ra_probabilities()`. Existing calls such as `declare_ra(N, prob = 0.5)` remain complete assignment. A declaration made with `formula` resolves the balancing matrix once, when the design is declared, and carries it, so a design declared inside a function cannot have its covariate shadowed by a same-named object elsewhere on the call stack; `balanced_ra()` called directly searches `environment(formula)` first, as `stats::lm()` does. `obtain_num_permutations()` reports `Inf` for balanced designs (the cube support is not enumerated) and `obtain_permutation_probabilities()` errors, since the support is not listed and the assignments are not equally likely. `balanced_ra()` is marked experimental: its interface may change.

This capability closes issue #35, load balancing across blocks, which earlier releases listed as out of scope. It also bears on issue #99, which asked for fixed-size unequal-probability sampling: `balanced_ra()` is that sampler, with exact first-order inclusion probabilities. The issue's premise about `simple_ra()` does not apply to the current implementation, whose `vsample()` C routine performs correct inverse-CDF multinomial sampling, confirmed by simulation.

## New: `declare_ra()` gains `data`

A declaration is a design rather than an assignment, and it is used in a different frame than the one it was written in, so it needs a way to say which table its variables come from. When `data` is supplied, every argument that carries one value per unit names columns of it and is looked up there and nowhere else: `blocks`, `clusters`, `m_unit`, `prob_unit`, `prob_unit_each`, and the variables in `formula`. Anything they name that is not a column is an error rather than a fall-through to the calling environment, and `N` defaults to `nrow(data)`. A bare column name, a string naming a column, or any expression whose variables are all columns are all accepted, so `blocks = interaction(region, year)` and `prob_unit_each = cbind(p_a, p_b)` work and `blocks = df$bl` does not. Arguments forwarded through a wrapper's `...` resolve the same way. `permutation_matrix` is left out on purpose: it has one row per unit but enumerates assignments rather than describing units. `data` is validated and discarded rather than stored, so declarations do not grow. Omitting `data` keeps the old behavior, which is to resolve everything in the calling environment.

Alongside it, every per-unit slot of `declare_ra()` refuses a single number and names the argument that takes one, so `declare_ra(prob_unit = 0.5)` says to use `prob` and `m_unit = 2` says to use `m` (`N = 1` is exempt, since there one value is one value per unit). `?declare_ra` no longer says that `prob` may vary by unit under simple random assignment: it may not, and never could, since `prob` must be of length 1 in every design.

## Reproducibility of randomizr 1.x seeds

Assignments are reproducible across the 1.x boundary. `set.seed(s)` followed by any of the assignment or sampling functions returns what randomizr 1.0.1 returned for that seed, so pre-registrations, replication scripts, and archived analyses that pinned a seed are unaffected by upgrading.

Reproducibility is not automatic for a rewrite, because the random number stream depends on how many uniforms are drawn and in what order, not only on the sampling design. Blocked assignment now runs in C++ (see Performance), and the implementation deliberately reproduces 1.x's draw sequence: the full within-block permutation of `sample(rep(conditions, c(n - m, m)))`, the separate draw that decides a treated count when `n * prob` is not an integer, R's own weighted sampling without replacement for multi-arm leftovers (`revsort()`'s descending sort included), and the early return that consumes no random numbers at all when a block is fully treated. The products `n * prob` are computed through `volatile` intermediates so that no compiler, under any floating-point contraction setting, fuses the multiply and the subtraction into an FMA; the fused form differs from R's in the sixteenth decimal place and that is enough to flip a tie and move a draw. `tests/testthat/test_stream_compat.R` pins output recorded from randomizr 1.0.1 across the complete, simple, blocked (every argument form), clustered, blocked-and-clustered, and sampling families, along with the stream position after a sequence of draws, and fails if any of it moves.

**The one exception is `strata_rs()` and `strata_and_cluster_rs()`**, where the draw for a given seed can differ from 1.x. It follows from the RS/RA unification described below, and the rule is exact: **2.0.1's `strata_rs()` returns what randomizr 1.x's `block_ra()` returned**, value for value. In 1.x the sampling and assignment families were written separately and did not agree with each other, so the draw moves precisely where 1.x's own `strata_rs()` and `block_ra()` disagreed: strata of odd size under the default probability, and any design where a per-stratum probability leaves a unit over, `strata_prob` and fractional `n_s * prob` included. Sampling probabilities are unchanged and correct, and the realized count distributions are unchanged; only the particular draw for a given seed differs. A test asserts the rule as an equality.

## Performance

Blocked assignment is the hot path in simulation work, where the same design is redrawn thousands of times, and it now runs in a single C++ call rather than one R-level `complete_ra()` call per block. `randomizr` gains `Rcpp` in `Imports` and `LinkingTo`.

Measured on an Apple M4 Pro under R 4.6.0, `block_ra()`:

| design | 1.0.1 | 2.0.1 |
|---|---|---|
| N = 20,000, 2,000 blocks | 8.9 ms | 1.1 ms |
| N = 100,000, 10,000 blocks | 46.0 ms | 6.5 ms |
| 3 arms, N = 20,000, 2,000 blocks | 21.8 ms | 1.2 ms |
| 4 arms | 22.4 ms | 1.3 ms |
| 3 arms via `prob_each` | 16.9 ms | 1.2 ms |
| 3 arms via `block_m_each` | 16.7 ms | 1.2 ms |
| 2 arms via `prob_each` | 8.7 ms | 1.1 ms |

The gain comes from removing an R-level function call per block rather than from drawing fewer random numbers. Drawing the same numbers as 1.x costs roughly a third of the achievable speedup and is what makes seeds reproduce, which is the better trade for a package whose output is cited in pre-registrations. `block_prob_each` gains less, because most of what it spends goes to validating that every row of the matrix sums to 1 rather than to assigning.

## `check_inputs = FALSE` works, and cannot corrupt memory

`check_inputs = FALSE` now works for blocked and clustered assignment. It failed on every path whenever `num_arms` or `conditions` was left implicit, because the validation also derived them, so skipping the validation skipped the derivation; the derivation is separate now and runs on both paths. Draws are unaffected: across the twenty designs in the parity test, `check_inputs = FALSE` returns byte-identical assignments to `check_inputs = TRUE` on the same seed. A blocked declaration made with `check_inputs = FALSE` can also be conducted, which never worked.

Skipping the checks waives the checking of a design; it cannot be allowed to waive memory safety. Every C++ kernel now range-checks its indexes, counts, and probabilities before anything writes: a block count or probability outside its range, counts that do not sum to the block size, probabilities that leave more leftovers than conditions, or a too-short `block_prob` are errors rather than reads and writes outside the buffer.

## Robustness

An `NA` in `blocks`, `clusters`, or `strata` is an error on every path, with `check_inputs = FALSE` included. 1.0.1 silently dropped the NA units and returned an assignment shorter than the number of units.

A `blocks` factor with unused levels has its unused levels dropped everywhere, so a subset of a factor behaves like the same subset of a character vector. 1.0.1 errored on unused levels.

`randomizr::conduct_ra(N = 10)` and `randomizr::obtain_condition_probabilities(assignment =, N = 10)` work without attaching the package, and a user object named `declare_ra` no longer hijacks them; the forwarded call now names `randomizr::declare_ra` explicitly.

`cluster_rs()` and `cluster_rs_probabilities()` accept `prob_unit` and `n_unit` end to end. The collapse to one value per cluster produced an array or a list that the complete-sampling internals could not digest, so the probabilities, declarations, and even `print()` on such a declaration all failed.

An `NA` in a `formula` covariate is an error; previously `model.matrix()` dropped the row and the assignment silently came back shorter than the number of units. `balanced_ra(formula = ~ 1)` draws complete assignment; a sorting degeneracy had made it a deterministic pairing of adjacent units, with correct marginal probabilities but a joint distribution nobody asked for. `balanced_ra()` refuses a non-numeric `N`.

Error message typo fixed: "The probabilties of assignment..." now reads "The probabilities of assignment...", and the strata `prob_unit` message says sampling rather than assignment.

## Internal restructuring

### RS functions unified with RA counterparts

The random sampling (RS) family has always been a two-condition special case of the random assignment (RA) family: sampled/not-sampled is equivalent to assignment with `conditions = c(0, 1)`. The RS implementations previously duplicated RA logic in parallel. They now delegate directly: `strata_rs()` and `strata_rs_probabilities()` delegate to `block_ra()` and `block_ra_probabilities()`, with `strata`/`n`/`strata_n`/`strata_prob` mapping to `blocks`/`m`/`block_m`/`block_prob` internally. The unification is the source of the one reproducibility exception noted above, and it also repaired a 1.x defect: `strata_rs()` on a stratum of three with `prob = 0.9` sampled each unit with probability 2/3 while its own probabilities function reported 0.9; the two now agree. `simple_rs()`, `cluster_rs()` and `strata_and_cluster_rs()` already delegated internally.

### S3 dispatch boilerplate replaced

`generated_methods.R` previously contained 264 lines of hand-maintained S3 dispatch: each `ra_function.*`, `ra_probabilities.*`, `rs_function.*`, and `rs_probabilities.*` method explicitly unpacked every slot of the declaration object and re-passed them by name to the underlying function, and any parameter addition required manual updates in two places. The 24 methods now extract only the formals the target function accepts, via `mget()` over the declaration environment, so the `delayedAssign()` slots on declaration objects (`probabilities_matrix` and the deprecated fields) are not triggered as a side effect of dispatch.

## Dependencies and documentation

`Depends: R (>= 3.6.0)`, raised from 3.5.0 because the C++ kernels draw through `R_unif_index()`, which R provides from 3.6.0.

The package ships three vignettes: the getting-started introduction, "What randomizr guarantees", and "Introduction to balanced_ra".

## Compatibility

ri2 (>= 0.4.1) depends on randomizr for `declare_ra()`, `conduct_ra()`, `obtain_condition_probabilities()`, `obtain_permutation_matrix()`, `obtain_num_permutations()`, the five `ra_*` S3 class names, and the `probabilities_matrix`, `blocks`, and `clusters` fields on declaration objects. All are unchanged, and the ri2 and DeclareDesign test suites pass without modification against this release.

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
