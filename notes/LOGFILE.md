# randomizr development log

Private working log. Gitignored and Rbuildignored. Mirror of `~/Dropbox/claude_control/logs/randomizr.md`, which is the Dropbox-backed source of truth. Entries before 2026-08-04 were recovered from a stray `LOGFILE.md` at the repo root, which was gitignored but not Rbuildignored and so had been shipping inside the tarball.


## 2026-05-10 — Documentation refresh (session with Claude Sonnet 4.6)

### Context

This session's randomizr work followed a major 2.0.0 refactor completed in an earlier session (see git log). The refactor added a Rcpp Knuth-shuffle fast path for two-arm block assignment, declaration caching, and mget-based S3 dispatch. This session focused on documentation only.

### What changed

**Roxygen docs refreshed in five files:**

- `R/block_ra.R` — description now explains that blocking reduces sampling variability by guaranteeing treated and control units in every covariate-defined subgroup; precision gain scales with the correlation between blocking variable and potential outcomes. Hidden parameters `.block_int` and `.N_per_block` are now explicitly documented as "Internal use only. Users should never set this argument."
- `R/cluster_ra.R` — description now explains why clustering is used (group-level interventions, spillovers, theoretical reasons) and the precision cost (effective sample size = number of clusters, loss grows with intra-cluster correlation).
- `R/complete_ra.R` — description clarifies the fixed-count guarantee that distinguishes complete from simple RA, and explains stochastic rounding for fractional implied counts.
- `R/simple_ra.R` — description explains when simple RA is appropriate (sequential/unknown N) and why complete RA is preferred otherwise.
- `R/declare_ra.R` — description explains the declare-then-conduct workflow, probability caching, and the return value with `\describe{\item{}}` markup.

**New vignette:** `vignettes/randomizr_algorithms.Rmd` — explains the algorithms behind each randomization type: vectorized Bernoulli for simple RA, partial Knuth shuffle for complete RA, O(N) counting sort + per-block C++ shuffle for block RA (with declaration caching note), cluster reduction for cluster RA, and probability calculation.

### Inspiration

Documentation framing drawn from FEDAI chapters 3 and 4 (blocking for precision, clustering for practical/theoretical reasons). Key quotes used for framing:
- Blocking: "rules out the unlucky assignments that would otherwise pull estimates far from the true average treatment effect"
- Clustering: "employed for practical reasons (individual-level assignments are not feasible) or for theoretical reasons (the intervention of interest is defined as a cluster-level intervention)"

### Resume this session

Session: /Users/alexandercoppock/.claude/projects/-Users-alexandercoppock-git-projects-metaprep/0e4e8439-5cbc-4e27-9ad8-17c3a95c0efd.jsonl


Private working log, Dropbox-backed. The repo is `~/git_projects/randomizr`, public at github.com/DeclareDesign/randomizr. Created 2026-08-04. The 2026-05-10 entry above predates it and was recovered from the repo root.

## 2026-08-04: the 2.0 refactor does not check, and the cause is two lines

Audited as part of bringing randomizr into the four-package CRAN plan (`claude_control/notes/zero_rename_plan.md`). It had been assumed to be at the same readiness as the three Zero packages because its branch is public and its DESCRIPTION says `2.0.0`. It is not.

**State on arrival.** `randomizr-2.0-refactor` at `b797efa` (2026-05-10), pushed to origin, `Version: 2.0.0`, 21 test files with 205 `test_that` blocks, README and NEWS present. `main` is at 1.0.0 (Feb 2025), `cran-patch` at 1.0.1, and CRAN has 1.0.1. Neither merged nor shipped.

**`R CMD check --as-cran` is 3 ERRORs, 2 WARNINGs, 4 NOTEs.** Examples fail, tests fail, vignettes fail to rebuild. Every one of the failures is the same error:

```
Error in block_assign_cpp(block_int, m_per_b) :
  function 'enterRNGScope' not provided by package 'Rcpp'
```

**Cause, diagnosed rather than guessed.** The refactor introduced randomizr's first C++ code, `src/block_assign.cpp` (68 lines, new), and registered it in `src/onload.c`. `NAMESPACE` was not touched: it still reads `useDynLib(randomizr)` with no `importFrom(Rcpp, ...)`, exactly as it did when the only compiled code was plain C (`restrictedparts.c`, `vsample`). Plain C never needed Rcpp's namespace loaded, and `Imports: Rcpp` in DESCRIPTION does not load it. With the namespace never loaded, Rcpp's C entry points are never registered, so the first Rcpp call cannot resolve `enterRNGScope`. The `libc++abi: __cxa_guard_acquire detected recursive initialization` abort in the test run is the same fault in its uglier form.

**Fix, verified in a throwaway copy so the repo was left untouched.** Replacing

```
useDynLib(randomizr)
```

with

```
importFrom(Rcpp,evalCpp)
useDynLib(randomizr, .registration = TRUE)
```

installs clean, and the example that failed in the check now runs: `block_and_cluster_ra()` over the 26-cluster/5-block fixture returns 171/180, `block_ra()` over 20 units returns 10/10. In the repo this belongs in a roxygen block (`@importFrom Rcpp evalCpp`, `@useDynLib randomizr, .registration = TRUE`) rather than as a hand edit to NAMESPACE, since NAMESPACE is generated.

**This is a defect the refactor introduced, not an environment problem.** Released randomizr 1.0.1 works on the same machine under the same Rcpp 1.1.1.1.1, checked directly. The branch has presumably been broken since May, which means it was pushed without a check ever being run against it. The lesson is the one the DeclareDesignZero audit already recorded in July: a commit that says "checks" is not evidence that anything was checked.

**The good news, and it is substantial.** The refactor's exported API is **byte-identical** to the released package's: 29 exports on each side, nothing added, nothing removed. For a major-version rewrite that is the strongest possible position, because a rewrite that changes no signature has near-zero reverse-dependency exposure. randomizr has 10 strong revdeps and 3 that Suggest it. Once the NAMESPACE fix lands and the check is clean, randomizr is arguably the safest of the four to ship.

**Not done here, deliberately:** the fix was not applied to the repo and nothing was committed or pushed. It is item one of Stage 1 completion in the plan.

### Resume this session
Session: /Users/alexandercoppock/.claude/projects/-Users-alexandercoppock-Library-CloudStorage-Dropbox-claude-control/d9a22e33-47dd-4682-b584-a5df0e8aabe0.jsonl

## 2026-08-04 (late): ignore files standardised, randomizr fixed and on `rewrite`, vignettes renamed, memo written

**All four packages are now on `rewrite` branches and all four check.** `fabricatr@rewrite` `fc882fe`, `estimatr@rewrite` `a4a962b`, `DeclareDesign@rewrite` `30a43ce`, `randomizr@rewrite` `cb5dcfc`. Zero CI runs fired on any of them at any point today.

**randomizr's 3 ERRORs / 2 WARNINGs / 4 NOTEs went to `Status: OK` on one roxygen line.** The minimal fix turned out to be `@importFrom Rcpp evalCpp` alone: `.registration = TRUE` is unnecessary because `src/onload.c` already calls `R_registerRoutines` by hand, and the `.Call` sites use string names with `PACKAGE =`, which resolve through the registration table. Tested the smaller change on its own before applying it. Two further defects fell out of the same check: `stats::runif` was used but never imported (needed `stats` in Imports too), and **a `LOGFILE.md` sat at the repo root, gitignored but NOT Rbuildignored, so it had been shipping inside the tarball.** Its content is a 2026-05-10 documentation session, which means **the claim in this morning's randomizr entry that the refactor had no contemporaneous log was wrong.** That entry is corrected and the May content is now the first entry of `notes/LOGFILE.md`, mirrored to `claude_control/logs/randomizr.md`.

**Ignore files brought to one standard across all four, and it was not cosmetic.** estimatrZero's `.Rbuildignore` had two entries, so `.github`, `.claude`, `.Rcheck` directories and stray tarballs would all have shipped inside the package; its `.gitignore` was missing `.DS_Store`, `.claude/` and tarballs. fabricatrZero had `.claude` Rbuildignored but not gitignored, so session state could have been committed. randomizr had no `^notes$` in `.Rbuildignore` at all. **Verified rather than assumed: built all four and counted private paths in the tarballs, 0 in every case.** `notes/` is now ignored both ways everywhere, which is what lets the logfile and the estimatr issue ledger live in the repos while staying private.

**Vignettes renamed to the release they describe** (Alex): `fabricatrZero.Rmd` -> `fabricatr2.0.Rmd`, `estimatrZero.Rmd` -> `estimatr2.0.Rmd`, `declaredesignzero.Rmd` -> `declaredesign2.0.Rmd`, with titles and `\VignetteIndexEntry` following and every `vignette()` reference in READMEs, NEWS and package docs updated. "Zero" named a parallel package; these ship as major versions of the packages they rewrite. Verified by building and installing all three: `vignette()` resolves the new names.

**Memo to Macartan and Graeme at `~/Desktop/rewrites_memo_20260804.md`.** Part A is the four install lines. Part B answers the NSE note with Macartan's own `declaration_17.6` run against the current code, output pasted in, plus the quoted-name workaround still working, the masking-handler cases, and a plain caller value. It leads with the convergence (two independent routes to one design), reports that **no convention flag was needed**, explains why `fabricate` keeps its special case, states the evaluation rule, and closes on the three open items: whether Live Designs is DDWizard or a second app, the unmerged diagnosands proposal, and `18.13`'s non-CRAN `interference` dependency.

### Resume this session
Session: /Users/alexandercoppock/.claude/projects/-Users-alexandercoppock-Library-CloudStorage-Dropbox-claude-control/d9a22e33-47dd-4682-b584-a5df0e8aabe0.jsonl

## 2026-08-04 (late, memo revision): the randomizr defect comes out of the memo

Alex's call: the Rcpp fault is fixed, so it does not go to Graeme. The paragraph explaining the `enterRNGScope` failure, the missing `stats::runif` import and the tarball-shipping `LOGFILE.md` is cut from `~/Desktop/rewrites_memo_20260804.md`, replaced by one line saying all four check clean apart from the expected fabricatrZero strong-dependency note. Memo is 1,247 words.

**The reasoning is worth keeping even though the paragraph is not.** A defect that is fixed, pushed and verified is a maintenance record, not news for a maintainer. It belongs in the logfile, which has it in full at the 2026-08-04 entries here and in `claude_control/logs/randomizr.md`. Sending it would have asked Graeme to hold a fact he can do nothing with.

**One caveat that stays true and is not in the memo:** the fix is verified on macOS only, because CI has deliberately not been run on any `rewrite` branch. That is a Stage 3 item already on the checklist (copy the parent's three workflows onto the branch and get CI green), and it applies to all four packages equally, not to randomizr in particular.

### Resume this session
Session: /Users/alexandercoppock/.claude/projects/-Users-alexandercoppock-Library-CloudStorage-Dropbox-claude-control/d9a22e33-47dd-4682-b584-a5df0e8aabe0.jsonl

## 2026-08-04 (end): all four branches at 2.0.0

`fabricatr@rewrite` `474b3fd`, `estimatr@rewrite` `65f5c26`, `DeclareDesign@rewrite` `af49ed0`, `randomizr@rewrite` `cb5dcfc`. All four check clean; no CI fired.

**2.0.0 is the correct major for all four**, since CRAN carries fabricatr 1.0.2, estimatr 1.0.6, DeclareDesign 1.1.1 and randomizr 1.0.1. randomizr was already there from May.

**The version bump and the rename are deliberately separate acts, and only one of them is reversible.** The version is metadata: `fabricatrZero 2.0.0` states intent and costs nothing. The rename is the one-way door, because two packages of one name cannot be loaded together, which would end the comparison against the released package that the suites and vignettes are built on. estimatrZero is the sharpest case, with 40 cross-references to estimatr across 6 test files including the 1e-12 identity check. So the packages are versioned 2.0.0 and still named `*Zero`, and the rename stays the last step before release.

**The `>= 2.0.0` constraint earned itself immediately.** DeclareDesignZero now declares `Imports: fabricatrZero (>= 2.0.0)`, and the first check after the bump came back **1 ERROR: "Package required and available but unsuitable version: fabricatrZero"**, because the user library still held 0.1.0. That is the constraint working, not a defect: it caught a stale dependency the moment one existed. Installing fabricatrZero 2.0.0 cleared it, and DeclareDesignZero is back to its expected single WARNING (fabricatrZero is not on CRAN, so it is an unresolvable strong dependency until fabricatr 2.0.0 ships, which is the same thing that forces the submission order).

**Three stale version strings found by grep and fixed**, all user-facing: the fabricatr vignette's `install.packages("fabricatrZero_0.1.0.tar.gz")` line, and the benchmark provenance sentences in the DeclareDesign and estimatr vignettes, which name the build the timings were measured on. Same code, renumbered, so those readings still stand. The three READMEs each carried a sentence asserting the old version and saying nothing on the branch claimed 2.0.0; all three now say the opposite and explain why the name has not moved with the number.

**Final check status:** fabricatrZero 1 NOTE, estimatrZero 1 NOTE, DeclareDesignZero 1 WARNING, randomizr OK. Every one of those is the expected fabricatrZero-not-on-CRAN condition or new-submission boilerplate.

### Resume this session
Session: /Users/alexandercoppock/.claude/projects/-Users-alexandercoppock-Library-CloudStorage-Dropbox-claude-control/d9a22e33-47dd-4682-b584-a5df0e8aabe0.jsonl
