#include <Rcpp.h>
#include <R_ext/Random.h>
#include <R_ext/Utils.h>
#include <cmath>
#include <vector>
using namespace Rcpp;

// Within-block random assignment for three or more arms, and for two arms
// whenever the call reaches complete_ra() through prob_each or m_each.
// Companion to block_assign_cpp(), which covers the two-arm m / prob /
// block_prob cases; between them they cover every branch block_ra_helper()
// can route to.
//
// As with the two-arm kernel, the point is not only to compute the same
// distribution but to consume the random number stream the way randomizr 1.x
// did, so a seed set under 1.x still reproduces. 1.x reached these branches by
// calling complete_ra() once per block from an mapply():
//
//   mode 0  m_each / block_m_each   conditions_vec <- rep(conditions, m_each)
//                                   No draw for the counts; one permutation.
//
//   mode 1  prob_each, block_prob_each, num_arms, or a scalar prob
//                                   m_each_floor <- floor(n * prob_each), and
//                                   the leftover units, N_remainder of them,
//                                   are handed to
//                                     sample(conditions, N_remainder,
//                                            prob = fix_up, replace = FALSE)
//                                   and APPENDED to conditions_vec, so their
//                                   positions are not contiguous with the
//                                   floors. Then one permutation.
//
// The weighted draw is R's own ProbSampleNoReplace, reproduced below rather
// than approximated: it sorts the probabilities descending with revsort(),
// which is not a stable sort, so ties resolve in an order no independent
// implementation would land on by accident.

// R's ProbSampleNoReplace (src/main/random.c), with R's revsort() from
// R_ext/Utils.h doing the descending sort. p is modified in place and must be
// normalised by the caller, as R's FixupProb does before calling this.
// Returns 1-based identities in ans.
static void prob_sample_no_replace(int n, double *p, int *perm,
                                   int nans, int *ans) {
  for (int i = 0; i < n; ++i) perm[i] = i + 1;
  revsort(p, perm, n);

  double totalmass = 1.0;
  int n1 = n - 1;
  for (int i = 0; i < nans; ++i, --n1) {
    double rT = totalmass * unif_rand();
    double mass = 0.0;
    int j = 0;
    for (; j < n1; ++j) {
      mass += p[j];
      if (rT <= mass) break;
    }
    ans[i] = perm[j];
    totalmass -= p[j];
    for (int k = j; k < n1; ++k) {
      p[k] = p[k + 1];
      perm[k] = perm[k + 1];
    }
  }
}

// [[Rcpp::export]]
IntegerVector block_assign_multi_cpp(IntegerVector block_int,
                                     NumericMatrix param,
                                     int mode) {
  int N = block_int.size();
  int G = param.nrow();
  int K = param.ncol();

  // Bounds, before anything writes. check_inputs = FALSE waives the checking
  // of a design; it cannot be allowed to waive memory safety, so the block
  // index, the counts, and the probabilities are all range-checked here even
  // though the validated path never trips them. block_assign_cpp() carries
  // the same guards.
  for (int i = 0; i < N; ++i) {
    int b = block_int[i];
    if (b == NA_INTEGER) {
      stop("`blocks` must not contain NA (unit %d).", i + 1);
    }
    if (b < 1 || b > G) {
      stop("Block index %d at unit %d is outside 1:%d.", b, i + 1, G);
    }
  }
  for (int g = 0; g < G; ++g) {
    for (int j = 0; j < K; ++j) {
      double x = param(g, j);
      if (!R_finite(x) || x < 0.0 || (mode == 0 && x > 2147483646.0)) {
        stop("Block %d, condition %d: %f is not a valid %s.",
             g + 1, j + 1, x, mode == 0 ? "count" : "probability");
      }
    }
  }

  // Units grouped by block, in block order, matching the mapply() the R path
  // would have run.
  std::vector<int> count(G, 0);
  for (int i = 0; i < N; ++i) count[block_int[i] - 1]++;
  std::vector<int> start(G + 1, 0);
  for (int g = 0; g < G; ++g) start[g + 1] = start[g] + count[g];
  std::vector<int> group_units(N);
  std::vector<int> cursor(start.begin(), start.begin() + G);
  for (int i = 0; i < N; ++i) group_units[cursor[block_int[i] - 1]++] = i;

  IntegerVector result(N);
  std::vector<int> v;              // conditions_vec, as 0-based arm indices
  std::vector<int> x;              // the permutation's remaining positions
  std::vector<double> fix(K);
  std::vector<int> permbuf(K), ansbuf(K > 0 ? K : 1);

  GetRNGstate();

  for (int g = 0; g < G; ++g) {
    int n_b = count[g];
    int* grp = &group_units[start[g]];
    v.clear();
    v.reserve(n_b);

    if (mode == 0) {
      // rep(conditions, m_each)
      int m_sum = 0;
      for (int j = 0; j < K; ++j) m_sum += (int) param(g, j);
      if (m_sum != n_b) {
        stop("Block %d has %d units but its counts sum to %d.",
             g + 1, n_b, m_sum);
      }
      for (int j = 0; j < K; ++j) {
        int m_j = (int) param(g, j);
        for (int t = 0; t < m_j; ++t) v.push_back(j);
      }

    } else {
      // rep(conditions, floor(n * prob_each)), then the remainder drawn.
      //
      // np is volatile so the product is forced to round to a double before
      // the floor and the subtraction. Written inline, the compiler contracts
      // the multiply and the subtract into a single FMA, so the product never
      // rounds to a double and 15 * 0.2 - 3 comes out as 1.67e-16 where R,
      // rounding at every step, gets exactly 0. That epsilon is invisible in
      // the probabilities and decisive in the draw: it breaks a tie in
      // revsort()'s descending sort, which hands the remainder unit to the
      // wrong arm and silently ends 1.x seed reproducibility. A plain local
      // is not enough: -ffp-contract=fast contracts straight through it.
      int assigned = 0;
      for (int j = 0; j < K; ++j) {
        volatile double np_v = n_b * param(g, j);
        double np = np_v;
        int f_j = (int) std::floor(np);
        assigned += f_j;
        for (int t = 0; t < f_j; ++t) v.push_back(j);
      }
      int rem = n_b - assigned;
      if (rem < 0 || rem > K) {
        // The floors can only leave 0 to K units over when the probabilities
        // sum to 1. Anything else would hand prob_sample_no_replace() more
        // draws than ansbuf holds, which is a buffer overflow, not a design
        // choice check_inputs = FALSE can waive.
        stop("Block %d's probabilities leave %d of %d units unassigned; "
             "they must sum to 1.", g + 1, rem, n_b);
      }
      if (rem > 0) {
        double s = 0.0;
        for (int j = 0; j < K; ++j) {
          volatile double np_v = n_b * param(g, j);
          double np = np_v;
          fix[j] = (np - std::floor(np)) / rem;
          s += fix[j];
        }
        for (int j = 0; j < K; ++j) fix[j] /= s;   // R's FixupProb
        prob_sample_no_replace(K, fix.data(), permbuf.data(), rem, ansbuf.data());
        for (int t = 0; t < rem; ++t) v.push_back(ansbuf[t] - 1);
      }
    }

    // assignment <- sample(conditions_vec, length(conditions_vec))
    int n_v = (int) v.size();
    x.resize(n_v);
    for (int i = 0; i < n_v; ++i) x[i] = i;
    int n_rem = n_v;
    for (int i = 0; i < n_v; ++i) {
      int j = (int) R_unif_index((double) n_rem);
      int idx = x[j];
      x[j] = x[--n_rem];
      // m_each summing to something other than the block size is rejected
      // upstream; the guard keeps a bad call from running off the block.
      if (i < n_b) result[grp[i]] = v[idx];
    }
  }

  PutRNGstate();
  return result;
}
