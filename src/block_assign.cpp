#include <Rcpp.h>
#include <R_ext/Random.h>
#include <cmath>
using namespace Rcpp;

// Within-block complete random assignment, reproducing randomizr 1.x's random
// number stream exactly.
//
// 1.x called complete_ra() once per block from an mapply(), so both the RNG
// pattern of each call and their interleaving with the permutations have to be
// reproduced. Which pattern applies is decided by block_ra_helper(), and it is
// not what the argument names suggest: only `block_prob` reaches complete_ra()
// as `prob`. A scalar `prob`, and the default, are converted to `prob_each` and
// take complete_ra()'s multi-arm branch instead.
//
//   mode 0  m / block_m       v = c(rep(0, n - m), rep(1, m)); no draw for m.
//   mode 1  default or prob   prob_each branch. m_each_floor = floor(n * p_each);
//                             if a remainder unit is left over, one draw of
//                             sample(conditions, 1, prob = fix_up) decides which
//                             arm receives it, and it is APPENDED to
//                             conditions_vec, so the treated positions are not
//                             contiguous.
//   mode 2  block_prob        Case 3. One draw of
//                             sample(c(m_ceiling, m_floor), 1, prob = ...),
//                             then v = c(rep(0, n - m), rep(1, m)).
//
// The permutation is R's own sample.int(n) loop: n draws of R_unif_index.

// One draw of sample(x, 1, prob = c(pa, pb), replace = FALSE) over two elements,
// i.e. R's ProbSampleNoReplace with n = 2, k = 1: probabilities are sorted
// descending, then a single uniform is walked against the cumulative. Returns
// true when the first element (the one carrying pa) is selected.
static inline bool draw_two(double pa, double pb) {
  double rT = unif_rand();
  // Strict: R's revsort swaps equal elements, so on a tie the SECOND element is
  // the one selected by rT <= p. Verified against sample(c(0,1), 1,
  // prob = c(0.5, 0.5)) over 2,000 seeds: 2000/2000 agreement.
  if (pa > pb) return rT <= pa;
  return !(rT <= pb);
}

// [[Rcpp::export]]
IntegerVector block_assign_cpp(IntegerVector block_int,
                               IntegerVector m_given,
                               NumericVector prob_b,
                               int mode) {
  int N = block_int.size();
  int G = m_given.size();

  if (mode != 0 && prob_b.size() < G) {
    stop("%d block probabilities were supplied for %d blocks.",
         (int) prob_b.size(), G);
  }

  // Bounds, before anything writes. Each of the three modes below fills v by
  // counting down from n_b, so a block count or probability outside its range
  // makes the loop start at a negative index and write off the front of the
  // buffer. check_inputs = FALSE waives the checking of a design; it cannot be
  // allowed to waive memory safety, and reaching here with a bad m_b used to
  // corrupt the heap and take R down with a bus error.
  for (int i = 0; i < N; ++i) {
    int b = block_int[i];
    if (b == NA_INTEGER) {
      stop("`blocks` must not contain NA (unit %d).", i + 1);
    }
    if (b < 1 || b > G) {
      stop("Block index %d at unit %d is outside 1:%d.", b, i + 1, G);
    }
  }

  std::vector<int> count(G, 0);
  for (int i = 0; i < N; ++i) count[block_int[i] - 1]++;

  for (int g = 0; g < G; ++g) {
    if (mode == 0) {
      int m_b = m_given[g];
      if (m_b < 0 || m_b > count[g]) {
        stop("Block %d has %d units but %d were requested for treatment.",
             g + 1, count[g], m_b);
      }
    } else {
      double p = prob_b[g];
      if (!R_finite(p) || p < 0.0 || p > 1.0) {
        stop("Block %d has assignment probability %f, which is not in [0, 1].",
             g + 1, p);
      }
    }
  }
  std::vector<int> start(G + 1, 0);
  for (int g = 0; g < G; ++g) start[g + 1] = start[g] + count[g];
  std::vector<int> group_units(N);
  std::vector<int> cursor(start.begin(), start.begin() + G);
  for (int i = 0; i < N; ++i) group_units[cursor[block_int[i] - 1]++] = i;

  IntegerVector result(N, 0);
  std::vector<int> x;
  std::vector<unsigned char> v;   // conditions_vec, 1 = treated
  GetRNGstate();

  for (int g = 0; g < G; ++g) {
    int n_b = count[g];
    int* grp = &group_units[start[g]];
    v.assign(n_b, 0);

    if (mode == 0) {
      int m_b = m_given[g];
      // 1.x's complete_ra() returns rep(1, N) before drawing anything when
      // m == N, so a fully treated block must consume no RNG at all here or
      // seeds set under 1.x stop reproducing. m == 0 has no such early
      // return in 1.x: it still permutes, so it falls through below.
      if (m_b == n_b) {
        for (int i = 0; i < n_b; ++i) result[grp[i]] = 1;
        continue;
      }
      for (int i = n_b - m_b; i < n_b; ++i) v[i] = 1;

    } else if (mode == 1) {
      double p = prob_b[g];
      // np0 and np1 are volatile so the product is forced to round to a
      // double before the subtraction below. Written inline, the compiler
      // contracts the multiply and the subtract into a single FMA, the
      // product never rounds, and e.g. 5 * 0.9 - 4 comes out as 4.4e-16
      // where R, which rounds at every step, gets exactly 0. The difference
      // is far below any probability anyone can observe and it still decides
      // the draw, because draw_two() resolves an exact tie the opposite way
      // from a near-tie. That silently ended 1.x seed reproducibility for
      // four of the designs tested. A plain local is not enough: under
      // -ffp-contract=fast the compiler contracts straight through it, so
      // volatile is what pins the rounding on every compiler.
      volatile double np0_v = n_b * (1.0 - p);
      volatile double np1_v = n_b * p;
      double np0 = np0_v;
      double np1 = np1_v;
      int mf0 = (int) std::floor(np0);
      int mf1 = (int) std::floor(np1);
      int rem = n_b - mf0 - mf1;
      for (int i = mf0; i < mf0 + mf1; ++i) v[i] = 1;
      if (rem > 0) {
        double fix0 = (np0 - mf0) / rem;
        double fix1 = (np1 - mf1) / rem;
        // conditions = c(0, 1); the drawn arm's unit is appended at the end.
        if (!draw_two(fix0, fix1)) v[mf0 + mf1] = 1;
      }

    } else {
      volatile double Np_v = n_b * prob_b[g];  // volatile: see np0_v above
      double Np = Np_v;
      int mf = (int) std::floor(Np);
      int mc = (int) std::ceil(Np);
      int m_b;
      if (mc == n_b) {
        m_b = mf;                       // 1.x returns before drawing here
      } else {
        double pfu = (mc > mf) ? (Np - mf) / (double)(mc - mf) : 0.5;
        m_b = draw_two(pfu, 1.0 - pfu) ? mc : mf;
      }
      for (int i = n_b - m_b; i < n_b; ++i) v[i] = 1;
    }

    // assignment <- sample(conditions_vec, length(conditions_vec))
    x.resize(n_b);
    for (int i = 0; i < n_b; ++i) x[i] = i;
    int n_rem = n_b;
    for (int i = 0; i < n_b; ++i) {
      int j = (int) R_unif_index((double) n_rem);
      int idx = x[j];
      x[j] = x[--n_rem];
      if (v[idx]) result[grp[i]] = 1;
    }
  }

  PutRNGstate();
  return result;
}
