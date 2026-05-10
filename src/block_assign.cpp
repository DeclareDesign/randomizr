#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// Within-block complete random assignment via Knuth partial shuffle.
//
// block_int:    1-indexed group membership for each unit (length N, values 1..G).
// m_per_block:  number of units to treat in each group (length G).
//
// Returns 0/1 integer vector (1 = treated). The caller maps to condition labels.
//
// Algorithm:
//   1. Counting sort to place unit indices into contiguous group slots — O(N).
//   2. Within each group of size n_b, do a partial Knuth shuffle of m_b steps,
//      drawing exactly m_b uniforms. Total RNG draws = sum(m_b) <= N/2 on average.
//
// This beats a comparison sort (O(N log N)) and avoids generating N upfront
// uniforms; total work is O(N) with very small constants.

// [[Rcpp::export]]
IntegerVector block_assign_cpp(IntegerVector block_int,
                                IntegerVector m_per_block) {
  int N = block_int.size();
  int G = m_per_block.size();

  // --- Step 1: counting sort ---

  // Count units per group
  std::vector<int> count(G, 0);
  for (int i = 0; i < N; ++i) count[block_int[i] - 1]++;

  // Prefix sums give the start index of each group in the flat array
  std::vector<int> start(G + 1, 0);
  for (int g = 0; g < G; ++g) start[g + 1] = start[g] + count[g];

  // Fill group_units: unit indices sorted into contiguous group runs
  std::vector<int> group_units(N);
  std::vector<int> cursor(start.begin(), start.begin() + G);
  for (int i = 0; i < N; ++i) {
    int g = block_int[i] - 1;
    group_units[cursor[g]++] = i;
  }

  // --- Step 2: partial Knuth shuffle within each group ---
  //
  // For group g with n_b units and m_b to treat: select m_b distinct positions
  // by shuffling the first m_b slots of group_units[start[g]..start[g+1]-1].
  // P(any particular m_b-subset) = 1 / C(n_b, m_b). Exactly correct.

  IntegerVector result(N, 0);
  GetRNGstate();
  for (int g = 0; g < G; ++g) {
    int n_b = count[g];
    int m_b = m_per_block[g];
    int* grp = &group_units[start[g]];

    for (int k = 0; k < m_b; ++k) {
      // Uniform integer in [k, n_b - 1]
      int j = k + static_cast<int>(unif_rand() * (n_b - k));
      if (j >= n_b) j = n_b - 1;    // guard: unif_rand() extremely rarely == 1
      if (j != k) std::swap(grp[k], grp[j]);
      result[grp[k]] = 1;
    }
  }
  PutRNGstate();

  return result;
}
