#include <Rcpp.h>
#include <vector>
#include <algorithm>
using namespace Rcpp;

// Cube method (Deville and Tille 2004) on the transportation polytope, which is
// what balanced_ra() draws from. See R/balanced_ra.R for what the algorithm is
// doing and why; this file is the fast path for the two loops that dominate.

// ---- two conditions --------------------------------------------------------
//
// With two conditions the state is a single vector, since the second column is
// one minus the first. Every unit then has exactly one edge in the fractional
// graph, to its own block, so the graph is a forest of stars: no cycle can
// exist and every maximal path is unit-block-unit. The general walk collapses
// to "hold one open unit per block, pair the next fractional unit with it, move
// one up and the other down", which is a single pass.
//
// Independent Bernoulli rounding of the leftover in each block would keep each
// block tight and let the overall count drift by up to the number of blocks.
// A second pass pairs those leftovers as one group, so the overall count stays
// tight as well. Each leftover is still 0 or 1, so no block moves by more than
// one from its target.

// Bounds for both entry points, before anything indexes. An NA block reaches
// here as NA_INTEGER (INT_MIN) after as.integer(factor(blocks)), and indexing
// with it corrupts memory; check_inputs = FALSE can waive design checking but
// not memory safety. block_assign_cpp() carries the same guards.
static void cube_check_index(const IntegerVector& b, const IntegerVector& ord,
                             int n) {
  if (b.size() != n) {
    stop("`blocks` indexes %d units but there are %d.", (int) b.size(), n);
  }
  if (ord.size() != n) {
    stop("The unit order has length %d but there are %d units.",
         (int) ord.size(), n);
  }
  for (int i = 0; i < n; i++) {
    if (b[i] == NA_INTEGER) {
      stop("`blocks` must not contain NA (unit %d).", i + 1);
    }
    if (b[i] < 1) {
      stop("Block index %d at unit %d is below 1.", b[i], i + 1);
    }
    if (ord[i] == NA_INTEGER || ord[i] < 1 || ord[i] > n) {
      stop("The unit order must be a permutation of 1:%d.", n);
    }
  }
}

// Pair fractional units that share a block id (two-arm count kernel, not
// cube-on-X). Leaves at most one fractional unit per id.
static void cube_pivot_pass(NumericVector& z, const std::vector<int>& seq,
                            const int* blk, int nb, double tol) {
  std::vector<int> open(nb + 1, -1);
  for (size_t t = 0; t < seq.size(); t++) {
    int j = seq[t];
    // Step 1: skip assigned.
    if (z[j] <= tol || z[j] >= 1.0 - tol) continue;
    int bl = blk[j];
    // Step 2: hold one open unit per block.
    if (open[bl] < 0) { open[bl] = j; continue; }

    // Step 3: kernel pair (z_i + z_j preserved).
    int i = open[bl];
    // Step 4: largest d+ and d-.
    double du = std::min(1.0 - z[i], z[j]);
    double dd = std::min(z[i], 1.0 - z[j]);
    // Step 5: fair bet, then transfer.
    if (unif_rand() < dd / (du + dd)) { z[i] += du; z[j] -= du; }
    else                              { z[i] -= dd; z[j] += dd; }

    // Step 6: housekeeping of open[].
    int keep = (z[i] > tol && z[i] < 1.0 - tol) ? i : j;
    open[bl] = (z[keep] > tol && z[keep] < 1.0 - tol) ? keep : -1;
  }
}

// [[Rcpp::export]]
NumericVector cube_two_arm_cpp(NumericVector p, IntegerVector b,
                               IntegerVector ord, double tol) {
  int n = p.size();
  NumericVector z = clone(p);
  cube_check_index(b, ord, n);
  int nb = 0;
  for (int i = 0; i < n; i++) if (b[i] > nb) nb = b[i];

  std::vector<int> seq(n);
  for (int t = 0; t < n; t++) seq[t] = ord[t] - 1;
  cube_pivot_pass(z, seq, b.begin(), nb, tol);

  // Step 1: collect leftovers.
  std::vector<int> left;
  left.reserve(nb);
  for (int t = 0; t < n; t++) {
    int j = seq[t];
    if (z[j] > tol && z[j] < 1.0 - tol) left.push_back(j);
  }
  if (left.size() > 1) {
    // Step 2: fake a single block.
    std::vector<int> one(n, 1);
    // Step 3: same pivot pass (overall count stays tight).
    cube_pivot_pass(z, left, one.data(), 1, tol);
  }

  // Step 4: Bernoulli any singleton leftover.
  for (int i = 0; i < n; i++) {
    if (z[i] > tol && z[i] < 1.0 - tol)
      z[i] = (unif_rand() < z[i]) ? 1.0 : 0.0;
  }
  for (int i = 0; i < n; i++) z[i] = (z[i] > 0.5) ? 1.0 : 0.0;
  return z;
}

// ---- three or more conditions ----------------------------------------------
//
// The R version rebuilt the whole bipartite graph on every move, which is
// quadratic in the number of units. It does not have to look at every unit: a
// working set of k units whose cells are still fractional always contains a
// cycle, because each such unit has at least two fractional cells (a unit with
// exactly one would need that cell to be an integer, since its row sums to 1
// and its other cells are 0 or 1), so the induced graph has at least 2k edges
// on at most 2k nodes, and a graph with as many edges as nodes is not a forest.
//
// So each move only ever examines k units, and since every move settles at
// least one of the n*k cells the whole draw is linear in the number of units.
// Flight (cycles) stays inside each block, and each block is then landed on
// its own (see the comment at cube_multi_cpp), so the within-block arm counts
// stay tight; overall totals may wander when several blocks' remainders land
// the same way.

// Move along the given walk, alternating the sign of the step. Consecutive
// edges share a node, so alternating leaves every unit's row total and every
// interior arm's column total untouched. The step is a fair bet, which is what
// keeps each cell's expectation equal to the probability that was asked for.
static void cube_move(std::vector<double>& Z, int n,
                      const std::vector<int>& cu, const std::vector<int>& ca,
                      double tol) {
  int m = cu.size();
  double dplus = R_PosInf, dminus = R_PosInf;
  // Step 1: largest d+ and d- (alternating sign).
  for (int e = 0; e < m; e++) {
    double z = Z[cu[e] + (size_t) ca[e] * n];
    if (e % 2 == 0) { dplus = std::min(dplus, 1.0 - z); dminus = std::min(dminus, z); }
    else            { dplus = std::min(dplus, z); dminus = std::min(dminus, 1.0 - z); }
  }
  if (!R_FINITE(dplus + dminus) || dplus + dminus <= 0) return;

  // Step 2: fair bet.
  bool up = unif_rand() < dminus / (dplus + dminus);
  // Step 3: apply the transfer.
  for (int e = 0; e < m; e++) {
    size_t ix = cu[e] + (size_t) ca[e] * n;
    double s = (e % 2 == 0) ? 1.0 : -1.0;
    Z[ix] += up ? s * dplus : -s * dminus;
    if (Z[ix] < tol) Z[ix] = 0.0;
    if (Z[ix] > 1.0 - tol) Z[ix] = 1.0;
  }
}

// Scratch space for cube_step. The working set never exceeds k units, so every
// buffer here has a size fixed by k and can be allocated once per draw rather
// than once per move, which is what the allocation cost otherwise dominates.
struct CubeWork {
  int k, stride, nvmax;
  std::vector<int> eu, ea, adj, alen, deg, deg0, seen, we, cu, ca;
  std::vector<char> edead, vdead, allowed, used;
  std::vector<int> q;

  CubeWork(int k_) : k(k_) {
    stride = k;                       // no node can have more than k edges
    nvmax = 2 * k;                    // at most k units plus k arms
    int emax = k * k;
    eu.resize(emax); ea.resize(emax);
    adj.resize((size_t) nvmax * stride); alen.resize(nvmax);
    deg.resize(nvmax); deg0.resize(nvmax); seen.resize(nvmax);
    edead.resize(emax); vdead.resize(nvmax);
    allowed.resize(emax); used.resize(emax);
    we.reserve(emax); cu.reserve(emax); ca.reserve(emax); q.reserve(nvmax);
  }
};

// One move on the working set: a cycle if the graph has one, a path otherwise
// (unless allow_path is false, in which case a forest is left for later).
// Returns false when there is nothing fractional left to move.
static bool cube_step(std::vector<double>& Z, int n, int k,
                      const std::vector<int>& W, double tol, CubeWork& ws,
                      bool allow_path) {
  int w = W.size();
  int nv = w + k;

  // Fractional cells are the edges of the unit-arm graph.
  int ne = 0;
  for (int a = 0; a < w; a++) {
    for (int j = 0; j < k; j++) {
      double z = Z[W[a] + (size_t) j * n];
      if (z > tol && z < 1.0 - tol) { ws.eu[ne] = a; ws.ea[ne] = j; ne++; }
    }
  }
  if (ne == 0) return false;

  for (int v = 0; v < nv; v++) ws.alen[v] = 0;
  for (int e = 0; e < ne; e++) {
    int u = ws.eu[e], r = w + ws.ea[e];
    ws.adj[(size_t) u * ws.stride + ws.alen[u]++] = e;
    ws.adj[(size_t) r * ws.stride + ws.alen[r]++] = e;
  }
  for (int v = 0; v < nv; v++) { ws.deg[v] = ws.deg0[v] = ws.alen[v]; ws.vdead[v] = 0; ws.seen[v] = -1; }
  for (int e = 0; e < ne; e++) { ws.edead[e] = 0; ws.used[e] = 0; }

  // Step 1: strip leaves; survivors are the 2-core.
  ws.q.clear();
  for (int v = 0; v < nv; v++) if (ws.deg[v] == 1) ws.q.push_back(v);
  while (!ws.q.empty()) {
    int v = ws.q.back(); ws.q.pop_back();
    if (ws.vdead[v] || ws.deg[v] != 1) continue;
    ws.vdead[v] = 1;
    for (int t = 0; t < ws.alen[v]; t++) {
      int e = ws.adj[(size_t) v * ws.stride + t];
      if (ws.edead[e]) continue;
      ws.edead[e] = 1;
      int o = (v == ws.eu[e]) ? (w + ws.ea[e]) : ws.eu[e];
      ws.deg[v]--; ws.deg[o]--;
      if (ws.deg[o] == 1) ws.q.push_back(o);
    }
  }

  int startv = -1;
  bool core = false;
  for (int e = 0; e < ne; e++) if (!ws.edead[e]) { core = true; startv = ws.eu[e]; break; }
  if (core) {
    // Step 2a: cycle on the core (flight; column totals exact).
    for (int e = 0; e < ne; e++) ws.allowed[e] = !ws.edead[e];
  } else {
    if (!allow_path) return false;
    for (int e = 0; e < ne; e++) ws.allowed[e] = 1;
    // Step 2b: landing path from an arm leaf.
    for (int j = 0; j < k; j++) if (ws.deg0[w + j] == 1) { startv = w + j; break; }
    if (startv < 0) startv = ws.eu[0];
  }

  ws.we.clear();
  int v = startv;
  ws.seen[v] = 0;
  int cyc = -1;
  while (true) {
    int pick = -1;
    for (int t = 0; t < ws.alen[v]; t++) {
      int e = ws.adj[(size_t) v * ws.stride + t];
      if (ws.allowed[e] && !ws.used[e]) { pick = e; break; }
    }
    if (pick < 0) break;
    ws.used[pick] = 1;
    int o = (v == ws.eu[pick]) ? (w + ws.ea[pick]) : ws.eu[pick];
    ws.we.push_back(pick);
    if (ws.seen[o] >= 0) { cyc = ws.seen[o]; break; }
    ws.seen[o] = ws.we.size();
    v = o;
  }
  if (ws.we.empty()) return false;

  ws.cu.clear(); ws.ca.clear();
  for (size_t t = (cyc >= 0 ? (size_t) cyc : 0); t < ws.we.size(); t++) {
    ws.cu.push_back(W[ws.eu[ws.we[t]]]);
    ws.ca.push_back(ws.ea[ws.we[t]]);
  }
  cube_move(Z, n, ws.cu, ws.ca, tol);
  return true;
}

static int cube_nfrac(const std::vector<double>& Z, int n, int k, int u,
                      double tol) {
  int nf = 0;
  for (int j = 0; j < k; j++) {
    double z = Z[u + (size_t) j * n];
    if (z > tol && z < 1.0 - tol) nf++;
  }
  return nf;
}

// Chauvet-Tille window of k units. allow_path true is landing.
static void cube_process(std::vector<double>& Z, int n, int k,
                         const std::vector<int>& units, double tol,
                         CubeWork& ws, bool allow_path) {
  size_t ptr = 0;
  std::vector<int> W;
  W.reserve(k);
  long long guard = (long long) units.size() * k + 10;
  while (guard-- > 0) {
    // Fill the window with units that still have >= 2 fractional cells.
    while ((int) W.size() < k && ptr < units.size()) {
      int u = units[ptr++];
      if (cube_nfrac(Z, n, k, u, tol) >= 2) W.push_back(u);
    }
    if (W.empty()) break;
    if (!cube_step(Z, n, k, W, tol, ws, allow_path)) break;
    // Drop units that have settled.
    std::vector<int> keep;
    for (size_t t = 0; t < W.size(); t++) {
      if (cube_nfrac(Z, n, k, W[t], tol) >= 2) keep.push_back(W[t]);
    }
    W.swap(keep);
  }
}

// [[Rcpp::export]]
NumericMatrix cube_multi_cpp(NumericMatrix P, IntegerVector b,
                             IntegerVector ord, double tol) {
  int n = P.nrow(), k = P.ncol();
  std::vector<double> Z(P.begin(), P.end());
  cube_check_index(b, ord, n);
  int nb = 0;
  for (int i = 0; i < n; i++) if (b[i] > nb) nb = b[i];

  CubeWork ws(k);
  std::vector<std::vector<int> > bu(nb + 1);
  for (int t = 0; t < n; t++) { int i = ord[t] - 1; bu[b[i]].push_back(i); }

  // Two-arm leftover coupling does not extend: a block can keep several
  // leftover units, and landing them as one global group can push a
  // block-arm count more than one away from its target. Each block is
  // therefore landed on its own. Overall tightness then follows when every
  // block target is an integer, and may slip when several remainders land
  // the same way.
  for (int bl = 1; bl <= nb; bl++)
    cube_process(Z, n, k, bu[bl], tol, ws, true);

  NumericMatrix out(n, k);
  for (int i = 0; i < n * k; i++) out[i] = (Z[i] > 0.5) ? 1.0 : 0.0;
  return out;
}
