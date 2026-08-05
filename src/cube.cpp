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

// [[Rcpp::export]]
NumericVector cube_two_arm_cpp(NumericVector p, IntegerVector b,
                               IntegerVector ord, double tol) {
  int n = p.size();
  NumericVector z = clone(p);
  int nb = 0;
  for (int i = 0; i < n; i++) if (b[i] > nb) nb = b[i];

  std::vector<int> open(nb + 1, -1);
  for (int t = 0; t < n; t++) {
    int j = ord[t] - 1;
    if (z[j] <= tol || z[j] >= 1.0 - tol) continue;
    int bl = b[j];
    if (open[bl] < 0) { open[bl] = j; continue; }

    int i = open[bl];
    double du = std::min(1.0 - z[i], z[j]);
    double dd = std::min(z[i], 1.0 - z[j]);
    if (unif_rand() < dd / (du + dd)) { z[i] += du; z[j] -= du; }
    else                              { z[i] -= dd; z[j] += dd; }

    int keep = (z[i] > tol && z[i] < 1.0 - tol) ? i : j;
    open[bl] = (z[keep] > tol && z[keep] < 1.0 - tol) ? keep : -1;
  }

  // At most one unit per block survives. Rounding it fairly moves that block's
  // count by less than one, so the count stays floor-or-ceiling.
  for (int bl = 1; bl <= nb; bl++) {
    if (open[bl] >= 0) z[open[bl]] = (unif_rand() < z[open[bl]]) ? 1.0 : 0.0;
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
// Paths are needed only once per block, on the fewer than k units left after
// the block runs out, which is the landing phase.

// Move along the given walk, alternating the sign of the step. Consecutive
// edges share a node, so alternating leaves every unit's row total and every
// interior arm's column total untouched. The step is a fair bet, which is what
// keeps each cell's expectation equal to the probability that was asked for.
static void cube_move(std::vector<double>& Z, int n,
                      const std::vector<int>& cu, const std::vector<int>& ca,
                      double tol) {
  int m = cu.size();
  double dplus = R_PosInf, dminus = R_PosInf;
  for (int e = 0; e < m; e++) {
    double z = Z[cu[e] + (size_t) ca[e] * n];
    if (e % 2 == 0) { dplus = std::min(dplus, 1.0 - z); dminus = std::min(dminus, z); }
    else            { dplus = std::min(dplus, z); dminus = std::min(dminus, 1.0 - z); }
  }
  if (!R_FINITE(dplus + dminus) || dplus + dminus <= 0) return;

  bool up = unif_rand() < dminus / (dplus + dminus);
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

// One move on the working set: a cycle if the graph has one, a path otherwise.
// Returns false when there is nothing fractional left to move.
static bool cube_step(std::vector<double>& Z, int n, int k,
                      const std::vector<int>& W, double tol, CubeWork& ws) {
  int w = W.size();
  int nv = w + k;

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

  // Strip leaves repeatedly. What survives is the 2-core, which holds every
  // cycle, so a walk inside it cannot dead-end and must close. Finding a cycle
  // whenever one exists is what keeps the arm totals exact: only path moves
  // disturb them, and those are deferred to the end of the block.
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
    for (int e = 0; e < ne; e++) ws.allowed[e] = !ws.edead[e];
  } else {
    for (int e = 0; e < ne; e++) ws.allowed[e] = 1;
    // A forest. Every unit has degree at least two, so every leaf is an arm,
    // and starting at one gives a maximal arm-to-arm path.
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

// [[Rcpp::export]]
NumericMatrix cube_multi_cpp(NumericMatrix P, IntegerVector b,
                             IntegerVector ord, double tol) {
  int n = P.nrow(), k = P.ncol();
  std::vector<double> Z(P.begin(), P.end());
  int nb = 0;
  for (int i = 0; i < n; i++) if (b[i] > nb) nb = b[i];

  CubeWork ws(k);
  std::vector<std::vector<int> > bu(nb + 1);
  for (int t = 0; t < n; t++) { int i = ord[t] - 1; bu[b[i]].push_back(i); }

  for (int bl = 1; bl <= nb; bl++) {
    std::vector<int>& units = bu[bl];
    size_t ptr = 0;
    std::vector<int> W;
    W.reserve(k);
    // Every move settles at least one cell, so this bound cannot be reached.
    long long guard = (long long) units.size() * k + 10;

    while (guard-- > 0) {
      while ((int) W.size() < k && ptr < units.size()) {
        int u = units[ptr++];
        int nf = 0;
        for (int j = 0; j < k; j++) {
          double z = Z[u + (size_t) j * n];
          if (z > tol && z < 1.0 - tol) nf++;
        }
        if (nf >= 2) W.push_back(u);
      }
      if (W.empty()) break;
      if (!cube_step(Z, n, k, W, tol, ws)) break;

      std::vector<int> keep;
      for (size_t t = 0; t < W.size(); t++) {
        int nf = 0;
        for (int j = 0; j < k; j++) {
          double z = Z[W[t] + (size_t) j * n];
          if (z > tol && z < 1.0 - tol) nf++;
        }
        if (nf >= 2) keep.push_back(W[t]);
      }
      W.swap(keep);
    }
  }

  NumericMatrix out(n, k);
  for (int i = 0; i < n * k; i++) out[i] = (Z[i] > 0.5) ? 1.0 : 0.0;
  return out;
}
