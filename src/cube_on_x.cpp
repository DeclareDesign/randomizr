#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>
using namespace Rcpp;

// Cube-on-X (Deville and Tille 2004; Chauvet and Tille 2006 window).
// Separate entry from cube.cpp so the count-tight two-arm leftover pairing
// and multi-arm per-block landing stay the hot path when formula is NULL.
// This file is used only when balanced_ra(formula = ...) builds a balancing
// matrix X. Nothing here is called from cube_two_arm_cpp / cube_multi_cpp.

// Find u != 0 in the kernel of A (q x w, row-major): A u = 0.
// A is X_window' when the window has w units and q balancing columns.
static bool kernel_vector(const std::vector<double>& A, int q, int w,
                          std::vector<double>& u, double eps) {
  if (w < 1) return false;
  u.assign(w, 0.0);
  if (q < 1) {
    // No constraint: any direction is in the kernel. Pair the first two
    // coordinates (pivotal) when possible.
    if (w == 1) return false;
    u[0] = 1.0;
    u[1] = -1.0;
    return true;
  }

  std::vector<double> M = A;
  std::vector<char> is_piv(w, 0);
  std::vector<int> piv_col;
  piv_col.reserve(q);
  int rank = 0;
  for (int c = 0; c < w && rank < q; c++) {
    int best = rank;
    double bestabs = std::fabs(M[(size_t) rank * w + c]);
    for (int r = rank + 1; r < q; r++) {
      double a = std::fabs(M[(size_t) r * w + c]);
      if (a > bestabs) { bestabs = a; best = r; }
    }
    if (bestabs < eps) continue;
    if (best != rank) {
      for (int j = 0; j < w; j++)
        std::swap(M[(size_t) rank * w + j], M[(size_t) best * w + j]);
    }
    double piv = M[(size_t) rank * w + c];
    for (int j = 0; j < w; j++) M[(size_t) rank * w + j] /= piv;
    for (int r = 0; r < q; r++) {
      if (r == rank) continue;
      double f = M[(size_t) r * w + c];
      if (f == 0.0) continue;
      for (int j = 0; j < w; j++)
        M[(size_t) r * w + j] -= f * M[(size_t) rank * w + j];
    }
    is_piv[c] = 1;
    piv_col.push_back(c);
    rank++;
  }

  std::vector<int> free_cols;
  for (int c = 0; c < w; c++) if (!is_piv[c]) free_cols.push_back(c);
  if (free_cols.empty()) return false;

  int idx = (int) std::floor(unif_rand() * (double) free_cols.size());
  if (idx < 0 || idx >= (int) free_cols.size()) idx = 0;
  int jf = free_cols[idx];
  u[jf] = 1.0;
  for (size_t r = 0; r < piv_col.size(); r++) {
    int pc = piv_col[r];
    double s = 0.0;
    for (int j = 0; j < w; j++) {
      if (j == pc) continue;
      s += M[(size_t) r * w + j] * u[j];
    }
    u[pc] = -s;
  }

  double nrm = 0.0, resid = 0.0;
  for (int j = 0; j < w; j++) nrm = std::max(nrm, std::fabs(u[j]));
  if (nrm < eps) return false;
  for (int j = 0; j < w; j++) u[j] /= nrm;

  for (int r = 0; r < q; r++) {
    double au = 0.0;
    for (int j = 0; j < w; j++) au += A[(size_t) r * w + j] * u[j];
    resid = std::max(resid, std::fabs(au));
  }
  return resid < 1e-5;
}

static void fisher_yates(std::vector<int>& v) {
  for (int i = (int) v.size() - 1; i > 0; i--) {
    int j = (int) std::floor(unif_rand() * (i + 1.0));
    if (j < 0) j = 0;
    if (j > i) j = i;
    std::swap(v[i], v[j]);
  }
}

static void snap_unit(NumericVector& z, int i, double tol) {
  if (z[i] <= tol) z[i] = 0.0;
  else if (z[i] >= 1.0 - tol) z[i] = 1.0;
  else if (z[i] < 0.0) z[i] = 0.0;
  else if (z[i] > 1.0) z[i] = 1.0;
}

// One martingale step on a window: z <- z +/- d * u, hitting a 0/1 bound.
static bool cube_on_x_step(NumericVector& z, const std::vector<int>& W,
                           const std::vector<double>& u, double tol) {
  int w = (int) W.size();
  double dplus = R_PosInf, dminus = R_PosInf;
  for (int j = 0; j < w; j++) {
    double uj = u[j], zj = z[W[j]];
    if (std::fabs(uj) < 1e-14) continue;
    if (uj > 0.0) {
      dplus  = std::min(dplus,  (1.0 - zj) / uj);
      dminus = std::min(dminus, zj / uj);
    } else {
      dplus  = std::min(dplus,  -zj / uj);
      dminus = std::min(dminus, (zj - 1.0) / uj);
    }
  }
  if (!R_FINITE(dplus + dminus) || dplus + dminus <= 1e-15) return false;

  bool up = unif_rand() < dminus / (dplus + dminus);
  double d = up ? dplus : -dminus;
  bool frozen = false;
  for (int j = 0; j < w; j++) {
    int i = W[j];
    z[i] += d * u[j];
    snap_unit(z, i, tol);
    if (z[i] <= 0.0 || z[i] >= 1.0) frozen = true;
  }
  if (!frozen) {
    // The step above is sized so that at least one unit lands exactly on 0 or
    // on 1, which is how the draw makes progress. In rare cases rounding error
    // leaves every unit a hair short of its bound, and then nothing has been
    // settled and the loop would spin. The unit with the least room left is
    // therefore settled directly, by a coin weighted by the value it currently
    // holds. That coin keeps the unit's assignment probability exactly right,
    // because a unit sitting at z is treated with probability z either way.
    // What it does not keep is the balancing constraint: the value it moves is
    // not the value the constraint expected to move, so a draw that reaches
    // this line can end up one unit away from the count the constraint implies.
    int best = W[0];
    double slack = std::min(z[best], 1.0 - z[best]);
    for (int j = 1; j < w; j++) {
      double s = std::min(z[W[j]], 1.0 - z[W[j]]);
      if (s < slack) { slack = s; best = W[j]; }
    }
    z[best] = (unif_rand() < z[best]) ? 1.0 : 0.0;
  }
  return true;
}

static bool try_window(NumericVector& z, const std::vector<int>& W,
                       const std::vector<double>& Xs, int n, int q_use,
                       std::vector<double>& A, std::vector<double>& u,
                       double tol) {
  int w = (int) W.size();
  A.assign((size_t) q_use * w, 0.0);
  for (int j = 0; j < w; j++) {
    int i = W[j];
    for (int c = 0; c < q_use; c++)
      A[(size_t) c * w + j] = Xs[(size_t) i + (size_t) c * n];
  }
  return kernel_vector(A, q_use, w, u, 1e-10) &&
         cube_on_x_step(z, W, u, tol);
}

// [[Rcpp::export]]
NumericVector cube_on_x_cpp(NumericVector p, NumericMatrix X, double tol) {
  int n = p.size();
  int q = X.ncol();
  if (X.nrow() != n) stop("X has %d rows and p has length %d.", X.nrow(), n);

  NumericVector z = clone(p);
  for (int i = 0; i < n; i++) snap_unit(z, i, tol);

  // Column-scale X so GE sees O(1) entries. Column scaling does not change
  // ker(X'): (X D)' u = 0 iff X' u = 0 when D is invertible.
  std::vector<double> Xs((size_t) n * std::max(q, 0));
  for (int c = 0; c < q; c++) {
    double ss = 0.0;
    for (int i = 0; i < n; i++) ss += X(i, c) * X(i, c);
    double s = std::sqrt(ss / std::max(n, 1));
    if (s < 1e-15) s = 1.0;
    for (int i = 0; i < n; i++) Xs[(size_t) i + (size_t) c * n] = X(i, c) / s;
  }

  // Order units by the first column of X that is not constant, so the
  // remainder that landing sees is a set of units with nearby values of that
  // column. An intercept is a column of ones and so is skipped by that rule,
  // which makes the sort column x under the usual ~ x and x1 under
  // ~ 0 + x1 + x2. Testing for constancy rather than assuming the intercept
  // sits in column 0 is what keeps those two cases consistent, and it is what
  // lets balanced_ra() tell a caller to put the covariate they least trust
  // themselves to model first in the formula.
  //
  // Flight steps stay martingales whatever the order, so nothing about the
  // stated guarantees rides on this; a random reverse only avoids always
  // leaving the same tail for landing. When every column is constant (~ 1, or
  // a degenerate covariate) there is nothing to sort on, and keeping the
  // identity order would make the window walk pair adjacent units
  // deterministically: units 1 and 2 would receive opposite conditions on
  // every draw. Marginals survive that, but the joint distribution becomes a
  // systematic paired design nobody asked for, so that case gets a shuffle.
  std::vector<int> ord(n);
  for (int i = 0; i < n; i++) ord[i] = i;
  int sort_col = -1;
  for (int c = 0; c < q && sort_col < 0; c++) {
    double v0 = Xs[(size_t) 0 + (size_t) c * n];
    for (int i = 1; i < n; i++) {
      if (Xs[(size_t) i + (size_t) c * n] != v0) { sort_col = c; break; }
    }
  }
  if (sort_col >= 0) {
    std::sort(ord.begin(), ord.end(), [&](int a, int b) {
      return Xs[(size_t) a + (size_t) sort_col * n] <
             Xs[(size_t) b + (size_t) sort_col * n];
    });
    if (unif_rand() < 0.5) std::reverse(ord.begin(), ord.end());
  } else {
    fisher_yates(ord);
  }

  std::vector<int> queue;
  queue.reserve(n);
  for (int t = 0; t < n; t++) {
    int i = ord[t];
    if (z[i] > tol && z[i] < 1.0 - tol) queue.push_back(i);
  }

  // The queue is consumed through a head index rather than by erasing its
  // front: erase() shifts every remaining element, which is O(queue) per
  // step and made the whole flight quadratic in n. The consumed prefix is
  // compacted away once it dominates, so memory stays O(n).
  size_t head = 0;
  int q_use = q;
  std::vector<double> A, u;
  long long guard = (long long) n * (q + 3) + 20;
  while (guard-- > 0) {
    int nf = (int) (queue.size() - head);
    if (nf == 0) break;
    if (q_use < 0) q_use = 0;
    if (q_use == 0) {
      for (int t = 0; t < nf; t++) {
        int i = queue[head + t];
        z[i] = (unif_rand() < z[i]) ? 1.0 : 0.0;
      }
      break;
    }

    // Fast flight: first q+1 of the queue. Survivors go to the back so
    // a leftover from the low end is not carried across the range and
    // paired with the opposite extreme.
    int w = std::min(nf, q_use + 1);
    std::vector<int> W(queue.begin() + head, queue.begin() + head + w);
    bool moved = try_window(z, W, Xs, n, q_use, A, u, tol);
    if (!moved && w < nf) {
      std::vector<int> Wall(queue.begin() + head, queue.end());
      moved = try_window(z, Wall, Xs, n, q_use, A, u, tol);
      if (moved) { W.swap(Wall); w = nf; }
    }
    if (!moved) {
      q_use--;
      continue;
    }

    // Only the window's units moved, so the tail is still all-fractional and
    // needs no rescan.
    head += w;
    for (int t = 0; t < w; t++) {
      int i = W[t];
      if (z[i] > tol && z[i] < 1.0 - tol) queue.push_back(i);
    }
    if (head > 4096 && head * 2 > queue.size()) {
      queue.erase(queue.begin(), queue.begin() + head);
      head = 0;
    }
  }

  for (int i = 0; i < n; i++) {
    if (z[i] > tol && z[i] < 1.0 - tol)
      z[i] = (unif_rand() < z[i]) ? 1.0 : 0.0;
    z[i] = (z[i] > 0.5) ? 1.0 : 0.0;
  }
  return z;
}
