#include "beeswarm.h"

#include <math.h>

static int which_min_abs(double *y_best, int *placed, int n)
{
  int i = 0;
  while (placed[i]) ++i;
  double best_val = fabs(y_best[i]);
  int result = i;
  for (++i; i<n; ++i) {
    if (placed[i]) continue;
    double a = fabs(y_best[i]);
    if (a < best_val) {
      best_val = a;
      result = i;
    }
  }
  return result;
}

/* Parameters:
 * x         circle positions on data axis
 * n_ptr     length of x
 * side      -1, 0, or 1
 * placed    which circles have been placed (logical type)
 * workspace an array of doubles for internal use
 * y         (output) circle positions on non-data axis
 */
void compactSwarm(double *x, int *n_ptr, int *side, int *placed,
    double *workspace, double *y)
{
  int n = *n_ptr;

  double *y_low = workspace;       // largest permitted negative y value
  double *y_high = workspace + n;  // smallest permitted positive y value
  double *y_best = workspace + 2 * n; //  current best permitted y value

  for (int iter=0; iter<n; iter++) {
    int i = which_min_abs(y_best, placed, n);
    double xi = x[i];
    double yi = y_best[i];
    y[i] = yi;
    placed[i] = 1;
    for (int j=0; j<n; j++) {
      if (placed[j]) continue;
      double xdiff = fabs(xi - x[j]);
      if (xdiff >= 1) continue;
      double y_offset = sqrt(1 - xdiff * xdiff);
      double y_hi = fmax(y_high[j], yi + y_offset);
      y_high[j] = y_hi;
      if (*side == 0) {
        double y_lo = fmin(y_low[j], yi - y_offset);
        y_low[j] = y_lo;
        y_best[j] = -y_lo < y_hi ? y_lo : y_hi;
      } else {
        y_best[j] = y_hi;
      }
    }
  }

  if (*side == -1) {
    for (int i=0; i<n; i++) {
      y[i] = -y[i];
    }
  }
}
