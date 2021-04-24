#include "beeswarm.h"

#include <float.h>
#include <math.h>
#include <stdbool.h>
#include <R_ext/Utils.h>
#include <R_ext/Visibility.h>

static bool is_y_feasible(double y, double *pre_dx_sq,
    double *pre_y, int pre_xy_len)
{
  for (int i=0; i<pre_xy_len; i++) {
    double y_diff = y - pre_y[i];
    if (pre_dx_sq[i] + y_diff * y_diff < 0.999)
      return false;
  }
  return true;
}

static void swarm(double *x, int n, int side,
    int *placed, double *workspace, double *y)
{
  for (int i=0; i<n; i++) {
    // place the i^th point

    R_CheckUserInterrupt();

    // poty is an array of potential y values for the next point to be placed
    double *poty = workspace;
    int poty_len = 0;
    poty[poty_len++] = 0;

    // poty_low contains potential y values that are below the y values of
    // previously-placed points; these will be added to poty if side != 1
    double *poty_low = workspace + n;
    int poty_low_len = 0;

    // nearby_y is an array containing the y values of previously-placed points
    // that are within a distance of 1 horizontally from the i^th point (which
    // is the next point to be placed).  There is one element of nearby_dx_sq
    // for each element of nearby_y; each element of nearby_dx_sq corresponds to
    // the square of the horizontal distance from a point to the i^th point.
    double *nearby_dx_sq = workspace + 2 * n;
    double *nearby_y = workspace + 3 * n;
    int nearby_xy_len = 0;

    for (int j=0; j<i; j++) {
      if (fabs(x[i] - x[j]) >= 1) continue;
      double x_diff = x[i] - x[j];
      nearby_dx_sq[nearby_xy_len] = x_diff * x_diff;
      nearby_y[nearby_xy_len] = y[j];
      nearby_xy_len++;
      double poty_off = sqrt(1 - x_diff * x_diff);
      poty[poty_len++] = y[j] + poty_off;
      poty_low[poty_low_len++] = y[j] - poty_off;
    }
    if (side == -1)
        poty_len = 1;   // remove poty values > 0
    if (side != 1)
      for (int j=0; j<poty_low_len; j++)
        poty[poty_len++] = poty_low[j];
    y[i] = DBL_MAX;
    for (int j=0; j<poty_len; j++) {
      if (fabs(poty[j]) < fabs(y[i]) &&
            is_y_feasible(poty[j], nearby_dx_sq, nearby_y, nearby_xy_len)) {
        y[i] = poty[j];
      }
    }
  }
}

/* Return the index of a unplaced point whose best permitted y value is as
 * close as possible to zero
 */
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

static void compactSwarm(double *x, int n, int side,
    int *placed, double *workspace, double *y)
{
  double *y_low = workspace;       // largest permitted negative y value
  double *y_high = workspace + n;  // smallest permitted positive y value
  double *y_best = workspace + 2 * n; //  current best permitted y value

  for (int iter=0; iter<n; iter++) {
    R_CheckUserInterrupt();
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
      if (side == 0) {
        double y_lo = fmin(y_low[j], yi - y_offset);
        y_low[j] = y_lo;
        y_best[j] = -y_lo < y_hi ? y_lo : y_hi;
      } else {
        y_best[j] = y_hi;
      }
    }
  }

  if (side == -1)
    for (int i=0; i<n; i++)
      y[i] = -y[i];
}

/* Compute a beeswarm layout for the array x.
 *
 * Parameters:
 * x         circle positions on data axis
 * n         length of x
 * compact   use compact layout?
 * side      -1, 0, or 1
 * placed    which circles have been placed (logical type)
 * workspace an array of doubles for internal use
 * y         (output) circle positions on non-data axis
 */
void attribute_hidden calculateSwarm(double *x, int *n, int *compact, int *side,
    int *placed, double *workspace, double *y)
{
  if (*compact) {
    compactSwarm(x, *n, *side, placed, workspace, y);
  } else {
    swarm(x, *n, *side, placed, workspace, y);
  }
}
