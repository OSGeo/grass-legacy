#include "gis.h"
#include "mapcalc.h"

/* Author: Michael Shapiro. 16 feb 1993 / update 12/99 on __CYGWIN__
 * NOTE:
 *   This depends on the Unix function drand48() which returns a
 *   double between 0.0 and 1.0
 * a hook is provided for seeding the random numbers by the main()
 * g_randseed(): it does nothing at present
 */

#if defined(__CYGWIN__) || defined(__APPLE__)
#define drand48() rand()/32767.0
#else
extern double drand48();
#endif

int 
g_randseed (void)
{
/* this could be written to seed the random numbers based on the time.
 * but I have grown gun shy of UNix include files (eg, <time.h>) and
 * their "types" (time_t) not to mention the need to generate 3 shorts
 * for the seed to drand48()
 */

    return 0;
}

int 
i_rand (int argc, CELL *argv[], register CELL *cell, register int ncols)
{
    register int i;
    CELL lo, hi;
    double x;

    for (i = 0; i < ncols; i++)
    {
	x = drand48();             /* x is between 0.0 and 1.0 */
	lo = argv[0][i];
	hi = argv[1][i];
	if (lo > hi)
	{
	    lo = argv[1][i];
	    hi = argv[0][i];
	}
    /* x*(hi-lo)+lo: biases results against lo,hi due to rounding */
	cell[i] = (CELL)round(x*(hi - lo + 1) + lo - .5);
	if (cell[i] > hi)
	    cell[i] = hi;
	if (cell[i] < lo)
	    cell[i] = lo;
    }

    return 0;
}

/* NOTE: if this function is used, the results will be biased at the end points
 * when cast to CELL
 */
int 
x_rand (int argc, double *argv[], register double *cell, register int ncols)
{
    register int i;
    double lo, hi;
    double x;

    for (i = 0; i < ncols; i++)
    {
	x = drand48();             /* x is between 0.0 and 1.0 */
	lo = argv[0][i];
	hi = argv[1][i];
	cell[i] = x*(hi - lo) + lo;
    }

    return 0;
}

int 
n_rand (int n, char *name)
{
    if (n==2) return 1;
    fprintf (stderr, "%s - ", name);
    if (n==0)
	fprintf (stderr, "no arguments ");
    else if (n<2)
	fprintf (stderr, "less than two arguments ");
    else
	fprintf (stderr, "more than two arguments ");
    fprintf (stderr, "specified. usage: %s(low,high)\n", name);
    return 0;
}

