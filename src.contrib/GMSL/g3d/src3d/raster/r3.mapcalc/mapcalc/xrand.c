#include "gis.h"
/* Author: Michael Shapiro. 16 feb 1993 
 * NOTE:
 *   This depends on the Unix function drand48() which returns a
 *   double between 0.0 and 1.0
 * a hook is provided for seeding the random numbers by the main()
 * g_randseed(): it does nothing at present
 */
#ifndef __CYGWIN__
extern double drand48();
#else
double drand48()
{
	return(rand()/32767.0);
}
#define srand48(sv) (srand((unsigned)(sv)))
#endif

g_randseed()
{
/* this could be written to seed the random numbers based on the time.
 * but I have grown gun shy of UNix include files (eg, <time.h>) and
 * their "types" (time_t) not to mention the need to generate 3 shorts
 * for the seed to drand48()
 */
}

/* NOTE: if this function is used, the results will be biased at the end points
 * when cast to CELL
 */
x_rand (argc, argv, cell, ncols)
    double *argv[];
    register double *cell;
    register int ncols;
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
}

n_rand (n,name) char *name;
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

