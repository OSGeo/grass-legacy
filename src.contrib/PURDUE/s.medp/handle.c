/*- handle.c --nasty data handling function.
 *
 * Assuming that you've recently read the man page, allow me to explain what
 * this function is trying to achieve.  The input and grid data has been read
 * and now we need to get it into a form suitable to polish. Consider first
 * the possibility that each node has either one or no site associated with
 * it.  After verifying that the grid is actually a grid (possibly rotated by
 * an arbitrary angle) and finding the nearest node for each site, we can
 * form a polish table. Visually,
 *
 *      0                      q
 *      -------------------------
 *    0 |#|#|#|#|#|#|#|#|#|#|#|#|         The polish table:
 *      |#|#|#|#|#|#|#|#|#|#|#|#|         y[k][el], k=0,...,p, el=0,...,q
 *      |#|#|#|@|#|@|#|@|@|@|#|#|
 *      |#|#|@|#|@|#|@|#|@|@|#|#|          # == EMPTY_CELL
 *      |#|@|#|@|#|@|#|@|#|@|#|#|          @ == some site
 *      |#|#|@|#|@|#|@|#|@|#|#|#|
 *      |#|@|@|#|@|#|@|@|@|@|#|#|
 *      |#|@|#|@|#|@|#|@|#|@|#|#|
 *      |#|@|#|#|@|#|@|@|@|#|#|#|
 *      |#|#|@|@|#|@|@|#|@|#|#|#|
 *      |#|#|@|#|@|#|@|#|@|#|#|#|
 *      |#|#|#|#|#|#|#|#|#|#|#|#|
 *      |#|#|#|#|#|#|#|#|#|#|#|#|
 *    p |#|#|#|#|#|#|#|#|#|#|#|#|
 *      -------------------------
 *
 *
 * Important here is the concept of an empty cell (denoted by #). The grid
 * has (p+1)*(q+1) nodes, but there are  2+1 rows and 1+1 columns that are
 * completely empty.  To make output easier to format, we must reduce p by 2
 * and q by 1. Once that is done, we can zero out the bottom and right fringe
 * (collector cells). Now, we should also make sure that any extra top and
 * left fringe areas are empty. Once this is done, we are ready to do the
 * polish.
 *
 *
 * This basically describes what this function is doing. It could probably be
 * done more efficiently, but...
 */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "polish.h"

handle (Map, no_sites, verbose, thresh, effects, report, stream)
  struct Map_info *Map;
  double thresh;
  int no_sites, verbose, effects, report;
  FILE *stream;
{
  int i, j, p, q, k, el, tmpk, tmpel, tmpd, maxdepth, d;
  char pad[10];
  double *tmpx, *tmpy, maxdist, distance, ***y, ***yold, ***ysave;
  double **row_g, **col_g, *all_g, longestline (), atof ();
  extern int *sitex, *sitey, *sitecat;
  extern float *datax;
  extern float *datay;
  extern int *cats;
  extern char **desc;
  extern struct Cell_head window;

  /*
   * note that nodes, areas, and lines are indexed beginning with 1, not 0
   * like a conventional C array
   */

  tmpx = (double *) G_malloc ((Map->n_nodes + 1) * sizeof (double));
  tmpy = (double *) G_malloc ((Map->n_nodes + 1) * sizeof (double));
  col_g = (double **) G_malloc ((int) (.5 * Map->n_nodes) * sizeof (double *));
  row_g = (double **) G_malloc ((int) (.5 * Map->n_nodes) * sizeof (double *));
  all_g = (double *) G_malloc (2 * sizeof (double));
  for (k = 0; k < (int) (.5 * Map->n_nodes); ++k)
  {
    col_g[k] = (double *) G_malloc (2 * sizeof (double));
    row_g[k] = (double *) G_malloc (2 * sizeof (double));
  }
  /* determine p and q, error check for non-rectangular grid */
  if (examine_grid (Map, &p, &q, tmpx, tmpy, verbose, row_g, col_g, all_g))
    G_fatal_error ("Problems with vector map---may not be rectangular");

  /*
   * Now allocate 3D arrays, (p+1) x (q+1) x maxdepth.  (we need the extra
   * elements for the median polish, to store all, row, and column effects).
   * Since we do not know maxdepth until all sites have picked their nodes,
   * we start out by making the 2D array only 1 deep.
   */
  y = (double ***) G_malloc ((p + 1) * sizeof (double **));
  yold = (double ***) G_malloc ((p + 1) * sizeof (double **));
  ysave = (double ***) G_malloc ((p + 1) * sizeof (double **));
  for (k = 0; k <= p; ++k)
  {
    y[k] = (double **) G_malloc ((q + 1) * sizeof (double *));
    yold[k] = (double **) G_malloc ((q + 1) * sizeof (double *));
    ysave[k] = (double **) G_malloc ((q + 1) * sizeof (double *));
    for (el = 0; el <= q; ++el)
    {
      y[k][el] = (double *) G_malloc (sizeof (double));
      yold[k][el] = (double *) G_malloc (sizeof (double));
      ysave[k][el] = (double *) G_malloc (sizeof (double));
      if (y[k][el] == NULL || yold[k][el] == NULL || ysave[k][el] == NULL)
	G_fatal_error ("error allocating polishing grid");
      else
      {
	ysave[k][el][0] = y[k][el][0] = EMPTY_CELL;
	yold[k][el][0] = 0.0;
      }
    }
  }

  /*
   * Initialize--associate site w/ closest node, but first we need to find
   * out the maximum number of sites that will be associated with a single
   * node so that we can know how deep to realloc {y} and {yold}. To save
   * repeating this search, we can store the depth in {yold} and dynamically
   * realloc {y} if necessary. Then, after we're finished, we can realloc
   * {yold}. BTW, {maxdist} is the maximum distance that a site can be from a
   * node (so we know that if a site is closer than this to a particular
   * node, then it *must* associate with that node.
   */
  maxdist = 0.5 * sqrt (2 * pow (longestline (Map), 2.0));
  for (maxdepth = 1, i = 0; i < no_sites; ++i)
  {
    for (k = 0; k < p; ++k)
      for (el = 0; el < q; ++el)
	if (hypot (datax[i] - tmpx[k * p + el], datay[i] - tmpy[k * p + el]) < maxdist)
	{
	  yold[k][el][0] += 1;
	  d = yold[k][el][0];
	  if (d > 1)		/* at least one site already associated */
	  {
	    if (d > maxdepth)	/* make arrays deeper */
	    {
	      for (tmpk = 0; tmpk <= p; ++tmpk)
	      {
		for (tmpel = 0; tmpel <= q; ++tmpel)
		{
		  y[tmpk][tmpel] = (double *)
		    G_realloc (y[tmpk][tmpel], d * sizeof (double));
		  yold[tmpk][tmpel] = (double *)
		    G_realloc (yold[tmpk][tmpel], d * sizeof (double));
		  ysave[tmpk][tmpel] = (double *)
		    G_realloc (ysave[tmpk][tmpel], d * sizeof (double));
		  for (tmpd = maxdepth - 1; tmpd < d; ++tmpd)
		    y[tmpk][tmpel][tmpd] = EMPTY_CELL;
		}
	      }
	    }
	    ysave[k][el][d - 1] = y[k][el][d - 1] = atof (desc[i]);
	    maxdepth = (maxdepth < d) ? d : maxdepth;
	  }
	  else
	    ysave[k][el][0] = y[k][el][0] = atof (desc[i]);

	  /* save georeferencing */
	  sprintf (desc[i], "|%d|%d|%d|%d|", k * p + el, k, el, d - 1);
	  el = q;
	  k = p;		/* get out of for loops - move to next site */
	  /*
	   * now site {i} should be attached to somewhere in {y[k][el]}. The
	   * coordinate of {y[k][el]} are ({tmpx[k*p+el],tmpy[k*p+el]).
	   */
	}
  }
  /* Now we should examine the grid and see if we can reduce {p} and/or {q} */
  for (k = p - 1; k >= 0; --k)
  {
    for (d = 0, el = q - 1; el >= 0; --el)
      if (y[k][el][0] != EMPTY_CELL)
	d = 1;
    if (d == 0)			/* still */
      p--;
  }
  for (el = q - 1; el >= 0; --el)
  {
    for (d = 0, k = p - 1; k >= 0; --k)
      if (y[k][el][0] != EMPTY_CELL)
	d = 1;
    if (d == 0)			/* still */
      q--;
  }
  p++;
  q++;				/* "One-off" adjustment ?? */

  if (verbose)
  {
    pad[0] = NULL;
    k = p + 5;
    el = 0;
    while ((k / 10) > 1.0)
      k /= 10, el++;
    k = q + 5;
    while ((k / 10) > 1.0)
      k /= 10, el++;
    for (k = 0; k < el; ++k)
      strcat (pad, " ");
    fprintf (stderr, "%d x %d grid of nodes ...%s    ", p + 1, q + 1, pad);
  }

  /* zero out row and column effect collector cells */
  for (k = 0; k <= p; ++k)
    y[k][q][0] = 0.0;
  for (el = 0; el <= q; ++el)
    y[p][el][0] = 0.0;

  /*
   * Previously, we reduced {p} & {q} (filled in extra places with EMPTY_CELL
   * values).  Now we need to go back and check the beginnings of the {y}
   * array to see if there are any empty rows or columns (in the original
   * data). If so, we need to put EMPTY_CELL values in corresponding
   * collector cells
   */

  for (k = 0; k < p; ++k)
  {
    for (d = 0, el = 0; el < q; ++el)
      if (y[k][el][0] != EMPTY_CELL)
	d = 1;
    if (d == 0)			/* still */
      y[k][q][0] = EMPTY_CELL;
    else
      k = p;			/* get out of loop */
  }

  for (el = 0; el < q; ++el)
  {
    for (d = 0, k = 0; k < p; ++k)
      if (y[k][el][0] != EMPTY_CELL)
	d = 1;
    if (d == 0)			/* still */
      y[p][el][0] = EMPTY_CELL;
    else
      el = q;			/* get out of loop */
  }

  if (verbose)
    fprintf (stderr, "% 4d sites\n", no_sites);

  /* FINALLY, do median polish */
  median_polish (y, yold, &i, p, q, maxdepth, thresh, verbose);

  /* now save residuals (and effects?) global arrays */
  j = georesid (y, p, q, maxdepth, tmpx, tmpy, effects, no_sites,
		row_g, col_g, all_g);

  if (report)
    print_report (ysave, y, stream, p, q, maxdepth, verbose);

  /* clean up and leave */
  free (tmpx);
  free (tmpy);
  free (y);
  free (yold);
  free (ysave);
  return j;
}
