#include <math.h>
#include "geom/basic.h"
#include "geom/optri.h"

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

/*--------------------------------------------------------------------------*/

void
doMakeNeighbors (g, neighbors, n)

     void *g;
     int ***neighbors;
     int *n;

{
  int nofSites, nofNeigh, status;
  indexType qe, site;
  void *q;

  q = grQE (g); /* pointer to the (quad) edge structure of the graph */

  inciInitIncidences (g); /* initialize the incidence structure */

  *n = nofSites = siNS (grSI (g));

  if (nofSites == 0) {
    **neighbors = NULL;
    return;
  }

  *neighbors = MALLOC (int*, nofSites);

  for (site = 0; site < nofSites; site++) {
    
    /* first count the number of neighbors for site "site" */

    nofNeigh = 0;
    status = inciGetFirstIncidence (g, site, &qe);
    while (status) { /* qe is a (quad) edge incident to "site" */
      nofNeigh++;
      status = inciGetNextIncidence (g, &qe);
    }

    /* then allocate enaugh memory and store the # neighbors in position 0 */

    (*neighbors) [site] = MALLOC (int, nofNeigh + 1);
    (*neighbors) [site][0] = nofNeigh;

    /* finally store the neighbors in the "neighbors" structure */

    nofNeigh = 0;
    status = inciGetFirstIncidence (g, site, &qe);
    while (status) {
      nofNeigh++;
      (*neighbors) [site][nofNeigh] = qeDST (q, qe);
      status = inciGetNextIncidence (g, &qe);
    }
  }

  inciEnd (); /* free the memory used for incidences */
}

/*--------------------------------------------------------------------------*/

static double
findMax (d, n) /* find the largest value of abs (d) */

     double *d;
     int n;

{
  int i;
  double max, tmp;

  if (n == 0) return 0;

  max = (d[0] < 0. ? - d[0] : d[0]);
  for (i = 1; i < n; i++) {
    tmp = d[i] < 0. ? - d[i] : d[i];
    max = MAX (max, tmp);
  }

  return max;
}

/*--------------------------------------------------------------------------*/

/* returns in "neighbors" a pointer to a 2 dimensional array containing the
   number of and indices of neighbors of the sites. e.g. neighbors [i][0] 
   is the number of neighbors of site i (i.e. the site with coordinates
   (x[i], y[i])). neighbors [i][j] is the j'th neighbor of site i (i.e. the
   site with the coordinates (x[neighbors [i][j]], y[neighbors [i][j]])).
   for every i, neighbors [i][neighbors [i][0] + 1] is outside the allocated
   memory space.
*/

void
makeNeighbors (x, y, nofSites, neighbors)

     double *x, *y;
     int nofSites;
     int ***neighbors;

{
  int n, i;
  double coordMax;
  void *g, *s; 

  if (nofSites == 0) {
    *neighbors = NULL;
    return;
  }

  /* need to find the maximum value since we have to specify the */
  /* precision in "grNew". */

  coordMax = MAX (findMax (x, nofSites), findMax (y, nofSites));
  coordMax = MAX (1., coordMax);

  /* we can use at most 16 decimal digits for any coordinate. */
  /* initialize the graph; i.e. site and edge structure */

  g = grNew (nofSites, (int) (16. - ceil (log10 (coordMax))));
  s = grSI (g); /* this is a pointer to the site structure of the graph */


  /* load the sites into the graph */

  for (i = 0; i < nofSites; i++)
    siSITEload (s, (coordType) x[i], (coordType) y[i], (coordType) 0);

  siMarkDuplicateSites (s); /* duplicate sites might cause problems */
  grPlanesweepTriangulation (g); 
  grDelaunayTriangulation (g);
  grCleanConvexHull (g); /* remove "flat" triangles */

  doMakeNeighbors (g, neighbors, &n);

  if (n != nofSites) {
    fprintf (stderr, "ERROR: makeNeighbors: something went wrong.\n");
    exit (1);
  }

  grDispose (g);
}

/*--------------------------------------------------------------------------*/

void
disposeNeighbors (neighbors, nofSites)

     int **neighbors;
     int nofSites;

{
  int i;

  if (neighbors == NULL) return;

  for (i = 0; i < nofSites; i++) FREE (neighbors [i]);
  FREE (neighbors);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
