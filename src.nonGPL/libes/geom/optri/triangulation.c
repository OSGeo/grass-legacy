#include "internoptri.h"
int GLOBAL_INTERRUPT_ALGORITHM;

/*---------------------------------------------------------------------------*/

typedef int triangleType[3];

typedef struct {

  triangleType *v; /* indices of vertices of triangle in ccw order. First */
	     /* triangle stored in v[0] */
  int nofTriangles; /* # triangles stored in v */
  int maxTriangles; /* size of v */

} triangleList;

static triangleList *makeTriangleList(int);
static void copyGraphToListOfTriangles(char *, triangleList **);

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/* external interfaces for triangulation algs */

/*---------------------------------------------------------------------------*/

void saveTriangulation (char *gExternal, char *fileName)

{
  graphType *g;

  g = (graphType *) gExternal;

  grPrintGraph(fileName, g, (int) 0, (int) 0, (double) 0.0);
}

/*---------------------------------------------------------------------------*/

void copyCoordinatesToGraph (int nofSites, int *x, int *y, int *z,
  int eliminateDuplicates, char **gExternal)

{
  graphType *g;

  *gExternal = (char *) grMake ();

  g = (graphType *) *gExternal;

      /* Fix this next line.  The zero is wrong, but at least it fits
         the prototype! */
  grAllocate(g, nofSites, nofSites * 3 - 6,0);
  if (eliminateDuplicates) siMarkDuplicateSites (grSI (g));
  
  while (NS(g) < nofSites) {
    siSITEload (g, x[NS(g)], y[NS(g)], (z != NULL ? z[NS(g)] : 0));
  }
}
  
/*---------------------------------------------------------------------------*/

static triangleList *makeTriangleList (int nofTriangles)

{
  triangleList *tmp;

  tmp = MALLOC (triangleList, 1);
  tmp->v = MALLOC (triangleType, nofTriangles);

  tmp->nofTriangles = 0;
  tmp->maxTriangles = nofTriangles;

  return tmp;
}

/*---------------------------------------------------------------------------*/

static void copyGraphToListOfTriangles (char *gExternal, triangleList **tl)

{
  graphType *g;
  indexType edge, qe;

  g = (graphType *) gExternal;

  *tl = makeTriangleList (2 * NS (g) - 5);

  for (edge = 0; edge < NE(g); edge++) {
    qe = grMAKEQE (edge);
    do {
      if (! (grISDELETEDQE (g, qe) || grISDELETEDQE (g, ONEXT (g, qe)) ||
	     grISDELETEDQE (g, OPREV (g, SYM (qe)))))
	if (DST (g, ONEXT (g, qe)) == DST (g, OPREV (g, SYM (qe))))
	  if ((qe < SYM (ONEXT (g, qe))) && (qe < OPREV (g, SYM (qe)))) {
	    if ((*tl)->nofTriangles >= (*tl)->maxTriangles) {
	      fprintf (stdout,"ERROR: copyGraphToListOfTriangles\n");
	      exit (1);
	    }
	    (*tl)->v[(*tl)->nofTriangles][0] = ORG (g, qe);
	    (*tl)->v[(*tl)->nofTriangles][1] = DST (g, qe);
	    (*tl)->v[(*tl)->nofTriangles][2] = 
	      DST (g, ONEXT (g, qe));
	    (*tl)->nofTriangles += 1;
	  }
      qe = SYM (qe);
    } while (qe != grMAKEQE (edge));
  }
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

