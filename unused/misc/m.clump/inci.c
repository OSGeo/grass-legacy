#include "geom/basic.h"
#include "geom/optri.h"
#define NOT_AN_EDGE -1

/*--------------------------------------------------------------------------*/

void
inciMakeIncidences (g, loi, n)

     void *g;
     indexType **loi;
     int *n;

{
  int nofSites, nofQes, site, qe;
  void *s, *q;

  s = grSI (g);
  nofSites = siNS (s);
  q = grQE (g);
  nofQes = qeNQE (q);

  *loi = MALLOC (indexType, nofSites);

  /* foevery site find one incident (quad)edge */

  for (site = 0; site < nofSites; site++) (*loi) [site] = NOT_AN_EDGE;
  for (qe = 0; qe < nofQes; qe++) (*loi) [qeORG (q, qe)] = qe;
  *n = nofSites;
}
    
/*--------------------------------------------------------------------------*/

static int *incidences = NULL;

/* initialize incidence structure */

void
inciInitIncidences (g)

  void *g;

{
  int dummy;

  if (incidences) FREE (incidences); /* clean up */
  inciMakeIncidences (g, &incidences, &dummy); 
}

/*--------------------------------------------------------------------------*/

void
inciEnd ()

{
  FREE (incidences);
  incidences = NULL;
}

/*--------------------------------------------------------------------------*/

static indexType firstEdge, nextEdge;

/* returns in "qe" some edge incident to "site". returns 1 if such and */
/* edge exists; 0 otherwise */

int
inciGetFirstIncidence (g, site, qe)

     void *g;
     indexType site;
     indexType *qe;
     
{
  *qe = firstEdge = incidences [site]; /* start with the first edge */

  if (firstEdge != NOT_AN_EDGE) { /* is any edge incident to "site" ? */
    nextEdge = qeONEXT (grQE (g), firstEdge); /* yes */
     return 1;
  }

  return 0; /* no */
}

/*--------------------------------------------------------------------------*/

/* returns in "qe" the next edge in counterclockwise order about */
/* "site" (see inciGetFirstIncidence). returns 1 if such an edge */
/* exists; 0 if already all edges incident to "site" have been */
/* visited */

int 
inciGetNextIncidence (g, qe)

     void *g;
     indexType *qe;
     
{
  if ((firstEdge == NOT_AN_EDGE) || (nextEdge == firstEdge)) return 0;

  *qe = nextEdge;
  nextEdge = qeONEXT (grQE (g), nextEdge);
  return 1;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/


