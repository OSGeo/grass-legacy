#include "internoptri.h"
#include "persquadedge.h"
#include <sys/time.h>

/*--------------------------------------------------------------------------*/

static int returnFalse (void)

{
  return 0;
}

/*--------------------------------------------------------------------------*/

static queuesType *
initializePool (void *h, poolType *pool, int (*GTfunction)(void))
{
  return (*pool->create) (3 * siNUS (hdagSI (h)), (void *) NULL, 
			  GTfunction, POOL_ADJUSTABLE_SIZE);
}

/*--------------------------------------------------------------------------*/

static indexType *makeRandomArray (void *s)
{
  indexType * arr;
  indexType i, j, tmp;
  struct timeval tp;
  struct timezone tzp;

  gettimeofday(&tp, &tzp);
  srandom(tp.tv_usec % (256*256));

  arr = MALLOC (indexType, siNUS (s));

  for (j = 0, i = 0; i < siNS (s); i++)
    if (! siIsDuplicateSite (s, i))
      arr[j++] = i;

  for (i = 0; i < siNUS (s); i++) {
    tmp = arr[i];
    j = /* random () % siNUS (s);*/ i;
    if ((j < 0) || (j >= siNUS (s))) {
      fprintf (stdout,"ERROR: randArray.\n");
      exit (1);
    }
    arr [i] = arr [j];
    arr [j] = tmp;
  }

  return arr;
}

/*--------------------------------------------------------------------------*/

void insertNextPoint (

     void *h,
     visualType *visual,
     indexType v,
     poolType   *pool,
     queuesType *edges,
     int *spl)

{
  indexType qe, qeSymNextSym, qeSymPrevSym, tmp;
  int count;
  void *p;

  qe = hdagInsertPoint (h, v, spl);
  p = hdagPQ (h);

  if (qe != (indexType)NULL) {
    qeSymNextSym = pSYM (pONEXT (p, pSYM(qe), pTimeNow (p)));
    qeSymPrevSym = pSYM (pOPREV (p, pSYM(qe), pTimeNow (p)));

    (*visual->drawEdge) (p, visual, qe, 1);
INTERRUPT_ALGORITHM;
    (*visual->drawEdge) (p, visual, qeSymNextSym, 1);
INTERRUPT_ALGORITHM;
    (*visual->drawEdge) (p, visual, qeSymPrevSym, 1);
INTERRUPT_ALGORITHM;

/*    fprintf (stdout,"insertNextPoint: %d %d %d\n", pORG (p, qe), pORG (p, qeSymNextSym), pORG (p, qeSymPrevSym));*/
    
    count = 0;
    if (pORG (p, qe) >= 0)
      count ++;
    if (pORG (p, qeSymNextSym) >= 0)
      count ++;
    if (pORG (p, qeSymPrevSym) >= 0)
      count ++;

    if (count == 1)
      if (pORG (p, qeSymNextSym) >= 0) {
	tmp = qe; qe = qeSymNextSym; qeSymNextSym = qeSymPrevSym;
	qeSymPrevSym = tmp;
      } else
	if (pORG (p, qeSymPrevSym) >= 0) {
	  tmp = qe; qe = qeSymPrevSym; qeSymPrevSym = qeSymNextSym; 
	  qeSymNextSym = tmp;
	}

    (* pool->insert) (edges, pQETOE (pONEXT(p, qe, pTimeNow (p))));
    (* pool->insert) (edges, pQETOE (pONEXT(p, qeSymNextSym, pTimeNow (p))));
    (* pool->insert) (edges, pQETOE (pONEXT(p, qeSymPrevSym, pTimeNow (p))));
  } else {
/*    fprintf (stdout,"insertNextPoint: %d not inserted\n", p);*/
  }
INTERRUPT_LABEL: ;
}

/*--------------------------------------------------------------------------*/

static void do22flip (

     void *p,
     visualType *visual,
     poolType   *pool,
     queuesType *edges,
     indexType qe,
     indexType vertex)

{
  indexType flipQe, edge;

  (*visual->eraseEdge) (p, visual, qe);
INTERRUPT_ALGORITHM;  
  flipQe = pqeFlip (p, qe);
  (*visual->drawEdge) (p, visual, flipQe, 1);
INTERRUPT_ALGORITHM;

  if (((pDST (p, flipQe) != vertex) && (pORG (p, flipQe) != vertex)))
    fprintf (stdout,"ERROR\n");

  /*    fprintf (stdout,"b %d %d %d %d %d\n", flipQe, pONEXT(p, pSYM(flipQe), pTimeNow (p)),        pOPREV(p, pSYM(flipQe), pTimeNow (p)), pONEXT(p, flipQe, pTimeNow (p)),
        pOPREV(p, flipQe, pTimeNow (p)));*/
  
  if (pORG (p, flipQe) == vertex) 
    flipQe = pSYM (flipQe);

  edge = pQETOE(pONEXT(p, flipQe, pTimeNow (p)));
  
  if (! (*pool->doesContain) (edges, edge)) {
    (* pool->insert) (edges, edge);
  }

  edge = pQETOE(pOPREV(p, flipQe, pTimeNow (p)));
  
  if (! ((pISCHedge (p, edge, pTimeNow (p))) ||
	 ((*pool->doesContain) (edges, edge)))) {
    (* pool->insert) (edges, edge);
  }
INTERRUPT_LABEL: ;
}

/*--------------------------------------------------------------------------*/

static int do31flip (

     void *p,
     visualType *visual,
     poolType   *pool,
     queuesType *edges,
     indexType flipQe,
     indexType vertex)

{
  indexType edge;
  int tmp, success;

/*  fprintf (stdout,"ERROR: halli hallo\n");*/

  if (pDST (p, pOPREV(p, pSYM (flipQe), pTimeNow (p))) ==
    pDST (p, pOPREV(p, pSYM (pONEXT(p, pSYM (flipQe), pTimeNow (p))), pTimeNow (p)))) {

    (*visual->eraseEdge) (p, visual, flipQe);
INTERRUPT_ALGORITHM;
    (*visual->eraseEdge) (p, visual, pOPREV(p, pSYM(flipQe), pTimeNow (p)));
INTERRUPT_ALGORITHM;
    (*visual->eraseEdge) (p, visual, pONEXT(p, pSYM(flipQe), pTimeNow (p)));
INTERRUPT_ALGORITHM;

    tmp = (pDST (p, pONEXT (p, flipQe, pTimeNow (p))) == vertex);

    if (tmp)
      edge = pQETOE (pOPREV (p, flipQe, pTimeNow (p)));
    else
      edge = pQETOE (pONEXT (p, flipQe, pTimeNow (p)));

    if (! (*pool->doesContain) (edges, edge)) {
      (*pool->insert) (edges, edge);
    }

    if (tmp)
      edge = pQETOE (pONEXT (p, pSYM(flipQe), pTimeNow (p)));
    else
      edge = pQETOE (pOPREV (p, pSYM(flipQe), pTimeNow (p)));

    if ((*pool->doesContain) (edges, edge)) {
      (*pool->delete) (edges, edge);
    }

    pqeDeleteTriangleStar (p, pOPREV(p, flipQe, pTimeNow (p)));

    success = 1;
  } else { /* can't flip */
    if (pDST (p, pONEXT (p, flipQe, pTimeNow (p))) == vertex)
      edge = pQETOE (pONEXT (p, pONEXT (p, pSYM(flipQe), pTimeNow (p)), pTimeNow (p)));
    else
      edge = pQETOE (pOPREV (p, pOPREV (p, pSYM(flipQe), pTimeNow (p)), pTimeNow (p)));

    if (! (*pool->doesContain) (edges, edge)) {
      (*pool->insert) (edges, edge);
    }
    success = 0;
  }

INTERRUPT_LABEL: ;
  return success;
}

/*--------------------------------------------------------------------------*/

static int doFlipEdge (

     void *h,
     visualType *visual,
     poolType   *pool,
     queuesType *edges,
     indexType qe,
     indexType vertex)

{
  double dummy;
  int success;
  void *p;

  p = hdagPQ (h);

  if (sosrLeftTurnInf (h, pDST (p, pONEXT(p, qe, pTimeNow (p))), pORG (p, qe),
		   pDST (p, pOPREV(p, qe, pTimeNow (p))), &dummy))
    if (sosrLeftTurnInf (h, pDST (p, pOPREV(p, qe, pTimeNow (p))), pDST (p, qe),
		     pDST (p, pONEXT(p, qe, pTimeNow (p))), &dummy)) {

      do22flip (hdagPQ (h), visual, pool, edges, qe, vertex);
      success = 1;

    } else {
      success = do31flip (hdagPQ (h), visual, pool, edges, qe, vertex);

  } else {

    success = do31flip (hdagPQ (h), visual, pool, edges, pSYM (qe), vertex);
  }
  return success;
}

/*--------------------------------------------------------------------------*/

static int inCircInf (

     void *h,
     indexType qe)

{
  double dummy;
  void *p;

  p = hdagPQ (h);

  if ((pDST(p, qe) < 0) && (pORG(p, qe) < 0))
    return 0;

  return (int) sosrInCircleInf (p, pDST(p, qe), 
				pDST(p, pONEXT(p, qe, pTimeNow (p))), 
				pORG(p, qe), 
				pDST(p, pONEXT(p, pSYM(qe), pTimeNow (p))), 
				1,
				&dummy);
}

/*--------------------------------------------------------------------------*/

static int regularityTestInf (

     void *h,
     indexType qe)

{
  double dummy;
  void *p;

  p = hdagPQ (h);

  if ((pDST(p, qe) < 0) && (pORG(p, qe) < 0))
    return 0;

  return (int) sosrInCircleInf (p, pDST(p, qe), 
				pDST(p, pONEXT(p, qe, pTimeNow (p))), 
				pORG(p, qe), 
				pDST(p, pONEXT(p, pSYM(qe), pTimeNow (p))), 
				0,
				&dummy);
}

/*--------------------------------------------------------------------------*/

static int return0 (

     void *h,
     indexType s0,indexType s1,indexType s2,indexType s3)

{
  return (int) 0;
}

/*--------------------------------------------------------------------------*/

static int inCircVerticesInf (

     void *h,
     indexType s0,indexType s1,indexType s2,indexType s3)

{
  double dummy;

  if ((s0 < 0) && (s2 < 0))
    return 0;

  return (int) sosrInCircleInf (h, s0, s1, s2, s3, 1, &dummy);
}

/*--------------------------------------------------------------------------*/

static int regularityTestVerticesInf (

     void *h,
     indexType s0,indexType s1,indexType s2,indexType s3)

{
  double dummy;

  if ((s0 < 0) && (s2 < 0))
    return 0;

  return (int) sosrInCircleInf (h, s0, s1, s2, s3, 0, &dummy);
}

/*--------------------------------------------------------------------------*/

static indexType *initialEdgeP (graphType *g)
{
  indexType * tmp;
  indexType vertex;

  tmp = MALLOC (indexType, NUS (g));

  for (vertex = 0; vertex < NUS (g); vertex++) 
    tmp [vertex] = -1;

  return tmp;
}

/*--------------------------------------------------------------------------*/

void verifyRegular (graphType *g, int doDelaunay)

{
  int (*testRegular)();
  indexType edge, qe, vertex, qeSym;
  int failCount;
  char fname[50];
  FILE *fp, *fopen();
  indexType *edgeP; /* pointers from vertices to one edge incident to */
  /* the vertex */

  fprintf (stdout,"testing %s: ", (doDelaunay ? "delaunayness" : "regularity"));
  (void) fflush(stdout);

  if ((fp = fopen ((doDelaunay ? "delaunay.verify" : "regular.verify"), "w")) == NULL) {
    (void) fprintf (stdout,"verifyRegular:  can't open \"%s\" to write.\n", fname);
    return;
  }

  if (doDelaunay == 1)
    testRegular = inCircVerticesInf;
  else
    testRegular = regularityTestVerticesInf;

  failCount = 0;

  for (edge = 0; edge < NE (g); edge++) {
    qe = grMAKEQE (edge);
    qeSym = SYM (qe);
    
    if (! grISCHedge(g, edge)) {
      if (ONEXT (g, qe) != SYM (OPREV (g, SYM (OPREV (g, SYM (qe)))))) {
	fprintf (fp, "not triangle %d\n", qe);
	failCount++;
      }
      if (ONEXT (g, qeSym) != SYM (OPREV (g, SYM (OPREV (g, SYM (qeSym)))))) {
	fprintf (fp, "not triangle %d\n", qeSym);
	failCount++;
      }
    } else 
      if ((ONEXT (g, qe) != SYM (OPREV (g, SYM (OPREV (g, SYM (qe)))))) &&
	  (ONEXT (g, qeSym) != SYM (OPREV (g, SYM (OPREV (g, SYM (qeSym))))))){
	fprintf (fp, "not triangle ch edge %d\n", qe);
	failCount++;
      }
  }

  if (! failCount)
    fprintf (stdout,"\nTriangulation test: Graph is a triangulation.");

  for (edge = 0; edge < NE (g); edge++) {
    qe = grMAKEQE (edge);
    qeSym = SYM (qe);
    vertex = DST(g, OPREV(g, qe));
    if ((vertex != ORG(g, qe)) && (vertex != DST(g, qe)) &&
	(vertex != DST(g, ONEXT(g, qe))))
      if (ONEXT (g, qe) == SYM (OPREV (g, SYM (OPREV (g, SYM (qe))))))
	if (! (*testRegular) (g, ORG(g, qe), DST(g, ONEXT(g, qe)),
			      DST(g, qe), vertex)) {
	  fprintf (fp, "not %s edge (%d, %d), %d %d\n",
		   (doDelaunay ? "delaunay" : "regular"), 
		   ORG(g, qe), DST(g, qe), DST(g, ONEXT(g, qe)),
		   vertex);
	  failCount++;
	}
    
    vertex = DST(g, ONEXT(g, qe));
    if ((vertex != ORG(g, qe)) && (vertex != DST(g, qe)) &&
	(vertex != DST(g, OPREV(g, qe))))
      if (ONEXT (g, qeSym) == SYM (OPREV (g, SYM (OPREV (g, SYM (qeSym))))))
	if (! (*testRegular) (g, ORG(g, qe), vertex, DST(g, qe),
			      DST(g, OPREV(g, qe)))) {
	  fprintf (fp, "not %s edge (%d, %d), %d %d\n",
		   (doDelaunay ? "delaunay" : "regular"), 
		   ORG(g, qe), DST(g, qe), DST(g, OPREV(g, qe)),
		   vertex);
	  failCount++;
	}
  }

  if (! failCount)
    fprintf (stdout,"\nAll edges are locally %s.", (doDelaunay ? "delaunay" : "regular"));

  edgeP = initialEdgeP (g);

  for (edge = 0; edge < NE (g); edge++) {
    qe = grMAKEQE (edge);
    qeSym = SYM (qe);
    
    edgeP [ORG(g, qe)] = qe;
    edgeP [ORG(g, qeSym)] = qeSym;
  }

  for (vertex = 0; vertex < NUS (g); vertex++) 
    if (edgeP [vertex] == -1) 
      for (edge = 0; edge < NE (g); edge++) {
	qe = grMAKEQE (edge);
	qeSym = SYM (qe);
	if ((vertex != ORG(g, qe)) && (vertex != DST(g, qe)) &&
	    (vertex != DST(g, ONEXT(g, qe))))
	  if (ONEXT (g, qe) == SYM (OPREV (g, SYM (OPREV (g, SYM (qe))))))
	    if (! (*testRegular) (g, ORG(g, qe), DST(g, ONEXT(g, qe)),
				  DST(g, qe), vertex)) {
	      fprintf (fp, "not %s edge (%d, %d), %d %d\n",
		       (doDelaunay ? "delaunay" : "regular"), 
		       ORG(g, qe), DST(g, qe), DST(g, ONEXT(g, qe)),
		       vertex);
	      failCount++;
	    }
	
	if ((vertex != ORG(g, qe)) && (vertex != DST(g, qe)) &&
	    (vertex != DST(g, OPREV(g, qe))))
	  if (ONEXT (g, qeSym) == SYM (OPREV (g, SYM (OPREV (g, SYM (qeSym))))))
	    if (! (*testRegular) (g, ORG(g, qe), vertex, DST(g, qe),
				  DST(g, OPREV(g, qe)))) {
	      fprintf (fp, "not %s edge (%d, %d), %d %d\n",
		       (doDelaunay ? "delaunay" : "regular"), 
		       ORG(g, qe), DST(g, qe), DST(g, OPREV(g, qe)),
		       vertex);
	      failCount++;
	    }
      }

  FREE (edgeP);

  if (! failCount) {
    fprintf (fp, "\ntriangulation is globally %s\n", 
	    (doDelaunay ? "delaunay" : "regular"));
    fprintf (stdout,"\ntriangulation is globally %s\n", 
	    (doDelaunay ? "delaunay" : "regular"));
  } else {
    fprintf (fp, "\nERROR: triangulation is not globally %s\n",
	    (doDelaunay ? "delaunay" : "regular"));
    fprintf (stdout,"\n\nERROR: triangulation is not globally %s\n",
	    (doDelaunay ? "delaunay" : "regular"));
    fprintf (stdout,"       more info in file %s.\n", 
	    (doDelaunay ? "delaunay.verify" : "regular.verify"));
  }

  fclose (fp);
}

/*--------------------------------------------------------------------------*/

void incrementalFlip (graphType *gOrig, visualType *visual,
  poolType *pool, int doDelaunay, int (*GTfunction)(void),
  int *nofFlips, int *nofAttempts, double *runTime)

{
  int      foundEdge;
  indexType qe;
  queuesType *edges;
  indexType edge;
  indexType n, index;
  int spl;
  int (*testRegular)();
  int (*testRegularVertices)();
  indexType * randSites;
  void * h;

  *runTime = get_user_time ();

  if (doDelaunay == 1) {
    testRegular = inCircInf;
    testRegularVertices = inCircVerticesInf;
  } else {
    testRegular = regularityTestInf;
    testRegularVertices = regularityTestVerticesInf;
  }

  h = hdagNew ((indexType) 3 * (NUS (gOrig) + 3) - 6, grSI (gOrig),
	       sosrLeftTurnInf, testRegularVertices);

  edges = initializePool (h, pool, GTfunction);
  hdagInsertTopTriangle (h, 
			 (indexType) -1, (indexType) -2, (indexType) -3);

  *nofFlips = 0;
  *nofAttempts = 0;
  spl = 0;

  randSites = makeRandomArray (grSI (gOrig));

  for (index = 0; index < siNUS (grSI (gOrig)); index++) {

    n = randSites [index];

/*    fprintf (stdout,"%d %lf %lf\n", n, SITEX(gOrig, n), SITEY(gOrig, n));*/

    insertNextPoint (h, visual, n, pool, edges, &spl);
INTERRUPT_ALGORITHM;
    foundEdge = (*pool->top) (edges, &edge);

    while (foundEdge) {
      
      (*pool->delete) (edges, edge);
      
      qe = pMAKEQE (edge);
      
/*      fprintf (stdout,"foundEdge: %d %d %d %d %d", qe, pORG (hdagPQ (h), qe), pDST (hdagPQ (h), qe),
	     pDST(hdagPQ (h), pONEXT(hdagPQ (h), qe, pTimeNow (hdagPQ (h)))),
	     pDST(hdagPQ (h), pOPREV(hdagPQ (h), qe, pTimeNow (hdagPQ (h)))));
*/

      
      if ((*testRegular) (h, qe)) {
	
/*	fprintf (stdout,"flipped\n");*/
	
	if (doFlipEdge (h, visual, pool, edges, qe, n))
	  (*nofFlips)++;
	else
	  (*nofAttempts)++;
INTERRUPT_ALGORITHM;	
      } else {

	(*nofAttempts)++;
/*	fprintf (stdout,"not flipped\n");*/

      }
      
      foundEdge = (*pool->top) (edges, &edge);
    }
  }

INTERRUPT_LABEL: ;

  (*pool->dispose) (edges);
  copyHdagToQe (hdagPQ (h), -1, grQE (gOrig));

  fprintf (stdout,"\n");

  hdagDispose (h);
  FREE (randSites);

if (! SHOULD_INTERRUPT_ALGORITHM) 
  *runTime = get_user_time () - *runTime;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

void incrementalDelaunayTriangulation (graphType *dt, int printInfo,
  visualType *visual, int *nofFlips, int *nofAttempts, double *runTime)

{
  fprintf (stdout,"Delaunay: Incremental: "); (void) fflush(stdout);

  incrementalFlip (dt, visual, queue (), 1, (void *) NULL,
		   nofFlips, nofAttempts, runTime);

    fprintf (stdout,"completed, cpu used ... %f,\n", *runTime);
  if (printInfo) 
    fprintf (stdout,"                       # flips ... %d, #attempts %d.\n",
	    *nofFlips, *nofAttempts);
}

/*--------------------------------------------------------------------------*/
    
void regularTriangulation (graphType *dt, int printInfo, visualType *visual,
  int *nofFlips, int *nofAttempts, double *runTime)

{
  fprintf (stdout,"Regular: Incremental: "); (void) fflush(stdout);

  incrementalFlip (dt, visual, queue (), 0, (void *) NULL,
		   nofFlips, nofAttempts, runTime);
    fprintf (stdout,"completed, cpu used ... %f,\n", *runTime);
  if (printInfo) 
    fprintf (stdout,"                       # flips ... %d, #attempts %d.\n",
            *nofFlips, *nofAttempts);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
