#include <math.h>
#include <search.h>
#include "internoptri.h"

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

void grPrintStatistics (graphType *g)

{
  fprintf (stdout,"Graph Statistics: sites %d, edges %d, ch-edges %d,\n", 
	  NS (g), NE (g), grNCHQE (g));
  fprintf (stdout,"                  sites not in triangulation %d.\n", 
	  (NS (g) * 3 - 3 - grNCHQE (g) - NE (g)) / 3);
}

/*--------------------------------------------------------------------------*/

void *grMake (void)

{
  graphType *g;

  g = MALLOC (graphType, 1);
  g->s = NULL;
  g->e = NULL;

  return (g);
}

/*--------------------------------------------------------------------------*/

void *grNewAdvanced (

     indexType nSiteMax,
     indexType nofSites,
     int useLIA,
     int nofDeci,
     coordType *X,coordType *Y,coordType *Z,
     indexType nEdgeMax,
     indexType nofPrescribedEdges,
     indexType *O,indexType *D)

{
  graphType * tmp;

  tmp = grMake ();
  grSIassign (tmp, 
	      siNewAdvanced (nSiteMax, nofSites, useLIA, nofDeci, X, Y, Z));
  grQEassign (tmp, 
	      qeNewAdvanced (nEdgeMax, nofPrescribedEdges, O, D));

  return tmp;
}

/*--------------------------------------------------------------------------*/

void *grNew (

     indexType nofSites,
     int nofDeci)

{
  return 
    grNewAdvanced (nofSites, (indexType) 0, 1, nofDeci, 
		   (coordType *) NULL, (coordType *) NULL, (coordType *) NULL,
		   (indexType) (nofSites * 3 - 6), (indexType) 0,
		   (indexType *) NULL, (indexType *) NULL);
}

/*--------------------------------------------------------------------------*/

void grDispose (graphType *g)

{
  if (NS (g)) FREE (g->s);
  if (NE (g)) FREE (g->e);
}

/*--------------------------------------------------------------------------*/

void grQEassign (graphType *g, void *q)

{
  qeIsQEtype (q);
  g->e = q;
}

/*--------------------------------------------------------------------------*/

void *grQE (graphType *g)

{
  qeIsQEtype (g->e);
  return (void *) g->e;
}

/*--------------------------------------------------------------------------*/

void grSIassign (graphType *g, void *s)

{
  siIsSItype (s);
  g->s = s;
}

/*--------------------------------------------------------------------------*/

void *grSI (graphType *g)

{
  siIsSItype (g->s);
  return (void *) g->s;
}

/*--------------------------------------------------------------------------*/

void grAllocateSites (

     graphType *g,
     indexType maxs,
     int nofDeci)

{
  grSIassign (g, siNew (maxs, nofDeci));
}

/*--------------------------------------------------------------------------*/

void grAllocateEdges (

     graphType *g,
     indexType maxe)

{
  grQEassign (g, qeNew (maxe));
}

/*--------------------------------------------------------------------------*/

void grAllocate (graphType *g, int maxs, int maxe, int nofDeci)

{
  if (maxs > 0)
    grAllocateSites (g, maxs, nofDeci);
  grAllocateEdges (g, maxe);
}

/*--------------------------------------------------------------------------*/

static int * inci;

/*--------------------------------------------------------------------------*/

static indexType grFindPrevEdge (
     
     graphType *g,
     int org,int dst)
     
/* returns the quadedge qe for which ORG (qe) == org and for which the */
/* "virtual" quadedge from org to dst lies between qe and ONEXT (qe). */
/* if the quadedge from org to dst exists, an error is reported. if */
/* org is not incident to any edge, an error is reported. */

{
  indexType qe, qeTop;
  indexType dstTop;

  qe = qeTop = inci[org];
  
  if (qeTop < 0) {
    fprintf (stdout,"ERROR: grFindPrevEdge: no edge incident to vertex.\n");
    exit (1);
  }
  
  dstTop = DST (g, qeTop);

  if (sosrLeftTurn (g, org, dstTop, dst)) {

    do {
      if ((qeTop != ONEXT (g, qe)) &&
	  sosrLeftTurn (g, org, dstTop, DST (g, ONEXT (g, qe))))
	qe = ONEXT (g, qe);
      else 
	return qe;
    } while (sosrLeftTurn (g, org, DST (g, qe), dst));

    return OPREV (g, qe);

  } else {

    do {
      if ((qeTop != OPREV (g, qe)) &&
          (! sosrLeftTurn (g, org, dstTop, DST (g, OPREV (g, qe)))))
        qe = OPREV (g, qe);
      else 
	return OPREV (g, qe);
    } while (! sosrLeftTurn (g, org, DST (g, qe), dst));

    return qe;

  }
}

/*--------------------------------------------------------------------------*/

static indexType grLoadEdge (

     graphType *g,
     int org, int dst)

{
  static int firstTime = 1;
  indexType newqe, i;

  if (firstTime) {
    firstTime = 0;
    for (i = 0; i < NS (g); i++)
      inci [i] = -1;
  }

  if (inci[org] < 0) 
    if (inci[dst] < 0) {
      newqe = grAddSiteSite (g, org, dst);
      inci[org] = newqe;
      inci[dst] = SYM (newqe);
    } else 
      inci[org] = newqe = 
	SYM (grAddEdgeSite (g, SYM (grFindPrevEdge (g, dst, org)), org));
  else
    if (inci[dst] < 0) {
      newqe = grAddEdgeSite (g, SYM (grFindPrevEdge (g, org, dst)), dst);
      inci[dst] = SYM (newqe);
    } else 
      newqe = grAddEdgeEdge (g,
			     SYM (grFindPrevEdge (g, org, dst)),
			     grFindPrevEdge (g, dst, org));

  grUNSETCONSTRedge (g, grQETOE (newqe));
  return newqe;
}

/*--------------------------------------------------------------------------*/

static indexType grLoadConstrainedEdge (

     graphType *g,
     int org,int dst)

{
  indexType newEdge;

  newEdge = grLoadEdge (g, org, dst);
  grSETCONSTRedge (g, grQETOE (grLoadEdge (g, org, dst)));

  return newEdge;
}

/*--------------------------------------------------------------------------*/

static indexType grFindCCWCHQE (graphType *g)

{
  int foundEdge;
  indexType qe, symQe, edge;

  for (foundEdge = 0, edge = 0; ((! foundEdge) && (edge < NE (g))); edge++) {
    symQe = SYM (qe = grMAKEQE (edge));
    foundEdge = ((DST (g, ONEXT (g, qe)) != DST (g, OPREV (g, symQe))) ||
		 (DST (g, OPREV (g, qe)) != DST (g, ONEXT (g, symQe))));
  }

  for (edge = 0; ((! foundEdge) && (edge < NE (g))); edge++) {
    symQe = SYM (qe = grMAKEQE (edge));
    foundEdge = (sosrLeftTurn (g, ORG (g, qe), 
			   DST (g, qe), DST (g, ONEXT (g, qe))) == 
		 sosrLeftTurn (g, ORG (g, qe), 
			   DST (g, qe), DST (g, OPREV (g, qe))));
  }
  
  if ((! foundEdge) && (NE (g) != 3)) {
    printf 
      ("ERROR: grFindCCWCHQE: graph is not the triangulation of a disk.\n");
    exit (1);
  }
  
  if (DST (g, ONEXT (g, qe)) == DST (g, OPREV (g, symQe)))
    if (DST (g, OPREV (g, qe)) == DST (g, ONEXT (g, symQe))) {
      qe = ONEXT (g, qe); symQe = SYM (qe);
      if (sosrLeftTurn (g, ORG (g, qe),
		    DST (g, qe), DST (g, ONEXT (g, symQe))))
	return qe;
      else 
	return OPREV (g, qe);
    } else
      return qe;
  else
    return symQe;
}  

/*--------------------------------------------------------------------------*/

void grResetCHstatus (graphType *g)

{
  int i;

  for (i = 0; i < NE (g); i++) {
    grUNSETCHedge (g, i);
  }
}

/*--------------------------------------------------------------------------*/

void grSetCHstatus (graphType *g)

{
  int chQe;

  grResetCHstatus (g);

  chQe = grCHQE (g);

  if (chQe < 0)
    grSETchQE (g, chQe = grFindCCWCHQE (g));

  grSETnChQE (g, 0);

  do {
    grSETnChQE (g, grNCHQE(g) + 1);
    grSETCHedge (g, grQETOE (chQe));
    chQe = ONEXT (g, SYM (chQe));
  } while (chQe != grCHQE (g));
}

/*--------------------------------------------------------------------------*/

void grCleanConvexHull (void *g)

{
  indexType first;
  indexType qe, prev;
  void *q;
  int count;
  double dummy;

  q = grQE (g);
  qeResetDELETEDstatus (q);
  count = 0;

  prev = qeSYM (qeCHQE (q));
  qe = qeONEXT (q, prev);
  first = qeORG (q, prev);
  do {
    while ((sosrCollinear (g, qeORG (q, qe), qeDST (q, qe),
			   qeDST (q, qeONEXT (q, qe)))) &&
           (sosrLexoCompare (q, qeORG (q, qe), qeDST (q, qeONEXT (q, qe)),&dummy) !=
            sosrLexoCompare (q, qeDST (q, qe), qeDST (q, qeONEXT (q, qe)),&dummy))) {
      qeSETCHedge (q, qeQETOE (qeONEXT (q, qe)));
      qeSETCHedge (q, qeQETOE (qeONEXT (q, qeSYM (qeONEXT (q, qe)))));
      qeUNSETCHedge (q, qeQETOE (qe));
      qeSETDELETEDedge (q, qeQETOE (qe));
      qeUnlink (q, qe);
      qeSETnChQE (q, qeNCHQE (q) + 1);
      qe = qeONEXT (q, prev);
      count++;
    }
    prev = qeSYM (qeONEXT (q, prev));
    qe = qeONEXT (q, prev);
  } while (qeORG (q, prev) != first);
  
  qeDeleteMarkedEdges (q);

/*  fprintf (stdout,"grCleanConvexHull: %d triangles removed\n", count);*/
}

/*--------------------------------------------------------------------------*/

static int grEdgesOverlap (

     void *g,void *q,
     indexType from1,indexType to1,indexType from2,indexType to2)

{
  double dummy;

  if (sosrSlopeCompare (g, from1, to1, from2, to2)) return 0;
  if (sosrIdenticalXY (g, from1, from2)) return 1;

  if (sosrIdenticalX (g, from1, to1))
    return (sosrIdenticalX (g, from1, from2) &&
	    (! sosrIdenticalY (g, to1, from2)) &&
	    sosrBelow (g, from2, to1, &dummy));
  else
    return ((! sosrIdenticalX (g, from1, from2)) &&
	    (! sosrIdenticalX (g, to1, from2)) &&
	    sosrLeftOf (g, from2, to1, &dummy) &&
	    sosrCollinear (g, from1, to1, from2));
}

/*--------------------------------------------------------------------------*/

typedef struct { indexType from, to; } fromToPairType;
void *gOverlap;
  
/*--------------------------------------------------------------------------*/

static int grOverlapEdgesGT (void *e1, void *e2)

{
#define E1 ((fromToPairType *) e1)
#define E2 ((fromToPairType *) e2)

  int gt;
  double dummy;

  if (gt = sosrSlopeCompare (gOverlap, E1->from, E1->to, E2->from, E2->to))
    return gt;
  else
    if (! sosrCollinear (gOverlap, E1->from, E1->to, E2->from))
      return (sosrLeftTurn (gOverlap, 
			    E1->from, E1->to, E2->from) ? 1 : -1);
    else
      return sosrLexoCompare (gOverlap, E1->from, E2->from, &dummy);

#undef E1
#undef E2
}

/*--------------------------------------------------------------------------*/

static int grOverlapVerticesGT (void *s1, void *s2)

{
  double dummy;

  return (sosrLexoCompare (gOverlap, *((indexType *) s1), 
		                     *((indexType *) s2), &dummy));
}

/*--------------------------------------------------------------------------*/

static indexType maxRight, first;
static indexType grOverlapDummy;

static int grOverlapBScompare (void *s1, void *s2)

{
  double dummy;
  int c1, c2;

  if (s2 == &grOverlapDummy) 
    return - grOverlapBScompare (s2, s1);

  if ((! (c1 = 
	  sosrLexoCompare (gOverlap, *((indexType *) s2), first, &dummy))) ||
      (! (c2 = 
	  sosrLexoCompare (gOverlap, *((indexType *) s2), maxRight, &dummy))))
    return 0;
  else
    if (c1 == -1) return 1;
    else 
      if (c2 == 1) return -1; 
      else return 0;
}

/*--------------------------------------------------------------------------*/

void grMakeNonDegenerate (void *g)

{
  indexType i, qe, nofEdges, vListIndex, lastV, nofSites;
  indexType *vp, *vpSave;
  void *q, *s;
  double dummy;
  fromToPairType *fromTo;
  indexType *vList, *freeV;

  gOverlap = g;
  q = grQE (g);
  s = grSI (g);

  if (! qeNE (q))
    return;

  freeV = MALLOC (indexType, siNS (s));
  for (i = 0; i < siNS (s); i++)
    freeV [i] = i;
  qsort (freeV, siNS (s), sizeof (indexType), grOverlapVerticesGT);
  nofSites = siNS (s);

  fromTo = MALLOC (fromToPairType, qeNE (q));
  nofEdges = 0;
  for (i = 0; i < qeNE (q); i++) {
    qe = qeMAKEQE (i);
    if (! ((qeORG (q, qe) == qeDST (q, qe)) ||
	   sosrIdenticalXY (g, qeORG (q, qe), qeDST (q, qe)))) 
      if (sosrLexoCompare (g, qeORG (q, qe), qeDST (q, qe), &dummy) == -1) {
	fromTo [nofEdges].from = qeORG (q, qe);
	fromTo [nofEdges++].to = qeDST (q, qe);
      } else {
	fromTo [nofEdges].from = qeDST (q, qe);
	fromTo [nofEdges++].to = qeORG (q, qe);
      }
  }

  qsort (fromTo, nofEdges, sizeof (fromToPairType), grOverlapEdgesGT);

  i = 0;
  vList = MALLOC (indexType, nofEdges * 2 + nofSites);
  qeNEreset (q);

  while (i < nofEdges) {

    first = vList [0] = fromTo [i].from;
    maxRight = vList [1] = fromTo [i].to;
    vListIndex = 2;
    i++;
    
    while ((i < nofEdges) && 
	   grEdgesOverlap (g, q, first, maxRight, 
  			                fromTo [i].from, fromTo [i].to)) {

      vList [vListIndex++] = fromTo [i].from;
      vList [vListIndex++] = fromTo [i].to;
      if (sosrLexoCompare (g, maxRight, fromTo [i].to, &dummy) == -1)
      maxRight = fromTo [i].to;
		  
      i++;
    }

    if ((vp = (indexType *)
	 bsearch (&grOverlapDummy, freeV, nofSites, sizeof (indexType), 
		  grOverlapBScompare))
	!= NULL) {

      vpSave = vp;

      while (*vp != first) {
	if (sosrCollinear (g, first, maxRight, *vp))
	  vList [vListIndex++] = *vp;
	vp--;
      }

      vp = vpSave;
      
      while (*vp != maxRight) {
	if (sosrCollinear (g, first, maxRight, *vp))
	  vList [vListIndex++] = *vp;
	vp++;
      }
    }
    
    qsort (vList, vListIndex, sizeof (indexType), grOverlapVerticesGT);
    
    lastV = 1;
    while (lastV < vListIndex) {
      if ((vList [lastV] != vList [lastV - 1]) &&
	  (! sosrIdenticalXY (g, vList [lastV], vList [lastV - 1]))) {
        qe = qeAddSiteSite (q, vList [lastV - 1], vList [lastV]);
        qeSETCONSTRedge (q, qeQETOE (qe));
      }
      lastV++;
    }

  }
  
  FREE (freeV);
  FREE (fromTo);
  FREE (vList);
}

/*--------------------------------------------------------------------------*/

static void * gDuplicate;

static int grDuplicateGT (void *s1, void *s2)

{
  double dummy;

  if (sosrIdenticalXY (gDuplicate, *((indexType *) s1), *((indexType *) s2)))
    if (siIsDuplicateSite (grSI (gDuplicate), (int) (*((indexType *) s1))))
      return 1;
    else
      if (siIsDuplicateSite (grSI (gDuplicate), (int) (*((indexType *) s2))))
	return -1;
      else
	return 0;
  else
    return (sosrLeftOf (gDuplicate, *((indexType *) s1), *((indexType *) s2),
			&dummy) ? -1 : 1);
}
    
/*--------------------------------------------------------------------------*/

void grMarkDuplicateSites (void *g)

{
  void *s, *q;
  indexType i, original, qe;
  indexType * Index, * vCrossRef;

  s = grSI (g);
  q = grQE (g);
  gDuplicate = g;

  if (! siMarkDuplicateSites (s)) 
    return;

  Index = MALLOC (indexType, siNS (s));

  for (i = 0; i < siNS (s); i++) 
    Index [i] = i;
  qsort (Index, siNS (s), sizeof (indexType), grDuplicateGT);

  vCrossRef = MALLOC (indexType, siNS (s));

  for (i = 0; i < siNS (s); i++) {
    if (siIsDuplicateSite (s, Index[i]))
      if (sosrIdenticalXY (g, original, Index [i]))
	vCrossRef [Index[i]] = original;
      else
	fprintf (stdout,"ERROR\n");
    else
      original = vCrossRef [Index [i]] = Index [i];
  }

  FREE (Index);

  for (i = 0; i < qeNE (q); i++) {
    qe = qeMAKEQE (i);
    qeSETORG (q, qe, vCrossRef [qeORG (q, qe)]);
    qeSETDST (q, qe, vCrossRef [qeDST (q, qe)]);
  }

  FREE (vCrossRef);
}
  
/*--------------------------------------------------------------------------*/

void grRemoveDuplicateSites (void *g)

{
  void *s, *q;
  indexType * Index;
  indexType i, qe, sOld;

  s = grSI (g);
  q = grQE (g);

  grMarkDuplicateSites (g);

  sOld = siNS (s);

  Index = MALLOC (indexType, siNS (s));

  siRemoveDuplicateSitesExpert (s, Index);

  for (i = 0; i < qeNE (q); i++) {
    qe = qeMAKEQE (i);
    qeSETORG (q, qe, Index [qeORG (q, qe)]);
    qeSETDST (q, qe, Index [qeDST (q, qe)]);
  }
  
  FREE (Index);
}
  
/*--------------------------------------------------------------------------*/

void grReadGraph (char *fname, graphType *g)

{
  indexType ns, ne;
  int maxW, maxA;

  siExamineSites (fname, &ns, &maxW, &maxA);
  qeExamineEdges (fname, &ne);

  if (ns == 0) {
    fprintf (stdout, "readGraph: Warning: no sites read in.\n");
    return;
  }

  if (maxW >= 16) {
    fprintf (stdout,"ERROR: readGraph: numbers too large.\n");
    exit (1);
  }

  grAllocate (g, ns, ns * 3 - 6, (maxW + maxA >= 16 ? 15 - maxW : maxA));

  siReadSites (fname, g->s);
  qeReadEdges (fname, g->e);

  NEset (g, ne);

  grResetALLstati (g);
/*  if (NE (g) >= 3)
    grSetCHstatus (g);*/

  /* fprintf (stdout,"readGraph: vertices (sites) read: %d\n", ns); */
}

/*--------------------------------------------------------------------------*/

void grPrintGraph (char *fname, graphType *g,
  int nofFlips, int nofSuccFlips, double runTime)
{
  FILE *fp;

  if (! strcmp (fname, ""))
    fp = stdout;
  else
    if ((fp = fopen (fname, "w")) == NULL) { (void)
      fprintf (stdout, "writeGraph: can't open \"%s\" to write.\n", fname);
      return;
    }

  siPrintSites (g->s, fp);
  qePrintEdges (g->e, fp);

  if (fclose (fp) == EOF) (void)
    fprintf (stdout, "writeGraph: could not close file \"%s\".\n", fname);
  
  /* write out a time stamp */
  fprintf (stdout,"\nTriangulation written to file \"%s\" %s", fname,
		".\n\n" /* ctime (&clock) */);
}

/*--------------------------------------------------------------------------*/

void grPrintGraphLong (char *fname, graphType *g,
  int nofFlips, int nofSuccFlips, double runTime)
{
  FILE *fp;

  if (! strcmp (fname, ""))
    fp = stdout;
  else
    if ((fp = fopen (fname, "w")) == NULL) { (void)
      fprintf (stdout, "writeGraph: can't open \"%s\" to write.\n", fname);
      return;
    }

  siPrintSites (g->s, fp);
  qePrintQEdges (g->e, fp);

  if (fclose (fp) == EOF) (void)
    fprintf (stdout, "writeGraph: could not close file \"%s\".\n", fname);
  
  /* write out a time stamp */
  fprintf (stdout,"\nTriangulation written to file \"%s\" %s", fname,
	 ".\n\n" /* ctime (&clock) */);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static int nofFlips, nofAttempts;
static double runTime;
static visualType *visual;
#define DONTPRINTINFO 0

/*--------------------------------------------------------------------------*/

void grMinMaxAngleTriangulation (void *g)

{
  visual = dummyVisual ();
  buildMinMaxAngleTriangulation (g, DONTPRINTINFO, visual, &nofFlips,
				 &nofAttempts, &runTime);  
  FREE (visual);
}

/*--------------------------------------------------------------------------*/

void grMinMaxAngleTriangulation2 (void *g)

{
  visual = dummyVisual ();
  buildMinMaxAngleTriangulation2 (g, DONTPRINTINFO, visual, &nofFlips,
				  &nofAttempts, &runTime);
  FREE (visual);
}

/*--------------------------------------------------------------------------*/

void grMaxMinHeightTriangulation (void *g)

{
  visual = dummyVisual ();
  buildMaxMinHeightTriangulation (g, DONTPRINTINFO, visual, &nofFlips,
				  &nofAttempts, &runTime);
  FREE (visual);
}

/*--------------------------------------------------------------------------*/

void grMaxMinHeightTriangulation2 (void *g)

{
  visual = dummyVisual ();
  buildMaxMinHeightTriangulation2 (g, DONTPRINTINFO, visual, &nofFlips,
				   &nofAttempts, &runTime);
  FREE (visual);
}

/*--------------------------------------------------------------------------*/

void grMinMaxSlopeTriangulation (void *g)

{
  visual = dummyVisual ();
  buildMinMaxSlopeTriangulation (g, DONTPRINTINFO, visual, &nofFlips,
				 &nofAttempts, &runTime);
  FREE (visual);
}

/*--------------------------------------------------------------------------*/

void grDelaunayTriangulation (void *g)
     
{
  visual = dummyVisual ();
  buildDelaunayTriangulation
    (g, DONTPRINTINFO, visual, &nofFlips, &nofAttempts, &runTime);
  FREE (visual);
}

/*--------------------------------------------------------------------------*/

void grIncrementalDelaunayTriangulation (void *g)

{
  visual = dummyVisual ();
  incrementalDelaunayTriangulation
    (g, DONTPRINTINFO, visual, &nofFlips, &nofAttempts, &runTime);
  FREE (visual);
}

/*--------------------------------------------------------------------------*/

void grIncrementalRegularTriangulation (void *g)

{
  visual = dummyVisual ();
  regularTriangulation
        (g, DONTPRINTINFO, visual, &nofFlips, &nofAttempts, &runTime);
  FREE (visual);
}

/*--------------------------------------------------------------------------*/

void grPlanesweepTriangulation (void *g)

{
  visual = dummyVisual ();
  buildInitialTriangulation (g, DONTPRINTINFO, visual, &runTime);
  FREE (visual);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

