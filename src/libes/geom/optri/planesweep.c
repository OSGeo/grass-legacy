#include "internoptri.h"
#include "stack.h"
#include "randtree.h"

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static graphType * gGlobal;
static indexType orgGlobal, dstGlobal;

static indexType *SV;
static indexType *LEA;
static void *CE;
static indexType *RM;
static indexType RMbot;
static indexType SVindex;
static indexType LEAindex;

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

#define plBSTdummyQE (NQEmax (gGlobal) + 1)

#define plBSTinit() (CE = rsNew (NQEmax (gGlobal) + 10, plAboveQE))
#define plBSTdispose() (rsDispose (CE))
#define plBSTinsert(qe) (rsInsert (CE, qe))
#define plBSTfind(v) (orgGlobal = dstGlobal = v, \
		       rsFindInit (CE, plBSTdummyQE))
#define plBSTabove(v) (orgGlobal = dstGlobal = v, \
		       rsFindInit (CE, plBSTdummyQE), \
		       rsNextSmaller (CE))
#define plBSTnextAbove() (rsNextSmaller (CE))
#define plBSTbelow(v) (orgGlobal = dstGlobal = v, \
		       rsFindInit (CE, plBSTdummyQE), \
		       rsNextLarger (CE))
#define plBSTnextBelow() (rsNextLarger (CE))
#define plBSTtop() (rsLargest (CE))
#define plBSTBot() (rsSmallest (CE))
#define plBSTdelete(qe) (rsDelete (CE, qe))
static int LTsite (int,int) ;

/*--------------------------------------------------------------------------*/

static int plAboveVQE (int v,int qe) 
{
  return sosrLeftTurn (gGlobal, DST (gGlobal, (indexType) qe),
		   ORG (gGlobal, (indexType) qe), v);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static int plAboveQE (int qe1,int qe2)

{
  if (qe2 == plBSTdummyQE) 
    return - plAboveQE (qe2, qe1);

  if (qe1 != plBSTdummyQE) {

/*    fprintf (stdout,"aboveQE %d %d %d, %d %d %d\n",
	    qe1, ORG (gGlobal, qe1), DST (gGlobal, qe1),
	    qe2, ORG (gGlobal, qe2), DST (gGlobal, qe2));*/
    if (ORG (gGlobal, (indexType) qe1) != ORG (gGlobal, (indexType) qe2))
      if (DST (gGlobal, (indexType) qe1) != DST (gGlobal, (indexType) qe2))
	if ((LTsite (ORG (gGlobal, (indexType) qe1),
		     ORG (gGlobal, (indexType) qe2))) &&
	    (LTsite (ORG (gGlobal, (indexType) qe2),
		     DST (gGlobal, (indexType) qe1))))
	  return 
	    (plAboveVQE ((int) ORG (gGlobal, (indexType) qe2), qe1) ? -1 : 1);
	else
	  return 
	    (plAboveVQE ((int) ORG (gGlobal, (indexType) qe1), qe2) ? 1 : -1);
      else
	return 
	  (plAboveVQE ((int) ORG (gGlobal, (indexType) qe1), qe2) ? 1 : -1);
    else
      if (DST (gGlobal, (indexType) qe1) != DST (gGlobal, (indexType) qe2))
	return 
	  (plAboveVQE ((int) DST (gGlobal, (indexType) qe1), qe2) ? 1 : -1);
      else
	return 0;

  } else

    if (orgGlobal == dstGlobal)
      if ((orgGlobal != ORG (gGlobal, (indexType) qe2)) &&
	  (dstGlobal != DST (gGlobal, (indexType) qe2))) {
/*	fprintf (stdout,"PPP %d %d %d\n",
	gGlobal->s[ORG (gGlobal, (indexType) qe2)].coord[0],
	gGlobal->s[orgGlobal].coord[0],
	gGlobal->s[DST (gGlobal, (indexType) qe2)].coord[0]);*/

	return (plAboveVQE (orgGlobal, qe2) ? 1 : -1);
      } else
	return 0;
    else
      if (orgGlobal != ORG (gGlobal, (indexType) qe2))
	return (plAboveVQE (orgGlobal, qe2) ? 1 : -1);
      else
	if (dstGlobal != DST (gGlobal, (indexType) qe2))
	  return (plAboveVQE ((int) dstGlobal, qe2) ? 1 : -1);
	else
	  return 0;
}

/*--------------------------------------------------------------------------*/

static int LTsite (int s1,int s2) 
{
  double dummy;

  return sosrLeftOf (gGlobal, s1, s2, &dummy);
} 

/*--------------------------------------------------------------------------*/

static int GTsiteQS (void *s1,void *s2) 
{
  return (LTsite (*((int *) s1), *((int *) s2)) ? -1 : 1);
} 

/*--------------------------------------------------------------------------*/

static int GTedgeQS (void *e1,void *e2) 
{
  if (ORG (gGlobal, *((int *) e1)) == ORG (gGlobal, *((int *) e2)))
    return 
      (plAboveVQE ((int) DST (gGlobal, *((int *) e2)), *((int *) e1)) ? 1 : -1);
  else    
    return (LTsite ((int) ORG (gGlobal, *((int *) e1)), 
		    (int) ORG (gGlobal, *((int *) e2))) ? -1 : 1);
} 

/*--------------------------------------------------------------------------*/

#define LEFTsite(g, a, b) ((LTsite (a, b)) ? a : b)
#define LEFTVERTEXQE(g, qe) (LEFTsite (g, ORG (g, qe), DST (g, qe)))

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static void plInitializeAll (graphType *g)
{
  int i, j;
  indexType qe, symqe, org, dst;

  gGlobal = g;

  SV = MALLOC (indexType, NUS (g) + 1);
  for (j = 0, i = 0; i < NS (g); i++)
    if (! ISDUPLICATEsite (g, i))
      SV [j++] = i;
  qsort (SV, NUS (g), sizeof (*SV), GTsiteQS);
  SV [NUS(g)] = -1;

/*  for (i = 0; i < NUS (g); i++)
    fprintf (stdout,"site: %lf %lf %d %d\n", SITEX (gGlobal, SV[i]), 
	    SITEY (gGlobal, SV[i]), i, SV [i]);*/

  for (i = 0; i < NE (g); i++) {
    symqe = SYM (qe = grMAKEQE (i));
    if (LEFTVERTEXQE (g, qe) != ORG (g, qe)) {
      org = ORG (g, qe);
      dst = DST (g, qe);
      grUnlinkQE(g, qe);
      if (grMAKEQE (grQETOE (symqe)) == symqe)
        grReinsertSiteSite (g, symqe, dst, org);
      else
        grReinsertSiteSite (g, qe, dst, org);
      if (LEFTVERTEXQE (g, qe) != ORG (g, qe)) {
	fprintf (stdout,"ERROR:\n");
	exit (1);
      }
    }
  }

  LEA = MALLOC (indexType, NE (g) + 1);
  for (i = 0; i < NE (g); i++)
    LEA [i] = grMAKEQE (i);
  qsort (LEA, NE (g), sizeof (*LEA), GTedgeQS);
  LEA [NE(g)] = -1;
  
  for (i = 0; i < NE (g); i++)
    if (LEA [i] == LEA[i+1]) {
      fprintf (stdout,"ERROR: lea: i == i+1.\n");
      exit (1);
    } else {
      if ((ORG (g, LEA[i]) == ORG (g, LEA[i+1])) &&
	  (DST (g, LEA[i]) == DST (g, LEA[i+1]))) {
	fprintf (stdout,"ERROR: duplicate edge found.\n");
	exit (1);
      }
    }

/*  for (i = 0; i < NE (g); i++) {
    qe = LEA[i];

    for (j = 0; j < i; j++)
      if (ORG (g, LEA[i]) == ORG (g, LEA[j]))
	if (sosrCollinear (g, ORG (g, LEA[i]), DST (g, LEA[i]), DST (g, LEA[j])))
	  fprintf (stdout,"WARNING: overlapping edges found.\n");
  }*/

  RM = MALLOC (indexType, NE (g) + 1);
  for (i = 0; i < NE (g); i++)
    RM [i] = 9999999;
  RMbot = SV[0];

  plBSTinit ();
}

/*--------------------------------------------------------------------------*/

static void plCleanup (void) 
{
  FREE (SV);
  FREE (LEA);
  FREE (RM);
  plBSTdispose ();
}

/*--------------------------------------------------------------------------*/

static void plSetRightMost (graphType *g,indexType qe,indexType qeBelow) 
{
/*  fprintf (stdout,"plSetRightmost: %d %d %d\n", qe, qeBelow, ONEXT (g, qe));*/
  if (qeBelow < 0) {
    RM [grQETOE (qe)] = -1;
    RMbot = qe;
  } else {
    RM [grQETOE (qe)] = -1;
    RM [grQETOE (qeBelow)] = qe;
  }
}

/*--------------------------------------------------------------------------*/

static indexType plGetRightmost (indexType qeBelow) 
{
  indexType rm;

  if (qeBelow < 0)
    return RMbot;
  else {
/*    fprintf (stdout,"plGetRightmost %d %d %d\n", qeBelow, grQETOE (qeBelow), 
	    RM [grQETOE (qeBelow)]);
    fflush(stdout);*/
    return (((rm = RM [grQETOE (qeBelow)]) == -1) ? 
	    ONEXT (gGlobal, qeBelow) : rm);
  }
}

/*--------------------------------------------------------------------------*/

static int plAddPrescribedEdges (graphType *g, indexType v,indexType qe) 
{
  indexType pqe, qeBelow;
  int indexIn;

  indexIn = LEAindex;

  if (((pqe = LEA [LEAindex]) != -1) && (v == ORG (g, pqe))) {

    if (qe < 0) {
/*      fprintf (stdout,"prescribe first: %d %d %d\n", pqe, v, DST (g, pqe));*/
      grReinsertSiteSite (g, pqe, v, DST (g, pqe));
    } else
      grReinsertEdgeSite (g, pqe, SYM (OPREV (g, qe)), DST (g, pqe));

    qeBelow = plBSTbelow (v);
    plBSTinsert (pqe);

    plSetRightMost (g, pqe, qeBelow);

    LEAindex++;
    qeBelow = pqe;

    while (((pqe = LEA [LEAindex]) != -1) && (v == ORG (g, pqe))) {

/*fprintf (stdout,"LT %d %d %d %d\n", DST (g, qeBelow), ORG (g, qeBelow),
	                    DST (g, pqe), ORG (g, pqe));*/

      if (sosrLeftTurn (g, DST (g, qeBelow), ORG (g, qeBelow), DST (g, pqe))) {
	fprintf (stdout,"ERROR: addPrescribed: not leftTurn.\n");
	exit (1);
      }

      plBSTinsert (pqe);

      grReinsertEdgeSite (g, pqe, SYM (qeBelow), DST (g, pqe));

      plSetRightMost (g, pqe, qeBelow);

      LEAindex++;
      qeBelow = pqe;
    }
  } 
  
  return LEAindex - indexIn;
}

/*--------------------------------------------------------------------------*/

static void plFindLimits (graphType *g, indexType vertex,
     indexType *qeRM,indexType *qeBelow,indexType *qeAbove,
     indexType *qeTop,indexType *qeBot)
{
  indexType tmp;

  tmp = plBSTfind (vertex);

/*fprintf (stdout,"tmp: %d\n", tmp);*/

  if (tmp == -1)
    tmp = plBSTnextBelow ();
  else
    if (DST (g, tmp) != vertex) {
      fprintf (stdout,"ERROR: plFindLimits: vertex != DST.\n");
      exit (1);
    }

  *qeRM = plGetRightmost (tmp);

/*rsPrintTree (CE);*/
/*fprintf (stdout,"plFindLimits: %d (%d %d) \n", *qeRM, ORG (g, *qeRM), DST (g, *qeRM));*/
  
  *qeBot = tmp;

/*fprintf (stdout,"below: %d\n", *qeBot);*/

  if ((*qeBot < 0) || (DST (g, *qeBot) != vertex))
    *qeBelow = -1;
  else 
    do {
      *qeBelow = *qeBot;
      *qeBot = plBSTnextBelow ();
/*fprintf (stdout,"below: %d %d\n", *qeBelow, *qeBot);*/
    } while ((*qeBot >= 0) && (DST (g, *qeBot) == vertex));

  if ((*qeBot == -1) && (*qeBelow != -1))
    *qeTop = *qeBelow;
  else
    *qeTop = plBSTnextAbove ();

/*fprintf (stdout,"above: %d\n", *qeTop);*/

  if ((*qeTop < 0) || (DST (g, *qeTop) != vertex))
    *qeAbove = -1;
  else 
    do {
      *qeAbove = *qeTop;
      *qeTop = plBSTnextAbove ();
/*fprintf (stdout,"above: %d %d\n", *qeAbove, *qeTop);*/
    } while ((*qeTop >= 0) && (DST (g, *qeTop) == vertex));

/*  fprintf (stdout,"found limits: %d %d %d %d\n", *qeBelow, *qeAbove, *qeBot, *qeTop);*/


  if (*qeAbove != -1)
    while ((tmp = plBSTfind (vertex)) != -1) {
/*rsPrintTree (CE);*/
/*      fprintf (stdout,"deleting %d\n", tmp);*/
      plBSTdelete (tmp);
    }
/*fprintf (stdout,"outout\n");*/
}

/*--------------------------------------------------------------------------*/

void planesweepConstrained (graphType *g, visualType *visual, double *runTime) 
{
  indexType qe, qeRM, vertex, qeBelow, qeAbove, qeBot, qeTop, qeFirst, qeLast;
  indexType qePrev, nop;

  (void) fprintf (stdout,"Plane Sweep:  ");  (void) fflush(stdout);

  *runTime = get_user_time ();

  plInitializeAll (g);
  LEAindex = 0;

  if (nop = plAddPrescribedEdges (g, SV [0], -1))
    if ((nop == 1) && (DST (g, LEA [0]) == SV [1])) {
      RMbot = ONEXT (g, SYM (LEA [0]));
      SVindex = 2;
    } else
      SVindex = 1;
  else {
    qe = grAddSiteSite (g, SV[0], SV[1]);
    RMbot = ONEXT (g, SYM (qe));
    SVindex = 2;
  }

/*fprintf (stdout,"NOP %d %d\n", nop, LEAindex);*/

  while (SVindex < NUS (g)) {
    
/*    fprintf (stdout,"BBC: site = %d %d\n", SVindex, SV[SVindex]);
    fprintf (stdout,"%lf %lf\n", SITEX (g, SV[SVindex]), SITEY (g, SV[SVindex]));*/

    vertex = SV[SVindex];
    
    plFindLimits (g, vertex, &qeRM, &qeBelow, &qeAbove, &qeBot, &qeTop);

/*    fprintf (stdout,"limits: %d %d %d %d\n", qeBelow, qeAbove, qeBot, qeTop);*/

    if ((qeAbove == -1) != (qeBelow == -1)) {
      fprintf (stdout,"ERROR: planesweepConstrained: above != below\n");
      exit (1);
    }

/*    fprintf (stdout,"qeRM %d %d %d\n", qeRM, ORG (g, qeRM), DST (g, qeRM));*/

    if (qeAbove == -1)
      qe = grAddEdgeSite (g, SYM (OPREV (g, qeRM)), vertex);
    else 
      qe = qeAbove;
    
/*    fprintf (stdout,"firstqe: %d %d %d %d\n", qeAbove, qe, ORG (g, qe), DST (g, qe));*/

    qeFirst = SYM (qe);
    qe = ONEXT (g, qe);

    if ((qe != qeTop) && (DST (g, qe) != vertex) &&
	 (! sosrLeftTurn (g, vertex, ORG (g, qe), DST (g, qe)))) {
      
      do {
	qe = grAddEdgeEdge (g, qe, qeFirst);
	
/*	fprintf (stdout,"qe high: %d %d\n", ORG (g, qe), DST (g, qe));*/

	qe = ONEXT (g, qe);
	
      } while ((qe != qeTop) &&
	       (! sosrLeftTurn (g, vertex, ORG (g, qe), DST (g, qe))));
    }

    qeLast = qeFirst;

    if ((qeBelow != -1) && (SYM (qeLast) != qeBelow)) {

      qe = SYM (qeLast);

      do {
	qe = OPREV (g, SYM (OPREV (g, qe)));

	if (DST (g, qe) == vertex) {
          qePrev = OPREV (g, qe);
          grUnlinkQE (g, qe);
          grReinsertEdgeEdge (g, qe, SYM (qePrev), qeLast);
        } else
          qe = grAddEdgeEdge (g, SYM (qe), qeLast);

	qeLast = SYM (qe);
      } while (qe != qeBelow);
    }

    qe = OPREV (g, SYM (qeLast));

    if ((OPREV (g, SYM (qeLast)) != qeBot) &&
	sosrLeftTurn (g, vertex, ORG (g, qe), DST (g, qe))) {

      do {
	qe = OPREV (g, SYM (qe));
	qe = grAddEdgeEdge (g, SYM (qe), qeLast);
/*	fprintf (stdout,"qe low: %d %d\n", ORG (g, qe), DST (g, qe));*/
        qeLast = SYM (qe);
	qe = OPREV (g, qe);
      } while ((qe != qeBot) &&
	       sosrLeftTurn (g, vertex, ORG (g, qe), DST (g, qe)));
    }

    if (qeBot < 0)
      RMbot = ONEXT (g, qeLast);
    else
      RM [grQETOE (qeBot)] = ONEXT (g, qeLast);

    (void) plAddPrescribedEdges (g, vertex, ONEXT (g, qeLast));
    SVindex++;
  }

  plCleanup ();

  *runTime = get_user_time () - *runTime;

INTERRUPT_LABEL: ;
}

/*--------------------------------------------------------------------------*/

void buildInitialTriangulation (
  graphType *g, int printInfo, visualType *visual, double *runTime)

{
  planesweepConstrained (g, visual, runTime);

  fprintf (stdout," completed, cpu used %f.\n\n", *runTime);

  grSetCHstatus (g);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

