#include "internoptri.h"
#include "stack.h"
#include "bitvector.h"

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

#define N3 0
#define N2LOGN 1


/*#define TIMER_ENABLE*/

#ifdef TIMER_ENABLE

#define TIMESUB (ANGLEMAKETIME = ANGLEMAKETIME - get_user_time())
#define TIMEADD (ANGLEMAKETIME = ANGLEMAKETIME + get_user_time())

#else

#define TIMESUB
#define TIMEADD

#endif

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

typedef struct {

  indexType center, left, right;

} wedgeType;

/*--------------------------------------------------------------------------*/

typedef struct {

  indexType center, left, right;

} localTriangleType;

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

/* global variables */ 

static graphType *g, *gPrime;
static visualType * visual;
static poolType *pool;
static queuesType * edges;
static indexType Q, S, globS;
static indexType prevToOrigPR, symNextSymToPR;
static localTriangleType actMax;
static stackType * edgeStack;
static wedgeType wedge;
static int flips, attempts;
static bvType * qsIsNotVerified;
static int qsIsNotVerifiedMaxIndex;

static int (*applicationGT) ();
static int (*applicationRightTurn) ();
static void (*applicationPrintMeasure) ();

#define APP_GT(o1,f1,t1,o2,f2,t2) (*applicationGT) (g,o1,f1,t1,o2,f2,t2)
#define APP_RT(o,f,t) (*applicationRightTurn) (g,o,f,t)

static int (*localGTfun) ();

#define NOT_A_FLIPPABLE_EDGE(g,edge) \
        (grISCHedge (g, edge) || grISCONSTRedge (g, edge))

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

void debugPrintQE (graphType *g, char *name, indexType qe, int intend)

{
/*  int i;


  for (i = 1; i <= intend; i++)
    fprintf (stdout,"   ");

  fprintf (stdout,"%s: %d, %d, %d\n", name, ORG (g, qe), DST (g, qe), qe);
  fflush (stdout);
*/
}

/*--------------------------------------------------------------------------*/

void debugPrintQE2 ( 
     graphType *g, char *name,
     indexType qe, int intend)
{
/*  int i;
  void printEdgeMeasure ();

  for (i = 1; i <= intend; i++)
    fprintf (stdout,"   ");

  fprintf (stdout,"%s: %d, %d, %d\n", name, ORG (g, qe), DST (g, qe), qe);
  fflush (stdout);
*/
/*  printEdgeMeasure (g, qe);*/
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static int localGT (graphType *g,
  localTriangleType *triple1, localTriangleType *triple2)

{
  static int tmp;

  TIMESUB;

  tmp = APP_GT (triple1->center, triple1->left, triple1->right,
		triple2->center, triple2->left, triple2->right);


/*  fprintf (stdout,"localGT %d\n", tmp);*/

  TIMEADD;
  NOFANGLECOMPARISONS++;

  return (int) tmp;

}

/*--------------------------------------------------------------------------*/

static int localGTplusRightTurn (graphType *g,
  localTriangleType *triple1, localTriangleType *triple2)

{
  static int tmp;

  TIMESUB;

  tmp = (APP_RT (triple1->center, triple1->left, triple1->right) ||
	 APP_GT (triple1->center, triple1->left, triple1->right,
		 triple2->center, triple2->left, triple2->right));

  TIMEADD;
  NOFANGLECOMPARISONS++;

  return (int) tmp;

}

/*--------------------------------------------------------------------------*/

static int localGTedges ( graphType *g, indexType edge1,indexType edge2)
{
  static int tmp;

  TIMESUB;

  tmp = APP_GT (ORG (g, edge1), DST (g, ONEXT (g, edge1)),
		DST (g, edge1), ORG (g, edge2),
		DST (g, ONEXT (g, edge2)), DST (g, edge2));

  TIMEADD;
  NOFANGLECOMPARISONS++;

  return (int) tmp;
}

/*--------------------------------------------------------------------------*/

static int isLeft (void)
{
  static int  isNotInsideLeft;

  TIMESUB;
  NOFANGLECOMPARISONS++;

  isNotInsideLeft  = APP_RT (S, globS, wedge.center); 

  TIMEADD;

  return isNotInsideLeft;
}

/*--------------------------------------------------------------------------*/

static void testInsideWedge (
     graphType *g,
     indexType candidate,
     wedgeType *wedge,
     int *isInsideRight,int *isInsideLeft)

{
  TIMESUB;

  *isInsideRight = 
    ! APP_RT (wedge->right, wedge->center, candidate);

  NOFANGLECOMPARISONS++;

  if (*isInsideRight) {
    *isInsideLeft  = 
      ! APP_RT (wedge->left, candidate, wedge->center);

    NOFANGLECOMPARISONS++;
  } else
    *isInsideLeft = 1;

  TIMEADD;
}

/*--------------------------------------------------------------------------*/

static void printEdgeMeasure (graphType *g, indexType edge1)
{
  (*applicationPrintMeasure) (stdout, 
	ORG (g, edge1), SITEX(g,  ORG (g, edge1)), SITEY(g, ORG (g, edge1)),
			      SITEZ(g, ORG (g, edge1)),
        DST (g, ONEXT (g, edge1)), SITEX(g, DST (g, ONEXT (g, edge1))),
			      SITEY(g, DST (g, ONEXT (g, edge1))),
			      SITEZ(g, DST (g, ONEXT (g, edge1))),
        DST (g, edge1), SITEX(g, DST (g, edge1)), SITEY(g,DST (g, edge1)),
			      SITEZ(g,DST (g, edge1)));
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static void markFinalEdges (graphType *g)
{ 
  int i;

  for (i = 0; i < NE(g); i++)
    if (NOT_A_FLIPPABLE_EDGE (g, i)) {
      grSETFINALedge (g, i);
      debugPrintQE (g, "final", grMAKEQE (i), 0);
    } else {
      grUNSETFINALedge (g, i);
      debugPrintQE (g, "not final", grMAKEQE (i), 0);
    }
}

/*--------------------------------------------------------------------------*/

static void unmarkFinalEdges (graphType *g)
{ 
  grResetFINALstatus (g);
}

/*--------------------------------------------------------------------------*/

static void mkCopyOfG (graphType *g, graphType **gPrime)
{  
  int i;

  *gPrime = grMake ();
  **gPrime = *g;
    /* Fix the next line, grAllocate() needs four parameters.
         the final 0 is wrong.  */
  grAllocate (*gPrime, 0, NE (g),0);

  for (i = 0; i < NE(g); i++) {
    grSETDELETEDedge (*gPrime, i);
  }
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

#define poolCreate(graph, fun) (*pool->create) (NQE(graph), graph, fun, \
                                                POOL_FIXED_SIZE)
#define poolDispose(qePool) (*pool->dispose)
#define poolInsert(qePool, qe) (*pool->insert) (qePool, (qe))
#define poolDelete(qePool, qe) (*pool->delete) (qePool, (qe))
#define poolDoesContain(qePool, qe) (*pool->doesContain) (qePool, (qe))

/*--------------------------------------------------------------------------*/

int poolTop ( queuesType *qePool, indexType *pqe)
{
  int found;

  found = (*pool->top) (qePool, pqe);

  return found;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static int GTfunctionForQEpool ( graphType *g, indexType qe1,indexType qe2)
{
  return localGTedges (g, qe1, qe2);
}

/*--------------------------------------------------------------------------*/

void maxMeasureQE (

     graphType *g,
     indexType qe,
     indexType *winner,
     int       *winnerExists)

{
  indexType other;

  other =  OPREV (g, SYM( qe ));

  *winnerExists = 1;

  if (! grISFINALQE (g, OPREV (g, SYM( qe ))))
    if (! grISFINALQE (g, OPREV (g, SYM( other ))))
      *winner = (localGTedges (g, qe, other) ? qe : other);
    else
      *winner = qe;
  else 
    *winner = other;

  other = SYM (ONEXT (g, qe));

  if (! grISFINALQE (g, OPREV (g, SYM( *winner )))) {
    if (! grISFINALQE (g, OPREV (g, SYM( other )))) 
      if (localGTedges (g, other, *winner))
	*winner = other;
  } else 
    if (! grISFINALQE (g, OPREV (g, SYM( other ))))
      *winner = other;
    else
      *winnerExists = 0;
}

  
/*--------------------------------------------------------------------------*/

static queuesType *initializePool (graphType *g, poolType *pool)

{
  int i;
  queuesType * tree;
  indexType qe;
  int exists;

  tree = poolCreate (g, GTfunctionForQEpool);
  
  for (i = 0; i < NE(g); i++) {
    maxMeasureQE (g, grMAKEQE (i), &qe, &exists);
    if (exists && (! poolDoesContain (tree, qe))) {
       poolInsert (tree, qe);
    }

    maxMeasureQE (g, SYM (grMAKEQE (i)), &qe, &exists);
    if (exists && (! poolDoesContain (tree, qe))) {
       poolInsert (tree, qe);
    }
  }
  
  return tree;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

#define SETqsIsNotVerified(x,y) \
  (bvSet (qsIsNotVerified, (x) * qsIsNotVerifiedMaxIndex + y))

#define UNSETqsIsNotVerified(x,y) \
  (bvClear (qsIsNotVerified, (x) * qsIsNotVerifiedMaxIndex + y))

#define TESTqsIsNotVerified(x,y) \
  (bvTest (qsIsNotVerified, (x) * qsIsNotVerifiedMaxIndex + y))

#define MAKEqsIsNotVerified(xMax) \
  (qsIsNotVerifiedMaxIndex = xMax, \
   qsIsNotVerified = bvNew ((xMax+1) * (xMax+1), BV_FIXED_SIZE))

#define DISPOSEqsIsNotVerified() (bvDispose (qsIsNotVerified))

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static void getMaxEdge ( indexType *flipEdge, int *found)
{
  indexType qe;

  *found = poolTop (edges, &qe);

  while (*found && grISFINALedge (g, grQETOE( OPREV(g, SYM(qe))))) {
debugPrintQE (g, "isfinal:", grMAKEQE (grQETOE( OPREV(g, SYM(qe)))), 0);
    poolDelete (edges, qe);
    *found = poolTop (edges, &qe);
  }

  if (*found) {
    *flipEdge = SYM (OPREV (g, SYM(qe)));
  }
}

/*--------------------------------------------------------------------------*/

static void unCutEarP (

     indexType prevToSQ,
     indexType *newPrevToSQ,
     int *finished)

{
  indexType tmp;

  debugPrintQE (gPrime, "unCutEarP", prevToSQ, 6);

  *newPrevToSQ = OPREV (gPrime, SYM (prevToSQ));
  tmp = OPREV (gPrime, SYM (*newPrevToSQ));

  *finished = (DST (gPrime, tmp) != S);
  
  if (! *finished) {
    stkPush (edgeStack, tmp);
    /*(*visual->eraseEdge) (gPrime, visual, tmp);*/
    grUnlinkQE (gPrime, tmp);
  }
}

/*--------------------------------------------------------------------------*/

static void unCutEarsP ( indexType prevToSQ)

{
  indexType newPrevToSQ;
  int finished;

  do {

    debugPrintQE (gPrime, "unCutEarsP", prevToSQ, 5);
    unCutEarP (prevToSQ, &newPrevToSQ, &finished);
    prevToSQ = newPrevToSQ;
  
  } while (! finished);
}

/*--------------------------------------------------------------------------*/

static void unCutEarR (

     indexType nextToSQ,
     indexType *newNextToSQ,
     int *finished)

{
   indexType tmp;

  debugPrintQE (gPrime, "unCutEarR", nextToSQ, 6); /*fprintf (stdout,"   %d\n", S);*/

  *newNextToSQ = ONEXT (gPrime, SYM (nextToSQ));
  tmp = ONEXT (gPrime, SYM (*newNextToSQ));

  *finished = (DST (gPrime, tmp) != S);

  debugPrintQE (gPrime, "unCutEarR_tmp", tmp, 7);
  
  if (! *finished) {
    stkPush (edgeStack, SYM (tmp));
    /*(*visual->eraseEdge) (gPrime, visual, tmp);*/
    grUnlinkQE (gPrime, tmp);
  }
}

/*--------------------------------------------------------------------------*/

static void unCutEarsR ( indexType nextToSQ) 
{
  indexType newNextToSQ;
  int finished;

  do { 

    debugPrintQE (gPrime, "unCutEarsR", nextToSQ, 5);
    unCutEarR (nextToSQ, &newNextToSQ, &finished);
    nextToSQ = newNextToSQ;

  } while (! finished);
}

/*--------------------------------------------------------------------------*/

static void cutEarP (
  indexType prevToSQ, indexType *newPrevToSQ, int *failed,int *done)
{
  localTriangleType newTriangle;
  indexType edge;
  indexType tmp;

  debugPrintQE (gPrime, "cutEarP", prevToSQ, 6);

  newTriangle.center = DST (gPrime, prevToSQ);
  newTriangle.left = ORG (gPrime, prevToSQ);
  tmp = OPREV (gPrime, SYM(prevToSQ));
  newTriangle.right = DST (gPrime, tmp);

  *failed = (*localGTfun) (gPrime, &newTriangle, &actMax);
  
  debugPrintQE (gPrime, "cutEarP_tmp", tmp, 7);
    
  if ((! *failed) && (newTriangle.right != Q)) {
    grUNSETFINALQE (gPrime, prevToSQ);
    stkPop (edgeStack, &edge);
    /*(*visual->drawEdge) (gPrime, visual, tmp);*/
    grReinsertEdgeEdge (gPrime, 
			SYM (edge), SYM (prevToSQ), OPREV (gPrime, SYM (tmp)));
  } else
    grSETFINALQE (gPrime, prevToSQ);
  
  *done = ((! *failed) && (newTriangle.right == Q));

  *newPrevToSQ = (*done || *failed ? prevToSQ : ONEXT (gPrime, prevToSQ));
}

/*--------------------------------------------------------------------------*/

static void cutEarsP ( indexType prevToSQ, int * failed)
{
   int done;
   indexType newPrevToSQ;

  do {

    debugPrintQE (gPrime, "cutEarsP", prevToSQ, 5);
    cutEarP (prevToSQ, &newPrevToSQ, failed, &done);
    prevToSQ = newPrevToSQ;

  } while (! (*failed || done));
}

/*--------------------------------------------------------------------------*/

static void cutEarR (
     indexType prevToSQ,
     int *failed,int *done)

{
   localTriangleType newTriangle;
   indexType edge;
   indexType tmp;

  debugPrintQE (gPrime, "cutEarR", prevToSQ, 6);

  tmp = ONEXT (gPrime, prevToSQ);
  newTriangle.center = DST (gPrime, tmp);
  newTriangle.right = ORG (gPrime, prevToSQ);
  tmp = ONEXT (gPrime, SYM (tmp));
  newTriangle.left = DST (gPrime, tmp);

  *failed = (*localGTfun) (gPrime, &newTriangle, &actMax);
  
  debugPrintQE (gPrime, "cutEarR_tmp", tmp, 7);
  
  if ((! *failed) && (newTriangle.left != Q)) {
    grUNSETFINALQE (gPrime, ONEXT (gPrime, prevToSQ));
    stkPop (edgeStack, &edge);
    /*(*visual->drawEdge) (gPrime, visual, tmp);*/
    grReinsertEdgeEdge (gPrime, edge, SYM (prevToSQ), SYM (tmp));
  } else
    grSETFINALQE (gPrime, ONEXT (gPrime, prevToSQ));
  
  *done = ((! *failed) && (newTriangle.left == Q));
}

/*--------------------------------------------------------------------------*/

static void cutEarsR ( indexType prevToSQ, int *failed) 
{
  int done;

  do {

    debugPrintQE (gPrime, "cutEarsR", prevToSQ, 5);
    cutEarR (prevToSQ, failed, &done);
  
  } while (! (*failed || done));
}

/*--------------------------------------------------------------------------*/

static void cutEarsPandR (indexType prevToSQ, int *failedLeft,int *failedRight)
{
  indexType newPrevToSQ;
  int doneLeft, doneRight;
  
  do {

    debugPrintQE (gPrime, "cutEarsPandR", prevToSQ, 4);

    cutEarR (prevToSQ, failedRight, &doneRight);
    cutEarP (prevToSQ, &newPrevToSQ, failedLeft, &doneLeft);
  
    prevToSQ = newPrevToSQ;

  } while (! (*failedLeft || *failedRight || doneLeft || doneRight));

  if (! (*failedLeft || *failedRight))
    if (doneLeft && (! doneRight)) 
      cutEarsR (prevToSQ, failedRight);
    else
      if (doneRight && (! doneLeft))
	cutEarsP (prevToSQ, failedLeft);
}

/*--------------------------------------------------------------------------*/

static void pushEarsP ( indexType prevToSQ)

{
  indexType count = 0;

  debugPrintQE (gPrime, "pushEarsP", prevToSQ, 6);

  while (! grISFINALQE (gPrime, prevToSQ)) {
    prevToSQ = ONEXT (gPrime, prevToSQ);
    count++;
  }

  while (count) {
  debugPrintQE (gPrime, "pushing Ear P", SYM(prevToSQ), 6);
    stkPush (edgeStack, SYM (prevToSQ));
    prevToSQ = OPREV (gPrime, prevToSQ);
    count--;
  }
}

/*--------------------------------------------------------------------------*/

static void pushEarsR ( indexType nextToSQ)

{
  indexType count = 0;
  
  debugPrintQE (gPrime, "pushEarsR", nextToSQ, 6);
  
  while (! grISFINALQE (gPrime, nextToSQ)) {
    nextToSQ = OPREV (gPrime, nextToSQ);
    count++;
  }
  
  while (count) {
  debugPrintQE (gPrime, "pushing Ear R", nextToSQ, 6);
    stkPush (edgeStack, nextToSQ);
    nextToSQ = ONEXT (gPrime, nextToSQ);
    count--;
  }
}

/*--------------------------------------------------------------------------*/

static int findS ( indexType PR)
{
  indexType prevToSQ, nextToSQ;
  static int failed, failedLeft, failedRight, isInsideLeft, isInsideRight;
  static indexType tmp;
  static int dummy, existsPfan, existsRfan;

 debugPrintQE (gPrime, "FindS", PR, 3); /*fprintf (stdout,"                %d\n", Q);*/

  if (grISFINALQE (g, PR))
    failed = 1;
  else {
    prevToSQ = SYM (ONEXT (g, PR));
    nextToSQ = ONEXT (g, prevToSQ); /* == SYM (OPREV (g, SYM (PR))) */
    S = ORG (g, prevToSQ); /* == third */

    if (grISDELETEDedge (gPrime, grQETOE (prevToSQ))) {
      grDuplicate (g, gPrime, prevToSQ);
      grUNSETDELETEDedge (gPrime, grQETOE(prevToSQ));
    }
    if (grISDELETEDedge (gPrime, grQETOE(nextToSQ))) {
      grDuplicate (g, gPrime, nextToSQ);
      grUNSETDELETEDedge (gPrime, grQETOE(nextToSQ));
    }

    grUnlinkQE(gPrime, PR);
    
    stkPush (edgeStack, PR);
    (flips)++;

    (*visual->eraseEdge) (g, visual, PR);
INTERRUPT_ALGORITHM;
    
    testInsideWedge (g, S, &wedge, &isInsideRight, &isInsideLeft);
    
    if (isInsideRight && isInsideLeft) {
      cutEarsPandR (prevToSQ, &failedLeft, &failedRight);
      failed = (failedLeft && failedRight);
      
      if (! failed)
	if (failedLeft) {
	  unCutEarsR (nextToSQ);
	  wedge.left = S;
	  failed = ! findS (nextToSQ);
INTERRUPT_ALGORITHM;
	  existsPfan = 1;
	  existsRfan = 0;
	} else 
	  if (failedRight) {
	    unCutEarsP (prevToSQ);
	    wedge.right = S;
	    failed = ! findS (SYM (prevToSQ));
INTERRUPT_ALGORITHM;
	    existsPfan = 0;
	    existsRfan = 1;
	  } else {
	    stkPop (edgeStack, &tmp);
	    grReinsertEdgeEdge (gPrime, tmp, 
				SYM (OPREV (gPrime, SYM (prevToOrigPR))),
				SYM (ONEXT (gPrime, prevToOrigPR)));
	    if (! stkEmpty (edgeStack))
	      fprintf (stdout,"ERROR: edgeinsert: finds: not stkEmpty!\n");
	    stkPush (edgeStack, tmp);
	    if (poolDoesContain (edges, prevToSQ))
	      poolDelete (edges, prevToSQ);
	    existsPfan = 1;
	    existsRfan = 1;
	  }
    } else
      if (! isInsideLeft) {
	cutEarsP (prevToSQ, &dummy);
	failed = ! findS (nextToSQ);
INTERRUPT_ALGORITHM;
	existsPfan = 1;
	existsRfan = 0;
      } else {
	cutEarsR (prevToSQ, &dummy);
	failed = ! findS (SYM (prevToSQ));
INTERRUPT_ALGORITHM;
	existsPfan = 0;
	existsRfan = 1;
      }
      
    if (! failed) {
      if (poolDoesContain (edges, PR))
	poolDelete (edges, PR);
      if (poolDoesContain (edges, SYM (PR)))
	poolDelete (edges, SYM (PR));
      if (poolDoesContain (edges, SYM (nextToSQ)))
	poolDelete (edges, SYM (nextToSQ));
      if (poolDoesContain (edges, OPREV (g, PR)))
	poolDelete (edges, OPREV (g, PR));

      if (existsRfan)
	pushEarsR (nextToSQ);
      if (existsPfan)
	pushEarsP (prevToSQ);
      
      grUnlinkQE(g, PR);
    } else {
      (*visual->drawEdge) (g, visual, PR, 1);
INTERRUPT_ALGORITHM;
    }      

    grSETDELETEDedge (gPrime, grQETOE(prevToSQ));
    grSETDELETEDedge (gPrime, grQETOE(nextToSQ));
  }

INTERRUPT_LABEL: 
  return ! failed;
}

/*--------------------------------------------------------------------------*/

static int verifyS ( indexType PR)
{
  indexType prevToSQ, nextToSQ;
  static int failed, failedLeft, failedRight, isInsideLeft, isInsideRight;
  static indexType tmp;
  static int dummy, existsPfan, existsRfan;

debugPrintQE (gPrime, "verifyS", PR, 3); /*fprintf (stdout,"                %d\n", Q);*/

  if (grISFINALQE (g, PR))
    failed = 1;
  else {
    prevToSQ = SYM (ONEXT (g, PR));
    nextToSQ = ONEXT (g, prevToSQ); /* == SYM (OPREV (g, SYM (PR))) */
    S = ORG (g, prevToSQ); /* == third */
    if (grISDELETEDedge (gPrime, grQETOE (prevToSQ))) {
      grDuplicate (g, gPrime, prevToSQ);
      grUNSETDELETEDedge (gPrime, grQETOE(prevToSQ));
    }
    if (grISDELETEDedge (gPrime, grQETOE(nextToSQ))) {
      grDuplicate (g, gPrime, nextToSQ);
      grUNSETDELETEDedge (gPrime, grQETOE(nextToSQ));
    }

    grUnlinkQE(gPrime, PR);
    
    stkPush (edgeStack, PR);
    (flips)++;

    (*visual->eraseEdge) (g, visual, PR);
INTERRUPT_ALGORITHM;

    testInsideWedge (g, S, &wedge, &isInsideRight, &isInsideLeft);

    if (isInsideRight && isInsideLeft) {
      cutEarsPandR (prevToSQ, &failedLeft, &failedRight);
      if (failedLeft || failedRight)  {
	UNSETqsIsNotVerified (Q, S);
	if (S != globS)
	  if (isLeft ()) {
	    debugPrintQE (gPrime, "verifying", prevToSQ, 3);	
	    unCutEarsP (prevToSQ);
	    cutEarsR (prevToSQ, &failedRight);
	    wedge.right = S;
	    failed = ! verifyS (SYM (prevToSQ));
	    existsPfan = 0;
	    existsRfan = 1;
	  } else {
	    debugPrintQE (gPrime, "verifying", nextToSQ, 3);	
	    unCutEarsR (nextToSQ);
	    cutEarsP (OPREV (gPrime, nextToSQ), &failedLeft);
	    wedge.left = S;
	    failed = ! verifyS (nextToSQ);
	    existsPfan = 1;
	    existsRfan = 0;
	  }
	else
	  failed = 1;
      } else {
	failed = 0;
	stkPop (edgeStack, &tmp);
	grReinsertEdgeEdge (gPrime, tmp, 
			    SYM (OPREV (gPrime, SYM (prevToOrigPR))),
			    SYM (ONEXT (gPrime, prevToOrigPR)));
	if (! stkEmpty (edgeStack))
	  fprintf (stdout,"ERROR: edgeinsert: verifyS: not stkEmpty!\n");
	stkPush (edgeStack, tmp);
	if (poolDoesContain (edges, prevToSQ))
	  poolDelete (edges, prevToSQ);
	existsPfan = 1;
	existsRfan = 1;
      }
    } else
      if (S != globS)
	if (! isInsideLeft) {
	  cutEarsP (prevToSQ, &dummy);
	  failed = ! verifyS (nextToSQ);
	  existsPfan = 1;
	  existsRfan = 0;
	} else {
	  cutEarsR (prevToSQ, &dummy);
	  failed = ! verifyS (SYM (prevToSQ));
	  existsPfan = 0;
	  existsRfan = 1;
	}
      else 
	failed = 1;
    
    if (! failed) {
      if (poolDoesContain (edges, PR))
	poolDelete (edges, PR);
      if (poolDoesContain (edges, SYM (PR)))
	poolDelete (edges, SYM (PR));
      if (poolDoesContain (edges, SYM (nextToSQ)))
	poolDelete (edges, SYM (nextToSQ));
      if (poolDoesContain (edges, OPREV (g, PR)))
	poolDelete (edges, OPREV (g, PR));
      
      if (existsRfan)
	pushEarsR (nextToSQ);
      if (existsPfan)
	pushEarsP (prevToSQ);
      
      grUnlinkQE(g, PR);
    } else {
      (*visual->drawEdge) (g, visual, PR, 1);
INTERRUPT_ALGORITHM;
    }

    grSETDELETEDedge (gPrime, grQETOE(prevToSQ));
    grSETDELETEDedge (gPrime, grQETOE(nextToSQ));
  }

INTERRUPT_LABEL: ;	
  return ! failed;
}

/*--------------------------------------------------------------------------*/

static int findSslow ( indexType PR) 
{
  static int succeeded, isInsideLeft, isInsideRight;


  UNSETqsIsNotVerified (Q, wedge.center);
  for (globS = 0; ISDUPLICATEsite (g, globS); globS++) {}
  succeeded = 0;

  do {
    if (TESTqsIsNotVerified (Q, globS)) {
      testInsideWedge (g, globS, &wedge, &isInsideRight, &isInsideLeft);
      if (isInsideRight && isInsideLeft && (globS != wedge.left) &&
	  (globS != wedge.right)) {

	succeeded = verifyS (PR);
INTERRUPT_ALGORITHM;
	if (! succeeded) {
	  wedge.center = Q;
	  wedge.left = ORG (g, PR);
	  wedge.right = DST (g, PR);
	  stkReset (edgeStack);
	  grDuplicate (g, gPrime, PR);
	  grUNSETDELETEDedge (gPrime, grQETOE(PR));
	  grDuplicate (g, gPrime, prevToOrigPR);
	  grUNSETDELETEDedge (gPrime, grQETOE(prevToOrigPR));
	  grDuplicate (g, gPrime, symNextSymToPR);
	  grUNSETDELETEDedge (gPrime, grQETOE(symNextSymToPR));
	}
	UNSETqsIsNotVerified (Q, wedge.center);

      }
    }
    for (globS++; (globS < NS(g)) && ISDUPLICATEsite (g, globS); globS++) {}
  } while ((! succeeded) && (globS < NS(g)));

INTERRUPT_LABEL: ;
  return succeeded;
}

/*--------------------------------------------------------------------------*/

static void initializeQsIsNotVerified (void) 
{
  int i, j;

  MAKEqsIsNotVerified (NS(g));
  
  for (i = 0; i <= NS(g); i++)
    for (j = 0; j <= NS(g); j++)
      SETqsIsNotVerified (i, j);
}

/*--------------------------------------------------------------------------*/

static void freeQsIsNotVerified (void) 
{
 DISPOSEqsIsNotVerified ();
}

/*--------------------------------------------------------------------------*/

static void findQS ( indexType PR, int modus)

{
  int found;
  indexType qe;
  indexType maxQE;
  int existsMaxQE;

  prevToOrigPR = OPREV (g, PR);
  symNextSymToPR = SYM (ONEXT (g, SYM (PR)));
  
  Q = DST (g, prevToOrigPR);

  actMax.center = Q;
  actMax.left = ORG (g, PR);
  actMax.right = DST (g, PR);

  wedge.center = Q;
  wedge.left = ORG (g, PR);
  wedge.right = DST (g, PR);

  stkReset (edgeStack);

  poolDelete (edges, symNextSymToPR);

  grDuplicate (g, gPrime, PR);
  grUNSETDELETEDedge (gPrime, grQETOE(PR));
  grDuplicate (g, gPrime, prevToOrigPR);
  grUNSETDELETEDedge (gPrime, grQETOE(prevToOrigPR));
  grDuplicate (g, gPrime, symNextSymToPR);
  grUNSETDELETEDedge (gPrime, grQETOE(symNextSymToPR));

  debugPrintQE (gPrime, "FindQS", PR, 2);
/*  printEdgeMeasure (g, symNextSymToPR);*/

  flips = 0;

  found = (modus == N2LOGN ? findS (PR) : findSslow (PR));
INTERRUPT_ALGORITHM;

  grSETDELETEDedge (gPrime, grQETOE(PR));
  grSETDELETEDedge (gPrime, grQETOE(prevToOrigPR));
  grSETDELETEDedge (gPrime, grQETOE(symNextSymToPR));


  if (! found) {
    debugPrintQE (gPrime, "finalizing", PR, 0);
    debugPrintQE (gPrime, "finalizing", prevToOrigPR, 0);
    debugPrintQE (gPrime, "finalizing", symNextSymToPR, 0);

    grSETFINALedge (g, grQETOE (PR));
    grSETFINALedge (g, grQETOE (prevToOrigPR));
    grSETFINALedge (g, grQETOE (symNextSymToPR));
    attempts = flips;
    flips = 0;
  } else {
    do {
      stkPop (edgeStack, &qe);

  debugPrintQE (gPrime, "before", qe, 3);
  debugPrintQE (gPrime, "onext", ONEXT (gPrime, qe), 3);
  debugPrintQE (gPrime, "oprev", OPREV (gPrime, qe), 3);
  debugPrintQE (gPrime, "onext_sym", ONEXT (gPrime, SYM (qe)), 3);
  debugPrintQE (gPrime, "oprev_sym", OPREV (gPrime, SYM (qe)), 3);

      grReinsertEdgeEdge (g, qe, SYM (OPREV (g, ONEXT (gPrime, qe))),
			  OPREV (gPrime, SYM (qe)));

      (*visual->drawEdge) (g, visual, qe, 1);
INTERRUPT_ALGORITHM;

  debugPrintQE (g, "after", qe, 3);

      maxMeasureQE (g, qe, &maxQE, &existsMaxQE);
      if (existsMaxQE)
	poolInsert (edges, maxQE);

    } while (! stkEmpty (edgeStack));

    maxMeasureQE (g, symNextSymToPR, &maxQE, &existsMaxQE);
    if (existsMaxQE)
      poolInsert (edges, maxQE);
    attempts = 0;
  }

INTERRUPT_LABEL: ;
}

/*--------------------------------------------------------------------------*/

static void doEdgeInsertion (int modus,
    graphType *local_g, visualType *local_visual, poolType *local_pool,
    int (*local_applicationGT)(graphType *, indexType, indexType, indexType, indexType, indexType, indexType),
    int GTdecidesRightTurn,
    int (*local_applicationRightTurn)(graphType *, indexType, indexType, indexType),
    void (*local_applicationPrintMeasure)(FILE *, indexType, coordType, coordType, coordType, indexType, coordType, coordType, coordType, indexType, coordType, coordType, coordType),
    int *nofFlips, int *nofAttempts, double *runTime)
     
{
  indexType newPR;
  int found;
  
  g = local_g;
  visual = local_visual;
  pool = local_pool;
  applicationGT = local_applicationGT;
  applicationRightTurn = local_applicationRightTurn;
  applicationPrintMeasure = local_applicationPrintMeasure;

  if (GTdecidesRightTurn == GT_CAN_DECIDE_RIGHT_TURN)
    localGTfun = localGT;
  else 
    localGTfun = localGTplusRightTurn;
  
  *runTime = get_user_time();
  markFinalEdges (g);
  edges = initializePool (g, pool);
  edgeStack = stkNew(2*NE(g));
  mkCopyOfG (g, &gPrime);
  if (modus == N3)
    initializeQsIsNotVerified ();
  
  *nofFlips = 0;
  *nofAttempts = 0;

  getMaxEdge (&newPR, &found);

  while (found) {
    findQS(newPR, modus);
INTERRUPT_ALGORITHM;   

    *nofFlips += flips;
    *nofAttempts += attempts;

    getMaxEdge (&newPR, &found);
  }

INTERRUPT_LABEL:
  stkDispose (edgeStack);
  poolDispose (edges);
  qeDispose (grQE (gPrime));
  FREE (gPrime);
  unmarkFinalEdges (g);
  if (modus == N3)
    freeQsIsNotVerified ();
if (! SHOULD_INTERRUPT_ALGORITHM) {
  *runTime = get_user_time() - *runTime;
  fprintf (stdout,"  completed, cpu used ... %f.\n", *runTime);
  fprintf (stdout,"                  # flips ... %d, #attempts %d.\n\n",
	  *nofFlips, *nofAttempts);
}
} 

/*--------------------------------------------------------------------------*/

void edgeInsertionN3 (
    graphType *local_g, visualType *local_visual, poolType *local_pool,
    int (*local_applicationGT)(graphType *, indexType, indexType, indexType, indexType, indexType, indexType),
    int GTdecidesRightTurn,
    int (*local_applicationRightTurn)(graphType *, indexType, indexType, indexType),
    void (*local_applicationPrintMeasure)(FILE *, indexType, coordType, coordType, coordType, indexType, coordType, coordType, coordType, indexType, coordType, coordType, coordType),
    int *nofFlips, int *nofAttempts, double *runTime)
     
{
/*  grNormalize (local_g);*/

  doEdgeInsertion (N3, local_g, local_visual, local_pool,
		   local_applicationGT, GTdecidesRightTurn,
		   local_applicationRightTurn,
		   local_applicationPrintMeasure,
		   nofFlips, nofAttempts, runTime); 
}

/*--------------------------------------------------------------------------*/

void edgeInsertionN2LOGN (graphType *local_g,
    visualType *local_visual, poolType *local_pool,
    int (*local_applicationGT)(graphType *, indexType, indexType, indexType, indexType, indexType, indexType),
    int GTdecidesRightTurn,
    int (*local_applicationRightTurn)(graphType *, indexType, indexType, indexType),
    void (*local_applicationPrintMeasure)(FILE *, indexType, coordType, coordType, coordType, indexType, coordType, coordType, coordType, indexType, coordType,
coordType, coordType),
    int *nofFlips, int *nofAttempts, double *runTime)
     
{
/*  grNormalize (local_g);*/

  doEdgeInsertion (N2LOGN, local_g, local_visual, local_pool, 
		   local_applicationGT, GTdecidesRightTurn,
		   local_applicationRightTurn, 
		   local_applicationPrintMeasure,
		   nofFlips, nofAttempts, runTime); 
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
