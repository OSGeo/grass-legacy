/*--------------------------------------------------------------------------*/

#include "optri.h"
#include "basic.h"

/*--------------------------------------------------------------------------*/

/* flips.c */

extern void lawsonFlip ();

/*--------------------------------------------------------------------------*/

/* graph.c */

typedef struct {

  void  	*s;
  void   	*e;

} graphType;

/*--------------------------------------------------------------------------*/

/* angle.c */

extern void buildMinMaxAngleTriangulation (/* g, printInfo, visual, 
					   nofFlips, nofAttempts, runTime */);  
extern void buildMinMaxAngleTriangulation2 (/* g, printInfo, visual, 
					    nofFlips, nofAttempts, runTime */);

/*--------------------------------------------------------------------------*/

/* height.c */

extern void buildMaxMinHeightTriangulation (/* g, printInfo, visual, 
					    nofFlips, nofAttempts, runTime */);
extern void buildMaxMinHeightTriangulation2 (/* g, printInfo, visual, 
					     nofFlips, nofAttempts, runTime */);

/*--------------------------------------------------------------------------*/

/* slope.c */

extern void buildMinMaxSlopeTriangulation (/* g, printInfo, visual, 
					   nofFlips, nofAttempts, runTime */);

/*--------------------------------------------------------------------------*/

/* delaunay.c */

extern void buildDelaunayTriangulation
               (/* g, printInfo, visual, nofFlips, nofAttempts, runTime */);

/*--------------------------------------------------------------------------*/

/* regular.c */

extern void incrementalDelaunayTriangulation
               (/* g, printInfo, visual, nofFlips, nofAttempts, runTime */);
extern void regularTriangulation
               (/* g, printInfo, visual, nofFlips, nofAttempts, runTime */);

/*--------------------------------------------------------------------------*/

/* plaesweep.c */

extern void buildInitialTriangulation (/* g, printInfo, visual, runTime */);

/*--------------------------------------------------------------------------*/

/* heur.angle.c */

extern int haComputeAngleQuadrantOnly (/* g, origin, from, to */);
extern int haAngleGT180 (/* g, origin, from, to */);
extern int haAngleGT (/* g, origin1, from1, to1, origin2, from2, to2 */);
extern void haPrintAngle (/* */);

/*--------------------------------------------------------------------------*/

/* heur.height.c */

extern int hhHeightLT0 (/* g, origin, from, to */);
extern int hhHeightLT (/* g, origin1, from1, to1, origin2, from2, to2 */);
extern void hhPrintHeight (/* */);

/*--------------------------------------------------------------------------*/

/* heur.slope.c */

extern int hsSlopeRightTurn (/* g, origin, from, to */);
extern int hsSlopeGT (/* g, origin1, from1, to1, origin2, from2, to2 */);
extern void hsPrintSlope (/* */);

/*--------------------------------------------------------------------------*/

/* hdag.c */

extern void * hdagNew (/* n, sites, localLeftTurnTest, 
			            localTerminationCriterion */);
extern void hdagDispose (/* h */);

extern int hdagIsHDAGtype (/* h */);

extern void * hdagPQ (/* h */);
extern void * hdagSI (/* h */);

extern void hdagInsertTopTriangle (/* h, v1, v2, v3 */);
extern indexType hdagLocatePoint (/* h, p, spl */);
extern indexType hdagInsertPoint (/* h, p, spl */);

/*--------------------------------------------------------------------------*/

/* persquadedge.c */

extern void * pqeNew (/* n */);
extern void pqeDispose (/* pq */);
extern indexType pMAKEQE (/* e */);
extern indexType pQETOE (/* qe */);
extern indexType pSYM (/* qe */);
extern indexType pORG (/* pq, qe */);
extern indexType pDST (/* pq, qe */);
extern void pSETORG (/* pq, qe, org */);
extern void pSETDST (/* pq, qe, org */);
extern int pqeISLASTENTRY (/* nxt, index, maxEntries, additional */);
extern int pqeISDEADslow (/* pq, qe, time */);
extern int pqeISDEAD (/* nxt, index, maxEntries, additional */);
extern int pqeISNEW (/* pq, qe, time */);
extern void pqeGetPresent (/* pq, qe, time, nxt, index, maxEntries, 
			      additional */);
extern void pqeGetFuture (/* nxt, index, maxEntries, additional,
			     futureNxt, futureIndex, futureMaxEntries, 
			     futureAdditional */);
extern int pqeHasFuture (/* nxt, index, maxEntries, additional */);
extern indexType pONEXT (/* pq, qe, time */);
extern indexType pOPREV (/* pq, qe, time */);
extern void pSETONEXT (/* pq, qe, next */);
extern indexType pqeAddSiteSite (/* pq, s1, s2 */);
extern indexType pqeAddEdgeSite (/* pq, a, s */);
extern indexType pqeAddEdgeEdge (/* pq, a, b */);
extern indexType pqeFlip (/* pq, qe */);
extern void pqeDelete (/* pq, qe */);
extern indexType pqeAddTriangleStar (/* pq, qe, p */);
extern void pqeDeleteTriangleStar (/* pq, qe */);
extern int pISCHQE (/* pq, qe, time */);
extern int pISCHedge (/* pq, edge, time */);
extern int pqeExistsAtTime (/* pq, pqe, time */);
extern void printPersistQe (/* pql, index */);
extern indexType pTimeNow (/* pq */);
extern indexType pTime0 (/* pq */);
extern indexType pNPE (/* pq */);

/*--------------------------------------------------------------------------*/

/* timer.c */

extern double get_user_time (/* */);
extern double get_time_of_day (/* */);

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

/* abbreviations */

#define NS(g) siNS ((g)->s)
#define NUS(g) siNUS ((g)->s)
#define NSmax(g) siNSmax ((g)->s)
#define SITEX(g, i) siSITEX ((g)->s, (i))
#define SITEY(g, i) siSITEY ((g)->s, (i))
#define SITEZ(g, i) siSITEZ ((g)->s, (i))
#define ISDUPLICATEsite(g, i) siIsDuplicateSite ((g)->s, (i))
#define grNewQE(n) qeNew(n)
#define grDisposeQE(g) qeDispose((g)->e)
#define NE(g) qeNE((g)->e)
#define NEmax(g) qeNEmax((g)->e)
#define NQE(g) qeNQE((g)->e)
#define NQEmax(g) qeNQEmax((g)->e)
#define NEset(g, ne) qeNEset((g)->e, ne)
#define NEset(g, ne) qeNEset((g)->e, ne)
#define NEreset(g) qeNEreset((g)->e)
#define grAdjustSizeQE(g,nNew) qeAdjustSize((g)->e, nNew)
#define grSETnChQE(g,nofCHQE) qeSETnChQE((g)->e,nofCHQE)
#define grNCHQE(g) qeNCHQE((g)->e)
#define grSETchQE(g,chQE) qeSETchQE((g)->e,chQE)
#define grCHQE(g) qeCHQE((g)->e)
#define SYM(qe) qeSYM(qe) 
#define ORG(g, qe) qeORG((g)->e, qe) 
#define DST(g, qe) qeDST((g)->e, qe) 
#define SETORG(g, qe, org) qeSETORG((g)->e, qe, org) 
#define SETDST(g, qe, org) qeSETDST((g)->e, qe, org) 
#define ONEXT(g, qe) qeONEXT((g)->e, qe) 
#define OPREV(g, qe) qeOPREV((g)->e, qe) 
#define SETONEXT(g, qe, next) qeSETONEXT((g)->e, qe, next) 
#define grReinsertSiteSite(g, qe, newA, newB) \
        qeReinsertSiteSite((g)->e, qe, newA, newB) 
#define grAddSiteSite(g, s1, s2) qeAddSiteSite((g)->e, s1, s2) 
#define grReinsertEdgeSite(g, qe, newA, newS) \
        qeReinsertEdgeSite((g)->e, qe, newA, newS) 
#define grAddEdgeSite(g, a, s) qeAddEdgeSite((g)->e, a, s) 
#define grReinsertEdgeEdge(g, qe, newA, newB) \
        qeReinsertEdgeEdge((g)->e, qe, newA, newB) 
#define grAddEdgeEdge(g, a, b) qeAddEdgeEdge((g)->e, a, b) 
#define grFlipEdge(g, qe) qeFlip((g)->e, qe) 
#define grUnlinkQE(g, qe) qeUnlink((g)->e, qe) 
#define grDuplicate(gSource, gDest, qe) qeDuplicate(gSource->e, gDest->e, qe) 
#define grResetCONSTRstatus(g) qeResetCONSTRstatus((g)->e) 
#define grResetFINALstatus(g) qeResetFINALstatus((g)->e) 
#define grResetDELETEDstatus(g) qeResetDELETEDstatus((g)->e) 
#define grSetDELETEDstatus(g) qeSetDELETEDstatus((g)->e) 
#define grResetALLstati(g) qeResetALLstati((g)->e) 
#define grMAKEQE(e) qeMAKEQE(e) 
#define grQETOE(qe) qeQETOE(qe) 
#define grQETOQE0(qe) qeQETOQE0(qe) 
#define grISCHQE(g, qe) qeISCHQE((g)->e, qe) 
#define grSETCHQE(g, qe) qeSETCHQE((g)->e, qe) 
#define grUNSETCHQE(g, qe) qeUNSETCHQE((g)->e, qe) 
#define grISCONSTRQE(g, qe) qeISCONSTRQE((g)->e, qe) 
#define grSETCONSTRQE(g, qe) qeSETCONSTRQE((g)->e, qe) 
#define grUNSETCONSTRQE(g, qe) qeUNSETCONSTRQE((g)->e, qe) 
#define grISFINALQE(g, qe) qeISFINALQE((g)->e, qe) 
#define grSETFINALQE(g, qe) qeSETFINALQE((g)->e, qe) 
#define grUNSETFINALQE(g, qe) qeUNSETFINALQE((g)->e, qe) 
#define grISDELETEDQE(g, qe) qeISDELETEDQE((g)->e, qe) 
#define grSETDELETEDQE(g, qe) qeSETDELETEDQE((g)->e, qe) 
#define grUNSETDELETEDQE(g, qe) qeUNSETDELETEDQE((g)->e, qe) 
#define grISCHedge(g, edge) qeISCHedge((g)->e, edge) 
#define grSETCHedge(g, edge) qeSETCHedge((g)->e, edge) 
#define grUNSETCHedge(g, edge) qeUNSETCHedge((g)->e, edge) 
#define grISCONSTRedge(g, edge) qeISCONSTRedge((g)->e, edge) 
#define grSETCONSTRedge(g, edge) qeSETCONSTRedge((g)->e, edge) 
#define grUNSETCONSTRedge(g, edge) qeUNSETCONSTRedge((g)->e, edge) 
#define grISFINALedge(g, edge) qeISFINALedge((g)->e, edge) 
#define grSETFINALedge(g, edge) qeSETFINALedge((g)->e, edge) 
#define grUNSETFINALedge(g, edge) qeUNSETFINALedge((g)->e, edge) 
#define grISDELETEDedge(g, edge) qeISDELETEDedge((g)->e, edge) 
#define grSETDELETEDedge(g, edge) qeSETDELETEDedge((g)->e, edge) 
#define grUNSETDELETEDedge(g, edge) qeUNSETDELETEDedge((g)->e, edge) 
#define grNormalize(g) qeNormalize((g)->e) 

/*--------------------------------------------------------------------------*/

typedef struct {

  indexType  *entry;
  indexType  *indexOf;
  void       *in;
  int         free;
  int         maxn; 
  int         maxIndex;
  int         nofElts;
  int         top;
  int         (*GT) ();
  char        *data;
  int         mode;

} queuesType;

#define POOL_FIXED_SIZE 23423
#define POOL_ADJUSTABLE_SIZE 4564243

typedef struct {

  queuesType * (*create) ();
  int        (*top) ();
  void       (*insert) ();
  void       (*delete) ();
  int        (*doesContain) ();
  void       (*dispose) ();
  
} poolType;

/*--------------------------------------------------------------------------*/

extern int GLOBAL_INTERRUPT_ALGORITHM;

/*---------------------------------------------------------------------------*/

double ANGLEMAKETIME;
long   NOFANGLECOMPARISONS;

#define GT_CAN_DECIDE_RIGHT_TURN 1
#define GT_CANNOT_DECIDE_RIGHT_TURN 0

/*--------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

#ifdef XXX

#include "forms.h"
  typedef FL_FORM menuType;

#else

  typedef char menuType;

#endif

typedef float colorType [3];

typedef struct {

  void (*initialize) ();
  void (*drawEdge) ();
  void (*redrawEdge) ();
  void (*eraseEdge) ();
  void (*memorizeEraseEdge) ();
  void (*memorizeDrawEdge) ();
  void (*memorizeRedrawWindow) ();
  void (*memorizeRedrawEdge) ();
  void (*cleanUp) ();
  void (*redrawWindow) ();
  menuType * (*makeMainMenu) ();
  menuType * (*makeChoiceMenu) ();
  void (*resetChoice) ();
  void (*pause) ();
  int (*getChoiceButton) ();
  void (*quality2dSetFunctions) ();
  void (*rotateRedraw) ();
  void (*autoRotateView) ();
  void (*drawVertices) ();
  long (*getThirdCoord) ();
  indexType (*dst) ();
  indexType (*org) ();
  indexType (*onext) ();
  indexType (*oprev) ();
  indexType (*sym) ();
  indexType (*qetoe) ();
  indexType (*makeqe) ();
  int (*isDeleted) ();
  void (*setDeleted) ();
  void (*unsetDeleted) ();
  void (*switchToPQE) ();
  int (*timeSup) ();
  char * (*getFileName) ();
  char * winTitle;
  int xLen, yLen;
  int x0, y0, x1, y1;
  int zMean;
  float scale;
  float trans[3], rvec[4];
  double COPx, COPy, COPz;
  float DeltaWOld, DeltaHOld, DeltaW, DeltaH;
  double maxDiff, ang;
  colorType colorTable[4];
  double delayTime;
  long window;
  menuType * menu;
  menuType * menu2;
  
} visualType;

extern visualType * dummyVisual ();
extern visualType * stdt2dVisual ();

/*---------------------------------------------------------------------------*/

#define INTERRUPT_ALGORITHM do {if (GLOBAL_INTERRUPT_ALGORITHM == 1) \
                                  goto INTERRUPT_LABEL;} while (0)
#define SHOULD_INTERRUPT_ALGORITHM (GLOBAL_INTERRUPT_ALGORITHM == 1)
#define DONT_INTERRUPT_ALGORITHM do{GLOBAL_INTERRUPT_ALGORITHM = 0;}while(0)

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

