/*--------------------------------------------------------------------------*/

#include "geom/optri.h"
#include "geom/basic.h"

/*--------------------------------------------------------------------------*/

#ifdef _NO_PROTOS
/* flips.c */

extern void lawsonFlip ();

/*--------------------------------------------------------------------------*/

/* graph.c */

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
#else
/* Prototypes */
/* graph.c */
void grPrintStatistics(graphType *);
void *grMake(void);
void grQEassign(graphType *, void *);
void *grQE(graphType *);
void grSIassign(graphType *, void *);
void *grSI(graphType *);
void grAllocate(graphType *, int, int, int);
void grResetCHstatus(graphType *);
void grSetCHstatus(graphType *);
void grCleanConvexHull(void *);
void grMakeNonDegenerate(void *);
void grMarkDuplicateSites(void *);
void grRemoveDuplicateSites(void *);
void grReadGraph(char *, graphType *);
void grPrintGraph(char *, graphType *, int, int, double);
void grPrintGraphLong(char *, graphType *, int, int, double);
void grMinMaxAngleTriangulation2(void *);
void grMaxMinHeightTriangulation2(void *);
void grIncrementalDelaunayTriangulation(void *);
void grIncrementalRegularTriangulation(void *);
/* regular.c */
void verifyRegular(graphType *, int);
/* heur.angle.c */
int haComputeAngleQuadrantOnly ( graphType *, indexType ,indexType,indexType);
int haAngleGT180 ( graphType *, indexType ,indexType,indexType);
int haAngleGT ( graphType *, indexType,indexType,indexType, indexType,indexType,indexType);
void haPrintAngle(void);
/* heur.height.c */
int hhHeightLT0(graphType *, indexType, indexType, indexType);
int hhHeightLT(graphType *, indexType, indexType, indexType, indexType, indexType, indexType);
void hhPrintHeight(FILE *, indexType, coordType, coordType, coordType, indexType, coordType, coordType, coordType, indexType, coordType, coordType, coordType);
/* heur.slope.c */
int hsSlopeRightTurn(graphType *, indexType, indexType, indexType);
int hsSlopeGT(graphType *, indexType, indexType, indexType, indexType, indexType, indexType);
void hsPrintSlope(FILE *, indexType, coordType, coordType, coordType, indexType, coordType, coordType, coordType, indexType, coordType, coordType, coordType);
/* hdag.c */
int hdagIsHDAGtype(void *);
void *hdagSI(void *);
void *hdagPQ(void *);
void hdagInsertTopTriangle(void *, indexType, indexType, indexType);
indexType hdagLocatePoint(void *, indexType, int *);
indexType hdagInsertPoint(void *, indexType, int *);
void *hdagNew(indexType, void *,
      int (*)(graphType *, indexType, indexType, indexType, double *),
      int (*)(void));
void hdagDispose(void *);
/* persquadedge.c */
int pqeISDEADslow(void *, indexType, indexType);
int pqeISNEW(void *, indexType, indexType);
indexType pONEXT(void *, indexType, indexType);
indexType pOPREV(void *, indexType, indexType);
void pSETONEXT(void *, indexType, indexType);
indexType pqeAddSiteSite(void *, indexType, indexType);
indexType pqeAddEdgeSite(void *, indexType, indexType);
indexType pqeAddEdgeEdge(void *, indexType, indexType);
indexType pqeFlip(void *, indexType);
void pqeDelete(void *, indexType);
indexType pqeAddTriangleStar(void *, indexType, indexType);
void pqeDeleteTriangleStar(void *, indexType);
int pISCHQE(void *, indexType, indexType);
int pISCHedge(void *, indexType, indexType);
int pqeExistsAtTime(void *, indexType, indexType);
/* 
//void printPersistQe(persistQeListType *, indexType);
//void printPersistQe2(nextRecordType *, indexType, indexType, additionalNextRecordsType *);
*/
void printPQ(void *);
void printPqeStats(void *);
void pqeDispose(void *);
indexType pTimeNow(void *);
indexType pTime0(void *);
/* timer.c */
double get_user_time(void);
double get_time_of_day(void);

/* Prototypes end */
#endif
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
/* flips.c */
void lawsonFlip(graphType *, visualType *, poolType *, int (*)(graphType *,indexType), int (*)(void), int *, int *, double *);
/* angle.c */
void buildMinMaxAngleTriangulation(graphType *, int, visualType *, int *, int *, double *);
void buildMinMaxAngleTriangulation2(graphType *, int, visualType *, int *, int *, double *);
/* height.c */
void buildMaxMinHeightTriangulation(graphType *, int, visualType *, int *, int *, double *);
void buildMaxMinHeightTriangulation2(graphType *, int, visualType *, int *, int *, double *);
/* slope.c */
void buildMinMaxSlopeTriangulation(graphType *, int, visualType *, int *, int *, double *);
/* delaunay.c */
void buildDelaunayTriangulation(graphType *, int, visualType *, int *, int *, double *);
/* regular.c */
void incrementalFlip(graphType *, visualType *, poolType *, int, int (*)(void), int *, int *, double *);
void incrementalDelaunayTriangulation(graphType *, int, visualType *, int *, int *, double *);
void regularTriangulation(graphType *, int, visualType *, int *, int *, double *);
/* planesweep.c */
void planesweepConstrained(graphType *, visualType *, double *);
void buildInitialTriangulation(graphType *, int, visualType *, double *);
/* edgeinsert.c */
void edgeInsertionN3(graphType *, visualType *, poolType *,
     int (*)(graphType *, indexType, indexType, indexType, indexType, indexType, indexType),
     int, int (*)(graphType *, indexType, indexType, indexType),
     void (*)(),
     int *, int *, double *);
void edgeInsertionN2LOGN(graphType *, visualType *, poolType *,
     int (*)(graphType *, indexType, indexType, indexType, indexType, indexType, indexType),
     int, int (*)(graphType *, indexType, indexType, indexType),
     void (*)(),
     int *, int *, double *);
/* heap.c */
void hpPrint(queuesType *);
void hpExc(queuesType *, int, int);
void hpConsistCheck(queuesType *, int, int);
void hpReset(queuesType *);
void hpMoveDownEntry(queuesType *, int);
void hpMoveUpEntry(queuesType *, int);
queuesType *hpNew(int, char *, int (*)(void));
void hpDispose(queuesType *);
poolType *heap(void);
/* novisual.c */
char *noVisualGetFileName(void);
menuType *dummyMakeMainMenu(void);
visualType *dummyVisual(void);
/* pqe2qe.c */
void copyHdagToQe ( void *, indexType, void *);
/* quadedge.c */
int qeIsQEtype(void *);
void qeSETONEXT(void *, indexType, indexType);
void qeSETOPREV(void *, indexType, indexType);
void qeSETORG(void *, indexType, int);
void qeSETDST(void *, indexType, int);
void *qeNewAdvanced(indexType, indexType, indexType *, indexType *);
void *qeNew(indexType);
void qeDispose(void *);
void qeAdjustSize(void *, int);
indexType qeNE(void *);
indexType qeNEmax(void *);
indexType qeNQE(void *);
indexType qeNQEmax(void *);
void qeNEset(void *, indexType);
void qeNEreset(void *);
indexType qeCHQE(void *);
void qeSETchQE(void *, indexType);
indexType qeNCHQE(void *);
void qeSETnChQE(void *, indexType);
indexType qeSYM(indexType);
indexType qeORG(void *, indexType);
indexType qeDST(void *, indexType);
indexType qeONEXT(void *, indexType);
indexType qeOPREV(void *, indexType);
void qeReinsertSiteSite(void *, indexType, indexType, indexType);
indexType qeAddSiteSite(void *, indexType, indexType);
void qeReinsertEdgeSite(void *, indexType, indexType, indexType);
indexType qeAddEdgeSite(void *, indexType, indexType);
void qeReinsertEdgeEdge(void *, indexType, indexType, indexType);
indexType qeAddEdgeEdge(void *, indexType, indexType);
void qeFlip(void *, indexType);
void qeUnlink(void *, indexType);
void qeDeleteEdge(void *, indexType);
void qeDuplicate(void *, void *, indexType);
void qeResetCONSTRstatus(void *);
void qeResetFINALstatus(void *);
void qeResetDELETEDstatus(void *);
void qeResetCHstatus(void *);
void qeResetALLstati(void *);
indexType qeMAKEQE(indexType);
indexType qeQETOE(indexType);
indexType qeQETOQE0(indexType);
int qeISCHQE(void *, indexType);
void qeSETCHQE(void *, indexType);
void qeUNSETCHQE(void *, indexType);
int qeISCONSTRQE(void *, indexType);
void qeSETCONSTRQE(void *, indexType);
void qeUNSETCONSTRQE(void *, indexType);
int qeISFINALQE(void *, indexType);
void qeSETFINALQE(void *, indexType);
void qeUNSETFINALQE(void *, indexType);
int qeISDELETEDQE(void *, indexType);
void qeSETDELETEDQE(void *, indexType);
void qeUNSETDELETEDQE(void *, indexType);
int qeISCHedge(void *, indexType);
void qeSETCHedge(void *, indexType);
void qeUNSETCHedge(void *, indexType);
int qeISCONSTRedge(void *, indexType);
void qeSETCONSTRedge(void *, indexType);
void qeUNSETCONSTRedge(void *, indexType);
int qeISFINALedge(void *, indexType);
void qeSETFINALedge(void *, indexType);
void qeUNSETFINALedge(void *, indexType);
int qeISDELETEDedge(void *, indexType);
void qeSETDELETEDedge(void *, indexType);
void qeUNSETDELETEDedge(void *, indexType);
void qeDeleteMarkedEdges(void *);
int qeMarkDuplicateEdges(void *);
int qeRemoveDuplicateEdges(void *);
void qeNormalize(void *);
void qePrintEdges(void *, FILE *);
void qePrintQEdges(void *, FILE *);
void qeReadEdges(char *, void *);
void qeExamineEdges(char *, int *);
/* queue.c */
void quReset(queuesType *);
queuesType *quNew(int, char *, int);
queuesType *pqStyleQuNew(int, char *, int (*)(void), int);
void quDispose(queuesType *);
void quPrint(queuesType *);
poolType *queue(void);
/* site.c */
int siIsSItype(void *);
void *siNewAdvanced(indexType, indexType, int, int, coordType *, coordType *, coordType *);
void *siNew(indexType, int);
indexType siNS(void *);
void siNSset(void *, int);
indexType siNSmax(void *);
indexType siNUS(void *);
coordType siSITEX(void *, indexType);
coordType siSITEY(void *, indexType);
coordType siSITEZ(void *, indexType);
coordType siMaxX(void *);
coordType siMinX(void *);
coordType siMaxY(void *);
coordType siMinY(void *);
coordType siMaxZ(void *);
coordType siMinZ(void *);
int siIsDuplicateSite(void *, indexType);
void siMarkAsDuplicateSite(void *, indexType);
void siMarkAsNonDuplicateSite(void *, indexType);
void siSITEreload(void *, indexType, coordType, coordType, coordType);
indexType siSITEload(void *, coordType, coordType, coordType);
int siMarkDuplicateSites(void *);
int siRemoveDuplicateSitesExpert(void *, indexType *);
int siRemoveDuplicateSites(void *);
void siPrintSites(void *, FILE *);
void siReadSites(char *, void *);
void siExamineSites(char *, int *, int *, int *);
/* sos.regular.c */
int sosrLeftTurn(graphType *, indexType, indexType, indexType);
int sosrCollinear(graphType *, indexType, indexType, indexType);
int sosrInCircle(graphType *, indexType, indexType, indexType, indexType, double *);
void sosrInit(void);
int sosrInCircleInf(graphType *, indexType, indexType, indexType, indexType, int, double *);
int sosrCocircular(graphType *, indexType, indexType, indexType, indexType);
int sosrLeftTurnInf(graphType *, indexType, indexType, indexType, double *);
int sosrLeftOf(graphType *, indexType, indexType, double *);
int sosrBelow(graphType *, indexType, indexType, double *);
int sosrIdenticalX(graphType *, indexType, indexType);
int sosrIdenticalY(graphType *, indexType, indexType);
int sosrIdenticalXY(graphType *, indexType, indexType);
int sosrLexoCompare(graphType *, indexType, indexType, double *);
int sosrSlopeCompare(graphType *, indexType, indexType, indexType, indexType);
/* triangulation.c */
void saveTriangulation(char *, char *);
void copyCoordinatesToGraph(int, int *, int *, int *, int, char **);
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

