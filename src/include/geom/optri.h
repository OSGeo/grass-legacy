/*-------------------------------------------------------------------------*/

typedef int indexType;

typedef double coordType;
typedef struct {
  void          *s;
  void          *e;
} graphType;

/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/

/*     functions for sites  */

extern void * siNew (/* n */);
extern void * siNewAdvanced (/* n, nofSetPoints, useLIA, floatA, x, y, z */);

extern int siIsSItype (/* s */);

extern indexType siNS (/* s */);
extern indexType siNUS (/* s */);
extern indexType siNSmax (/* s */);
extern void siNSset (/* s, nNew */);

extern coordType siSITEX (/* s, site */), siSITEY (/* s, site */), 
                 siSITEZ (/* s, site */);
extern coordType siMinX(/* s */), siMaxX(/* s */), siMinY(/* s */), 
                 siMaxY(/* s */), siMinZ(/* s */), siMaxZ(/* s */);

extern int siIsDuplicateSite (/* s, site */);
extern void siMarkAsDuplicateSite (/* s, site */);
extern void siMarkAsNonDuplicateSite (/* s, site */);
extern int siMarkDuplicateSites (/* s */);
extern int siRemoveDuplicateSites (/* s */);

extern indexType siSITEload (/* s, x, y, z */);
extern void siSITEreload (/* s, site, x, y, z */);

extern void siReadSites (/* fname, s */);
extern void siExamineSites (/* fname, nofSites, maxW, maxA */);

extern void siPrintSites (/* s, fp */);

/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/

/*     functions for quadedges */

extern void *qeNew (/* n */);
extern void * qeNewAdvanced (/* n, org, dst */);
extern void qeAdjustSize (/* q, nNew */);
extern void qeDispose (/* q */);

extern int qeIsQEtype (/* q */);

extern indexType qeNE (/* q */);
extern indexType qeNEmax (/* q */);
extern indexType qeNQE (/* q */);
extern indexType qeNQEmax (/* q */);
extern void qeNEset (/* q, ne */);
extern void qeNEreset (/* q */);

extern indexType qeMAKEQE (/* e */);
extern indexType qeQETOE (/* qe */);
extern indexType qeQETOQE0 (/* qe */);

extern indexType qeORG (/* q, qe */);
extern indexType qeDST (/* q, qe */);
extern indexType qeONEXT (/* q, qe */);
extern indexType qeOPREV (/* q, qe */);
extern indexType qeSYM (/* qe */);

extern void qeSETORG (/* q, qe, org */);
extern void qeSETDST (/* q, qe, org */);
extern void qeSETONEXT (/* q, qe, next */);
extern void qeSETOPREV (/* q, qe, prev */);

extern void qeFlip (/* q, qe */);

extern indexType qeAddSiteSite (/* q, s1, s2 */);
extern indexType qeAddEdgeSite (/* q, a, s */);
extern indexType qeAddEdgeEdge (/* q, a, b */);

extern void qeUnlink (/* q, qe */);
extern void qeDeleteEdge (/* q, qe */);
extern void qeDeleteMarkeddges (/* q */);
extern int qeRemoveDuplicateEdges (/* q */);
extern void qeReinsertEdgeEdge (/* q, qe, newA, newB */);
extern void qeReinsertSiteSite (/* q, qe, newA, newB */);
extern void qeReinsertEdgeSite (/* q, qe, newA, newS */);

extern void qeDuplicate (/* qSource, qDest, qe */);

extern void qeResetALLstati (/* q */);

extern void qeResetCONSTRstatus (/* q */);
extern int qeISCONSTRQE (/* q, qe */);
extern void qeSETCONSTRQE (/* q, qe */);
extern void qeUNSETCONSTRQE (/* q, qe */);
extern int qeISCONSTRedge (/* q, edge */);
extern void qeSETCONSTRedge (/* q, edge */);
extern void qeUNSETCONSTRedge (/* q, edge */);

extern void qeResetFINALstatus (/* q */);
extern int qeISFINALQE (/* q, qe */);
extern void qeSETFINALQE (/* q, qe */);
extern void qeUNSETFINALQE (/* q, qe */);
extern int qeISFINALedge (/* q, edge */);
extern void qeSETFINALedge (/* q, edge */);
extern void qeUNSETFINALedge (/* q, edge */);

extern void qeResetDELETEDstatus (/* q */);
extern int qeISDELETEDQE (/* q, qe */);
extern void qeSETDELETEDQE (/* q, qe */);
extern void qeUNSETDELETEDQE (/* q, qe */);
extern int qeISDELETEDedge (/* q, edge */);
extern void qeSETDELETEDedge (/* q, edge */);
extern void qeUNSETDELETEDedge (/* q, edge */);

extern void qeResetCHstatus (/* q */);
extern int qeISCHQE (/* q, qe */);
extern void qeSETCHQE (/* q, qe */);
extern void qeUNSETCHQE (/* q, qe */);
extern int qeISCHedge (/* q, edge */);
extern void qeSETCHedge (/* q, edge */);
extern void qeUNSETCHedge (/* q, edge */);
extern indexType qeCHQE (/* q */);
extern void qeSETchQE (/* q,chQE */);
extern indexType qeNCHQE (/* q */);
extern void qeSETnChQE (/* q,nofCHQE */);

extern void qeReadEdges (/* fname, q */);
extern void qeExamineEdges (/* fname, nofEdges */);

extern void qePrintEdges (/* q, fp */);
extern void qePrintQEdges (/* q, fp */);

/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/

/*     functions for graph */

extern void *grMake (/*  */);
extern void *grQE (/* g */);
extern void *grSI (/* g */);

void * grNew (/* nofSites, nofDeci */);
extern void * grNewAdvanced (/* nSiteMax, nofSites, useLIA, nofDeci, X, Y, Z,
				nEdgeMax, nofPrescribedEdges, O, D */);
void grDispose(graphType *);

extern void grQEassign (/* g, q */);
extern void grSIassign (/* g, s */);

extern void grAllocateSites (/* g, maxs, floatA */);
extern void grAllocate (/* g, maxs, maxe, floatA */);

extern void grCleanConvexHull (/* g */);
extern void grMakeNonDegenerate (/* g */);
extern void grRemoveDuplicateSites (/* g */);

extern void grReadGraph (/* fname, g */);
extern void grPrintGraph (/* fname, g, nofFlips, nofSuccFlips, runTime */);
extern void grPrintGraphLong (/* fname, g, nofFlips, nofSuccFlips, runTime */);
extern void grPrintStatistics (/* g */);
void grPlanesweepTriangulation(void *);
void grDelaunayTriangulation(void *);
void grMinMaxAngleTriangulation(void *);
void grMaxMinHeightTriangulation(void *);
void grMinMaxSlopeTriangulation(void *);

/*--------------------------------------------------------------------------*/

/* sos.c */

extern void sosrInit (/*  */); 
extern int sosrLeftOf (/* g, s0, s1, detdbl */);
extern int sosrLeftTurn (/* g, s0, s1, s2, detdbl */); 
extern int sosrCollinear (/* g, s0, s1, s2 */); 
extern int sosrInCircleInf (/* g, s0, s1, s2, s3, doDelaunay, detdbl */); 
extern int sosrLeftTurnInf (/* g, s0, s1, s2, detdbl */); 
extern int sosrInCircle (/* g, s0, s1, s2, s3, detdbl */);
extern int sosrLexoCompare (/* g, s0, s1, detdbl */);
extern int sosrLeftOf (/* g, s0, s1, detdbl */);

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

/* #define TESTMAGIC */

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

