/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

/* the following is the type definition for a persistent quadedge */
/* datastructure. i will assume the convention that the 'time' of the */
/* first unused pointer is MARKED_LAST and that 'additional' points to NULL */
/* unless this pointer points to an 'additionalNextRecord'. Also */
/* nxt[0].time can never be MARKED_LAST. An edge which gets deleted at time t */
 /* has time equal to t and nxt equal to MARKED_DELETED. */

#define NOF_NEXT 4
#define NOF_NEXT_ADDITIONAL 4
#define MARKED_LAST ((indexType) -67748445)
#define MARKED_DELETED ((indexType) -32452435)
#define TIME_0 ((indexType) 0)
#define TIME_NOW ((indexType) (PQ->time))

/*---------------------------------------------------------------------------*/

typedef struct {

  indexType edge; /* pointer to the edgeindex */
  indexType time; /* time of creation of this pointer */

} nextRecordType;

/*---------------------------------------------------------------------------*/

typedef struct theAdditionalNextRecordsType {

  nextRecordType                         nxt[NOF_NEXT_ADDITIONAL];
  struct theAdditionalNextRecordsType * additional;

} additionalNextRecordsType;

/*---------------------------------------------------------------------------*/

typedef struct {

  indexType                   org;
  nextRecordType              nxt[NOF_NEXT];
  additionalNextRecordsType * additional;
  
} persistQeListType;

/*---------------------------------------------------------------------------*/

typedef struct {

  persistQeListType *l;
  indexType max;
  indexType n;
  indexType time; /* this is actually a count of the number of */
                  /* quadedge changes performed (used for persistant) */
  int magic;

} persistQeType;

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

extern indexType pQNBR (/*qe*/);
extern indexType pMAKEQE (/*e*/);
extern indexType pQETOE (/*qe*/);
extern indexType pQETOQE0 (/*qe*/);

extern indexType pSYM (/*qe*/);
extern indexType pORG (/*g, qe*/);
extern indexType pDST (/*g, qe*/);

extern void pSETORG (/*g, qe, org*/);
extern void pSETDST (/*g, qe, dst*/);

extern indexType pROT1 (/*qe*/);
extern indexType pROT3 (/*qe*/);

extern indexType pONEXT (/*g, qe*/);
extern indexType pOPREV (/*g, qe*/);

extern void pSETONEXT (/*g, qe, next*/);

extern indexType pLNEXT (/*g, qe*/);
extern indexType pRNEXT (/*g, qe*/);

extern indexType pLEFTFACET (/*g, qe*/);
extern void pSETLEFTFACET (/*g, qe, facet*/);


extern int pISCHQE (/*g, qe*/);
extern void pSETCHQE (/*g, qe*/);
extern void pUNSETCHQE (/*g, qe*/);

extern int pISCONSTRQE (/*g, qe*/);
extern void pSETCONSTRQE (/*g, qe*/);
extern void pUNSETCONSTRQE (/*g, qe*/);

extern int pISFINALQE (/*g, qe*/);
extern void pSETFINALQE (/*g, qe*/);
extern void pUNSETFINALQE (/*g, qe*/);


extern int pISDELETEDQE (/*g, qe*/);
extern void pSETDELETEDQE (/*g, qe*/);
extern void pUNSETDELETEDQE (/*g, qe*/);

extern int pISCHedge (/*g, edge*/);
extern void pSETCHedge (/*g, edge*/);
extern void pUNSETCHedge (/*g, edge*/);

extern int pISCONSTRedge (/*g, edge*/);
extern void pSETCONSTRedge (/*g, edge*/);
extern void pUNSETCONSTRedge (/*g, edge*/);

extern int pISFINALedge (/*g, edge*/);
extern void pSETFINALedge (/*g, edge*/);
extern void pUNSETFINALedge (/*g, edge*/);

extern int pISDELETEDedge (/*g, edge*/);
extern void pSETDELETEDedge (/*g, edge*/);
extern void pUNSETDELETEDedge (/*g, edge*/);


extern indexType	pqeAddSiteSite ();
extern indexType	pqeAddEdgeSite ();
extern indexType	qeAddEdgeEdge ();
extern indexType	pqeFlip ();
extern void             pqeDelete ();

extern void             pqeGetFuture ();
extern void             pqeGetPresent ();
extern int              pqeISNEW ();
extern int              pqeISDEAD ();
extern int              pqeISLASTENTRY ();
extern indexType        pqeAddTriangleStar ();
extern void             pqeDeleteTriangleStar ();
extern void *           pqeNew ();
