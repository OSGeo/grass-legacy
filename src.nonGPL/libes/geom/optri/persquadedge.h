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

/* persquadedge.c */
additionalNextRecordsType *makeNewNextRecord(void);
indexType pNPE(void *);
void *pqeNew(indexType);
indexType pMAKEQE(indexType);
indexType pQETOE(indexType);
indexType pSYM(indexType);
indexType pORG(void *, indexType);
indexType pDST(void *, indexType);
void pSETORG(void *, indexType, int);
void pSETDST(void *, indexType, int);
int pqeISLASTENTRY(nextRecordType *, indexType, indexType, additionalNextRecordsType *);
int pqeISDEADslow(void *, indexType, indexType);
int pqeISDEAD(nextRecordType *, indexType, indexType, additionalNextRecordsType **);
int pqeISNEW(void *, indexType, indexType);
void pqeGetPresent(void *, indexType, indexType, nextRecordType **, indexType *, indexType *, additionalNextRecordsType ***);
int pqeHasFuture(nextRecordType *, indexType, indexType, additionalNextRecordsType **);
void pqeGetFuture(nextRecordType *, indexType, indexType, additionalNextRecordsType **, nextRecordType **, indexType *, indexType *, additionalNextRecordsType ***);
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
void printPersistQe(persistQeListType *, indexType);
void printPersistQe2(nextRecordType *, indexType, indexType, additionalNextRecordsType *);
void printPQ(void *);
void printPqeStats(void *);
void pqeDispose(void *);
indexType pTimeNow(void *);
indexType pTime0(void *);
