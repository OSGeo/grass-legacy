#include "internoptri.h"
#include "persquadedge.h"

/*--------------------------------------------------------------------------*/

#define PQEMAGIC 1345435

/*--------------------------------------------------------------------------*/

static indexType pROT1 (indexType);
static indexType pROT3 (indexType);

#ifdef TESTMAGIC

  static persistQeType *pqeMagic (void *pq)

  {
    int (*X) ();

    X = NULL;

    if (((persistQeType *) pq)->magic != PQEMAGIC) {
      fprintf (stdout,"ERROR: pqeMagic: wrong magic.\n");
      X ();
      exit (1);
    }

    return (persistQeType *) pq;
  }

#else

#define pqeMagic(pq) ((persistQeType *) pq)

#endif


#define PQ (pqeMagic(pq))
#define PQL (PQ->l)

/*-------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

additionalNextRecordsType *makeNewNextRecord (void)

{
  additionalNextRecordsType * tmp;

  tmp = MALLOC (additionalNextRecordsType, 1);

  tmp->additional = NULL;

  tmp->nxt[0].time = MARKED_LAST;

  return tmp;
}

/*-------------------------------------------------------------------------*/

indexType pNPE (void *pq)
{
  return PQ->n;
}

/*-------------------------------------------------------------------------*/

static void pSETONEXTintern (void *pq, indexType qe,indexType next)

{
  nextRecordType *nxt;
  indexType index;
  indexType maxEntries;
  additionalNextRecordsType **additional;

  pqeGetPresent (pq, qe, TIME_NOW, &nxt, &index, &maxEntries, &additional);

/*  fprintf (stdout,"pSETONEXTintern: %d %d %d %d\n", qe, next, index, maxEntries);*/

  if (nxt[index].time == PQ->time)
    nxt[index].edge = next;
  else
    if (nxt[index].time == MARKED_LAST) {
      nxt[index].edge = next;
      nxt[index].time = PQ->time;
      if (index <  maxEntries - 1)
        nxt[index + 1].time = MARKED_LAST;
    } else
      if (index < maxEntries - 1) {
	nxt[index + 1].edge = next;
	nxt[index + 1].time = PQ->time;
	if (index < maxEntries - 2)
	  nxt[index + 2].time = MARKED_LAST;
      } else {
	*additional = makeNewNextRecord ();
	(*additional)->nxt[0].edge = next;
	(*additional)->nxt[0].time = PQ->time;
	if (1 != NOF_NEXT_ADDITIONAL)
	  (*additional)->nxt[1].time = MARKED_LAST;
      }
}

/*-------------------------------------------------------------------------*/

static void pMarkDeletedEdgeIntern (void *pq, indexType edge)

{
  indexType qe;

  qe = pMAKEQE (edge);
  pSETONEXTintern (pq, qe, MARKED_DELETED);
  pSETONEXTintern (pq, pSYM(qe), MARKED_DELETED);
  pSETONEXTintern (pq, pROT1(qe), MARKED_DELETED);
  pSETONEXTintern (pq, pROT3(qe), MARKED_DELETED);

}

/*-------------------------------------------------------------------------*/

static indexType MakeNewEdge (void *pq) 
{
  indexType qe;

  if (PQ->n == PQ->max) {
    REALLOC (PQ->l, persistQeListType, 
	     (unsigned int) (1.2 * (float) PQ->max) * 4);
    
    PQ->max *= 1.2;
  }

  qe = pMAKEQE (PQ->n);
  PQL[qe].nxt[0].time = MARKED_LAST;
  PQL[qe].additional = NULL;

  qe = pSYM (qe);
  PQL[qe].nxt[0].time = MARKED_LAST;
  PQL[qe].additional = NULL;


  qe = pROT1 (qe);
  PQL[qe].nxt[0].time = MARKED_LAST;
  PQL[qe].additional = NULL;

  qe = pSYM (qe);
  PQL[qe].nxt[0].time = MARKED_LAST;
  PQL[qe].additional = NULL;

  PQ->n += 1;
  return pMAKEQE (PQ->n - 1);
}

/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/

static indexType pqeAddSiteSiteIntern ( void *pq,
     indexType s1,indexType s2)	/* the two sites where the edge will be added */

/*   grAddSiteSite adds an edge from site s1 to site s2, assuming that */
/*   neither site already has an edge connected to it. */ 

{
  indexType qenew;

  qenew = MakeNewEdge (pq);

  if (((s1 == 46) && (s2 == 108)) || ((s1 == 108) && (s2 == 46)))
    fprintf (stdout,"pqeAddSiteSiteIntern %d\n", qenew);

  pSETORG (pq, qenew, s1);
  pSETDST (pq, qenew, s2);

  pSETONEXTintern (pq, qenew, qenew);
  pSETONEXTintern (pq, qenew +1, pROT3 (qenew));
  pSETONEXTintern (pq, qenew +2, pSYM (qenew));
  pSETONEXTintern (pq, qenew +3, pROT1 (qenew));

  return (qenew);
}

/*-------------------------------------------------------------------------*/

static indexType pqeAddEdgeSiteIntern (

     void *pq,
     indexType a,		/* quadedge pointing to origin of new site */
     indexType s)		/* site which will be new edge's destination */
/*
 *   grAddSite adds an edge from the Dest of edge "a" to site s.  It
 * is assumed that site s has no other edges connected to it.  Furthermore,
 * the following equalities must hold: aRight == qenewRight and 
 * aRnext == qenewOnext, where qenew is the new edge added.
 *
 *  aRnext \
 *          *- - - - ->* s
 *          ^   qenew
 *          | a
 */
{
  indexType qenew, aRnext;

  aRnext = pONEXT (pq, pSYM (a), (indexType) PQ->time - 1);

  qenew = MakeNewEdge (pq);

  pSETORG (pq, qenew, pDST (pq, a));
  pSETORG (pq, pSYM (qenew), s);

  pSETONEXTintern (pq, qenew, aRnext);
  pSETONEXTintern (pq, qenew +1, pROT1 (a));
  pSETONEXTintern (pq, qenew +2, pSYM (qenew));
  pSETONEXTintern (pq, qenew +3, pROT1 (qenew));

  pSETONEXTintern (pq, pSYM (a), qenew);
  pSETONEXTintern (pq, pROT1 (aRnext), pROT3 (qenew));

  return (qenew);
}

/*-------------------------------------------------------------------------*/

static indexType pqeAddEdgeEdgeIntern ( void *pq, indexType a,indexType b)

/*
 *   grAddEdgeEdge adds an edge to between the site pointed to by edge
 * a and the site which is the origin of edge b.  The following equalities
 * must to hold: aRight == qenewRight and qenewLeft == bLeft.
 *
 *    \ aRnext       ^
 *     \    qenew    | b
 *      *- - - - - ->*
 *      ^             \
 *      |a             \ bLprev
 */
{
  indexType qenew, aRnext, bLprev;

  aRnext = pONEXT (pq, pSYM (a), (indexType) PQ->time - 1);
  bLprev = pSYM (pONEXT (pq, b, (indexType) PQ->time - 1));

  qenew = MakeNewEdge (pq);

  pSETORG (pq, qenew, pDST (pq, a));
  pSETORG (pq, pSYM (qenew), pORG (pq, b));

  pSETONEXTintern (pq, qenew, aRnext);
  pSETONEXTintern (pq, qenew +1, pROT1 (a));
  pSETONEXTintern (pq, qenew +2, pSYM (bLprev));
  pSETONEXTintern (pq, qenew +3, pROT3 (b));

  pSETONEXTintern (pq, pSYM (a), qenew);
  pSETONEXTintern (pq, b, pSYM (qenew));
  pSETONEXTintern (pq, pROT1 (aRnext), pROT3 (qenew));
  pSETONEXTintern (pq, pROT3 (bLprev), pROT1 (qenew));

  return (qenew);
} 

/*-------------------------------------------------------------------------*/

static indexType pqeFlipIntern (void *pq, indexType qe)

/*            *                          *
 * eSymOprev /^\ eSymOnext             /   \
 *          / | \                     / e'  \
 *         * e|  *          ===>     *<------*
 *   eOnext \ | / eOprev              \     /
 *           \|/                       \   /
 *            *                          *
 */

/* there is an important differnce between this version and the non */
/* persistent version: here we cannot use 'e' for the flipped edge, */
/* since e still has to remain to exist for older versions, so we have */
/* to make a new edge e' */

{
  indexType eOnext, eOprev, eSymOnext, eSymOprev;
  indexType qenew, time;

  time = PQ->time - 1;

  /* save neighboring edges in original configuration (in this */
  /* persistant version this is only a speed-up) */
  eOnext = pONEXT (pq, qe, time);
  eOprev = pOPREV (pq, qe, time);
  eSymOnext = pONEXT (pq, pSYM (qe), time);
  eSymOprev = pOPREV (pq, pSYM (qe), time);
  
  qenew = MakeNewEdge (pq);

  /* set delete time for qe */
  pMarkDeletedEdgeIntern (pq, pQETOE (qe));

  /* set sites to new values */
  pSETORG (pq, qenew, pDST (pq, pONEXT (pq, pSYM (qe), time)));
  pSETORG (pq, pSYM (qenew), pDST (pq, pONEXT (pq, qe, time)));

  /* set nxt pointers of the flipped edge */
  pSETONEXTintern (pq, qenew, pSYM (eOprev));
  pSETONEXTintern (pq, pROT1 (qenew), pROT1 (eSymOnext));
  pSETONEXTintern (pq, pSYM (qenew), pSYM (eSymOprev));
  pSETONEXTintern (pq, pROT3 (qenew), pROT1 (eOnext));

  /* Update pointers of neighboring edges. */
  pSETONEXTintern (pq, pROT1 (eOnext), pROT3 (eOprev));
  pSETONEXTintern (pq, pSYM (eOnext), pSYM (qenew));

  pSETONEXTintern (pq, eOprev, eOnext);
  pSETONEXTintern (pq, pROT3 (eOprev), pROT3 (qenew));

  pSETONEXTintern (pq, pROT1 (eSymOnext), pROT3 (eSymOprev));
  pSETONEXTintern (pq, pSYM (eSymOnext), qenew);

  pSETONEXTintern (pq, eSymOprev, eSymOnext);
  pSETONEXTintern (pq, pROT3 (eSymOprev), pROT1 (qenew));

  return qenew;
}

/*-------------------------------------------------------------------------*/

static void pqeDeleteIntern (

     void *pq,
     indexType qe)

/*            *                         *
 * eSymOprev /^\ eSymOnext             / \
 *          / | \                     /   \
 *         * e|  *          ===>     *     * 
 *   eOnext \ | / eOprev              \   /  
 *           \|/                       \ /   
 *            *                         *    
 */

{
  indexType eOnext, eOprev, eSymOnext, eSymOprev;
  indexType time;

  time = PQ->time - 1;

  /* set delete time for qe */
  pMarkDeletedEdgeIntern (pq, pQETOE (qe));

  /* save neighboring edges in original configuration */
  eOnext = pONEXT(pq, qe, time);
  eOprev = pOPREV(pq, qe, time);
  eSymOnext = pONEXT(pq, pSYM(qe), time);
  eSymOprev = pOPREV(pq, pSYM(qe), time);

  /* Update pointers of neighboring edges. */
  pSETONEXTintern (pq, pROT1(eOnext), pROT3(eOprev));
  pSETONEXTintern (pq, eOprev, eOnext);
  pSETONEXTintern (pq, pROT1(eSymOnext), pROT3(eSymOprev));
  pSETONEXTintern (pq, eSymOprev, eSymOnext);
}

/*-------------------------------------------------------------------------*/

static indexType pqeAddTriangleStarIntern (

     void *pq,
     indexType qe,
     indexType p)

/*                 qe                              qe
             *<------------*               a *<------------* b
              \           /                   \ \       / /     
               \    p    /                     \  \ p /  /      
                \   *   /         ====>         \   *   /       
              e  \     / f                     e \  |  / f       
                  \   /                           \ | /         
                   \ /                             \|/          
                    *                               * c   

    where initially f == pONEXT (qe) and e == pOPREV (pSYM (qe)) and
                    e == pSYM (pONEXT (pSYM (f))).
*/

/* the return value is the newly inserted quadedge from the origin of */
/* qe to p. */

{
  indexType qeSym, bp, pa;

  qeSym = pSYM (qe);

  /* insert quadedge b -> p */
  bp = pqeAddEdgeSiteIntern (pq, qeSym, p);
  
  /* insert quadedge p -> a */
  pa = pqeAddEdgeEdgeIntern (pq, bp, pOPREV (pq, qeSym, PQ->time - 1));
  
  /* insert quadedge c -> p */
  (void) pqeAddEdgeEdgeIntern (pq, pONEXT (pq, bp, PQ->time), pa);

  return bp;
}

/*-------------------------------------------------------------------------*/

static void 
pqeDeleteTriangleStarIntern (void *pq, indexType qe)

/*                                          
           qe                                qe                      
   a *<------------* b                 *<------------*                
      \ \       / /                     \           /                 
       \  \ p /  /                       \    p    /    
        \   *   /          ====>          \   *   /                    
       e \  |  / f                      e  \     / f                  
          \ | /                             \   /                     
           \|/                               \ /                     
            * c        	                      *         
*/

{
  indexType bp;
  indexType time;
  indexType eOnext, eSymOnext, eSymOprev, eOPPOSITE;

  time = PQ->time - 1;
  bp = pONEXT (pq, qe, time);

  /* save neighboring edges in original configuration */
  eOnext = pONEXT(pq, bp, time);
  eSymOnext = pONEXT(pq, pSYM(bp), time);
  eSymOprev = pOPREV(pq, pSYM(bp), time);
  eOPPOSITE = pSYM (pONEXT (pq, pONEXT (pq, pSYM (eOnext), time), time));

  /* delete edges incident to p */
  pMarkDeletedEdgeIntern (pq, pQETOE (bp));
  pMarkDeletedEdgeIntern (pq, pQETOE (eSymOnext));
  pMarkDeletedEdgeIntern (pq, pQETOE (eSymOprev));

  /* Update pointers of neighboring edges. */
  pSETONEXTintern (pq, pROT1(eOnext), pROT3(qe));
  pSETONEXTintern (pq, pSYM(eOnext), pSYM (eOPPOSITE));
  pSETONEXTintern (pq, pROT3(qe), pROT3 (eOPPOSITE));
  pSETONEXTintern (pq, qe, eOnext);
  pSETONEXTintern (pq, pROT3 (eOPPOSITE), pROT1(eOnext));
  pSETONEXTintern (pq, eOPPOSITE, pSYM (qe));
}

/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/

/* EXPORTED FUNCTIONS */
/* all exported functions which modify the quadedge datastructure */
/* increase the time by 1 unit. i.e also if an external function inserts */
/* several edges the time is only increased by 1 unit. */

/*-------------------------------------------------------------------------*/

void *
pqeNew (indexType n)

{
  persistQeType * tmp;

  tmp = MALLOC (persistQeType, 1);
  tmp->max = (float) n * (float) (9.0 / 4.0);
  tmp->n = 0;
  tmp->time = TIME_0;
  tmp->magic = PQEMAGIC;

  tmp->l = MALLOC (persistQeListType, n * 9);  /* n*9 works ok */

  return (void *) tmp;
}

/*-------------------------------------------------------------------------*/

static indexType pQNBR (indexType qe)

/* QNBR takes quadedge index and returns 0..3, which of 4 parts qe refers to */

{
  return qe & 3;
}
/*-------------------------------------------------------------------------*/

indexType pMAKEQE (indexType e)

/* pMAKEQE takes an edge index and returns quadedge index */

{
  return e << 2;
}

/*-------------------------------------------------------------------------*/

indexType 
pQETOE (indexType qe)

/* QETOE takes a quadedge index and returns the edge number */

{
  return qe >> 2;
}

/*-------------------------------------------------------------------------*/

static indexType pQETOQE0 (indexType qe)

/* QETOQE0 takes a quadedge index and returns qe index with no rotation */

{
  return (qe | 3) ^ 3;
}

/*-------------------------------------------------------------------------*/

indexType pSYM (indexType qe)

{
  return qe ^ 2;
}

/*-------------------------------------------------------------------------*/

#define MOD4(n) ((n) & 3)

static indexType pROT1 (indexType qe)

{
  return pQETOQE0 (qe) + MOD4 (pQNBR (qe) + 1);
}

/*-------------------------------------------------------------------------*/

static indexType pROT3 (indexType qe)

{
  return pQETOQE0 (qe) + MOD4 (pQNBR (qe) + 3);
}

/*-------------------------------------------------------------------------*/

indexType 
pORG (void *pq, indexType qe)

{
  return PQL[qe].org;
}

/*-------------------------------------------------------------------------*/

indexType 
pDST (void *pq, indexType qe)

{
  return pORG (pq, pSYM (qe));
}

/*-------------------------------------------------------------------------*/

void 
pSETORG (void *pq, indexType qe, int org)

{
  PQL[qe].org = org;
}

/*-------------------------------------------------------------------------*/

void 
pSETDST (void *pq, indexType qe, int org)

{
  pSETORG (pq, pSYM (qe), org);
}

/*-------------------------------------------------------------------------*/

int 
pqeISLASTENTRY (nextRecordType *nxt, indexType index, indexType maxEntries, additionalNextRecordsType *additional)

/* returns true iff there is no further entry after nxt[index] */

{
  void (*x) () = NULL;
  if ((nxt[index].time != MARKED_LAST) && 
      (nxt[index].edge == MARKED_DELETED)) {
    fprintf (stdout,"ERROR: pqeISLASTENTRY: not defined for deleted edge.\n");
    (*x)();
    exit (1);
  }
  
  return (((index < maxEntries - 1) && (nxt[index + 1].time == MARKED_LAST)) ||
	  ((index == maxEntries - 1) && (additional == NULL)));
}

/*-------------------------------------------------------------------------*/

int pqeISDEADslow (void *pq, indexType qe, indexType time)

{
  nextRecordType *nxt;
  indexType index;
  indexType maxEntries;
  additionalNextRecordsType **additional;

  pqeGetPresent (pq, qe, time, &nxt, &index, &maxEntries, &additional);
  return pqeISDEAD (nxt, index, maxEntries, additional);
}

/*-------------------------------------------------------------------------*/

int pqeISDEAD (nextRecordType *nxt, indexType index, indexType maxEntries,
  additionalNextRecordsType **additional)

/* returns true iff edge does not exist anymore at the time */
/* nxt[index].time */

{
  return ((nxt[index].time != MARKED_LAST) &&
	  (nxt[index].edge == MARKED_DELETED));
}

/*-------------------------------------------------------------------------*/

int 
pqeISNEW (void *pq, indexType qe, indexType time)

/* returns true iff qe came alife at time 'time' */

{
  return (PQL[qe].nxt[0].time == time);
}

/*-------------------------------------------------------------------------*/

void pqeGetPresent (void *pq, indexType qe, indexType time,
  nextRecordType **nxt, indexType *index, indexType *maxEntries,
  additionalNextRecordsType ***additional)

/* computes all the information necessary to access the next pointer */
/* at time 'time' and the future thereof in constant time. if time == */
/* -1 then the current time is assumed. */

{
  indexType i;
  additionalNextRecordsType * tmp;

  if (time == TIME_NOW)
    time = PQ->time;

  /* a little hack for a little bit more speed */

  if (((NOF_NEXT == 1) && (PQL[qe].additional == NULL)) ||
      ((NOF_NEXT > 1) && 
       ((PQL[qe].nxt[1].time == MARKED_LAST) ||
	(PQL[qe].nxt[1].time > time))) ||
      (PQL[qe].nxt[0].time == MARKED_LAST)) {
    
    *nxt = PQL[qe].nxt;
    *index = 0;
    *maxEntries = NOF_NEXT;
    *additional = &(PQL[qe].additional);
    return;

  } else {

    /* the hope for this hack is that PQL[qe].additional is NULL */
    i = 1;
    while ((i < NOF_NEXT) && (PQL[qe].nxt[i].time <= time) &&
	   (PQL[qe].nxt[i].time != MARKED_LAST)) {
      i++;
    };

    if (i < NOF_NEXT) {
      *nxt = PQL[qe].nxt;
      *index = i - 1;
      *maxEntries = NOF_NEXT;
      *additional = &(PQL[qe].additional);
      return;
    } else 
      if ((PQL[qe].additional == NULL) || 
	  (PQL[qe].additional->nxt[0].time > time)) {
	*nxt = PQL[qe].nxt;
	*index = NOF_NEXT - 1;
	*maxEntries = NOF_NEXT;
	*additional = &(PQL[qe].additional);
	return;
      } else { /* there goes the hope */

/*	printPersistQe (&(PQL[qe]), qe);*/

	tmp = PQL[qe].additional;
	while ((tmp->additional != NULL) &&
	       (tmp->additional->nxt[0].time <= time)) {
	  tmp = tmp->additional;
	}
	
	i = 1;
	while ((i < NOF_NEXT_ADDITIONAL) && (tmp->nxt[i].time <= time) &&
	       (tmp->nxt[i].time != MARKED_LAST)) {
	  i++;
	};
	
	*nxt = tmp->nxt;
	*index = i - 1;
	*maxEntries = NOF_NEXT_ADDITIONAL;
	*additional = &(tmp->additional);
	return;
      }
  }
}
  
/*-------------------------------------------------------------------------*/

int 
pqeHasFuture (nextRecordType *nxt, indexType index, indexType maxEntries, additionalNextRecordsType **additional)

{
  if (index == maxEntries - 1) 
    if ((*additional != NULL) && (nxt[index].edge != MARKED_DELETED))
      return 1;
    else
      return 0;
  else
    if ((nxt[index + 1].time != MARKED_LAST) && 
	(nxt[index].edge != MARKED_DELETED))
      return 1;
    else
      return 0;
}

/*-------------------------------------------------------------------------*/

void pqeGetFuture (nextRecordType *nxt, indexType index, indexType maxEntries,
  additionalNextRecordsType **additional,
  nextRecordType **futureNxt, indexType *futureIndex,
  indexType *futureMaxEntries,
  additionalNextRecordsType ***futureAdditional)

/* computes the future from the information provided by function */
/* pqeGetPresent. Future is defined as the next change in the nxt */
/* field of an quadedge. this is not necessarily the time immediately */
/* following the present */

{
  if (index == maxEntries - 1)
    if ((*additional != NULL) && (nxt[index].edge != MARKED_DELETED)) {
      *futureNxt = (*additional)->nxt;
      *futureIndex = 0;
      *futureMaxEntries = NOF_NEXT_ADDITIONAL;
      *futureAdditional = &((*additional)->additional);
    } else {
      fprintf (stdout,"ERROR: pqeGetFuture 1: edge %p has no future.\n", nxt);
      exit (1);
    }
  else 
    if ((nxt[index + 1].time != MARKED_LAST) && 
	(nxt[index].edge != MARKED_DELETED)) {
      *futureNxt = nxt;
      *futureIndex = index + 1;
      *futureMaxEntries = maxEntries;
      *futureAdditional = additional;
    } else {
      fprintf (stdout,"ERROR: pqeGetFuture 2: edge %p has no future.\n", nxt);
      exit (1);
    }
}
    
/*-------------------------------------------------------------------------*/

indexType pONEXT (void *pq, indexType qe, indexType time)

{
  nextRecordType *nxt;
  indexType index;
  indexType maxEntries;
  additionalNextRecordsType **additional;

  pqeGetPresent (pq, qe, time, &nxt, &index, &maxEntries, &additional);

  return nxt[index].edge;
}

/*-------------------------------------------------------------------------*/

indexType pOPREV (void *pq, indexType qe, indexType time)

{
  return pROT1 (pONEXT (pq, pROT1 (qe), time));
}

/*-------------------------------------------------------------------------*/

void 
pSETONEXT (void *pq, indexType qe, indexType next)

{
  (PQ->time)++;
  pSETONEXTintern (pq, qe, next);
}

/*-------------------------------------------------------------------------*/

indexType 
pqeAddSiteSite (
    void *pq,
    indexType s1,
    indexType s2	/* the two sites where the edge will be added */
)
     
{
  (PQ->time)++;
  return pqeAddSiteSiteIntern (pq, s1, s2);
}
  
/*-------------------------------------------------------------------------*/

indexType 
pqeAddEdgeSite (void *pq, indexType a, indexType s)

{
  (PQ->time)++;
  return pqeAddEdgeSiteIntern (pq, a, s);
}

/*-------------------------------------------------------------------------*/

indexType 
pqeAddEdgeEdge (void *pq, indexType a, indexType b)

{
  (PQ->time)++;
  return pqeAddEdgeEdgeIntern (pq, a, b);
}

/*-------------------------------------------------------------------------*/

indexType 
pqeFlip (void *pq, indexType qe)

{
  (PQ->time)++;
  return pqeFlipIntern (pq, qe);
}

/*-------------------------------------------------------------------------*/

void 
pqeDelete (void *pq, indexType qe)

{
  (PQ->time)++;
  pqeDeleteIntern (pq, qe);
}

/*-------------------------------------------------------------------------*/

indexType 
pqeAddTriangleStar (void *pq, indexType qe, indexType p)

{
  (PQ->time)++;
  return pqeAddTriangleStarIntern (pq, qe, p);
}

/*-------------------------------------------------------------------------*/

void 
pqeDeleteTriangleStar (void *pq, indexType qe)

{
  (PQ->time)++;
  pqeDeleteTriangleStarIntern (pq, qe);
}

/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/

int 
pISCHQE (void *pq, indexType qe, indexType time)

/* ISCHQE returns 1 iff qe is convex hull edge */

{
  return 
    (pORG (pq, qe) >= 0) && (pDST (pq, qe) >= 0) &&
    ((pDST (pq, pONEXT (pq, qe, time)) < 0) ||
    (pDST (pq, pOPREV (pq, qe, time)) < 0));
}

/*-------------------------------------------------------------------------*/

int 
pISCHedge (void *pq, indexType edge, indexType time)

/* ISCHedge returns 1 iff edge is convex hull edge */

{
  return pISCHQE (pq, pMAKEQE (edge), time);
}

/*-------------------------------------------------------------------------*/

int 
pqeExistsAtTime (void *pq, indexType pqe, indexType time)

{
  indexType t;

  nextRecordType *nxt, *futureNxt;
  indexType index, futureIndex;
  indexType maxEntries, futureMaxEntries;
  additionalNextRecordsType **additional, **futureAdditional;

  t = PQL[pqe].nxt[0].time;
  
  if (t > time)
    return 0;
  if (t == time)
    return 1;

  pqeGetPresent (pq, pqe, time, &nxt, &index, &maxEntries, &additional);
  return ! pqeISDEAD (nxt, index, maxEntries, additional);  
}

/*-------------------------------------------------------------------------*/

static int 
countNofNext (persistQeListType *pql)

{
  int count;
  additionalNextRecordsType * anr;
  indexType i;

  count = 0;
  
  i = 0;
  while ((i < NOF_NEXT) && (pql->nxt[i].time != MARKED_LAST)) {
    count++;
    i++;
  }

  anr = pql->additional;

  while (anr != NULL) {

    i = 0;
    while ((i < NOF_NEXT_ADDITIONAL) && (anr->nxt[i].time != MARKED_LAST)) {
      count++;
      i++;
    }
    anr = anr->additional;
  }

  return count;
}

/*-------------------------------------------------------------------------*/

void printPersistQe (persistQeListType *pql, indexType index)

{
  additionalNextRecordsType * anr;
  indexType i;

  fprintf (stdout,"\npq (%d): org = %d, additional = %p\n", index, pql->org, pql->additional);
  
  i = 0;
  while ((i < NOF_NEXT) && (pql->nxt[i].time != MARKED_LAST)) {
    fprintf (stdout,"         time: %d, edge: %d\n", pql->nxt[i].time, pql->nxt[i].edge);
    i++;
  }

  if ((i < NOF_NEXT) && (pql->nxt[i].time == MARKED_LAST))
    fprintf (stdout,"         time: last, edge: %d\n", pql->nxt[i].edge);

  fprintf (stdout,"\n");

  anr = pql->additional;

  while (anr != NULL) {
    fprintf (stdout,"  anr->additional = %p\n", anr->additional);
    i = 0;
    while ((i < NOF_NEXT_ADDITIONAL) && (anr->nxt[i].time != MARKED_LAST)) {
      fprintf (stdout,"         time: %d, edge: %d\n", anr->nxt[i].time,
	      anr->nxt[i].edge);
      i++;
    }
    if ((i < NOF_NEXT_ADDITIONAL) && (anr->nxt[i].time == MARKED_LAST))
      fprintf (stdout,"         time: last, edge: %d\n", anr->nxt[i].edge);
    anr = anr->additional;
  }
}

/*-------------------------------------------------------------------------*/

void 
printPersistQe2 (nextRecordType *nxt, indexType index, indexType maxEntries, additionalNextRecordsType *additional)


{
  additionalNextRecordsType * anr;
  indexType i;

  fprintf (stdout,"\nFast: index = %d, maxEntries = %d\n", index, maxEntries);
  
  i = 0;
  while ((i <= maxEntries) && (nxt[i].time != MARKED_LAST)) {
    fprintf (stdout,"         time: %d, edge: %d\n", nxt[i].time, nxt[i].edge);
    i++;
  }

  if ((i < maxEntries) && (nxt[i].time == MARKED_LAST))
    fprintf (stdout,"         time: last, edge: %d\n", nxt[i].edge);

  fprintf (stdout,"\n");

  anr = additional;

  while (anr != NULL) {
    fprintf (stdout,"  anr->additional = %p\n", additional);
    i = 0;
    while ((i <= NOF_NEXT_ADDITIONAL) && (anr->nxt[i].time != MARKED_LAST)) {
      fprintf (stdout,"         time: %d, edge: %d\n", anr->nxt[i].time,
	      anr->nxt[i].edge);
      i++;
    }
    if ((i < NOF_NEXT_ADDITIONAL) && (anr->nxt[i].time == MARKED_LAST))
      fprintf (stdout,"         time: last, edge: %d\n", anr->nxt[i].edge);
    anr = anr->additional;
  }
}

/*-------------------------------------------------------------------------*/

void 
printPQ (void *pq)

{
  indexType index;

  fprintf (stdout,"\n---------------------\n");

  for (index = 0; index < PQ->n * 2; index++) {
    printPersistQe (&(PQL[index * 2]), index * 2);
  }
}

/*-------------------------------------------------------------------------*/

void 
printPqeStats (void *pq)

{
 int count[26];
 int i, c;

 for (i = 0; i < 26; i++)
   count[i] = 0;

 for (i = 0; i < PQ->n * 4; i++)
   if ((c = countNofNext (&(PQL[i]))) <= 25)
     count [c] += 1;
   else
     count [0] += 1;

 fprintf (stdout,"Next Pointer Length Distribution:\n\n");

 for (i = 1; i <= 13; i++)
   fprintf (stdout,"%6d", i);
 fprintf (stdout,"\n");
 for (i = 1; i <= 13; i++)
   fprintf (stdout,"%6d", count[i]);
 fprintf (stdout,"\n\n");

 for (i = 14; i <= 25; i++)
   fprintf (stdout,"%6d", i);
   fprintf (stdout,"     >\n");
 for (i = 14; i <= 25; i++)
   fprintf (stdout,"%6d", count[i]);
   fprintf (stdout,"%6d", count[0]);
 fprintf (stdout,"\n");
 fprintf (stdout,"\n");
}
/*-------------------------------------------------------------------------*/

void 
pqeDispose (void *pq)

{
  int i;
  additionalNextRecordsType * additional, * tmp;

  for (i = 0; i < PQ->n; i++) {
    additional = PQL[i].additional;
    while (additional != NULL) {
      tmp = additional->additional;
      FREE (additional);
      additional = tmp;
    }
  }

  FREE (PQ->l);
  FREE (pq);
}

/*-------------------------------------------------------------------------*/

indexType 
pTimeNow (void *pq)

{
  return PQ->time;
}
/*-------------------------------------------------------------------------*/

indexType 
pTime0 (void *pq)

{
  return TIME_0;
}


/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/
