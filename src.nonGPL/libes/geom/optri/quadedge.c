
#include "internoptri.h"

/*--------------------------------------------------------------------------*/

#define QEMAGIC 46436345

/*--------------------------------------------------------------------------*/


/* typedef short int indexType; */
typedef struct {

  unsigned int isCH : 1;
  unsigned int isConstrained : 1;
  unsigned int isFinal : 1;
  unsigned int isDeleted : 1;

} qeStatusType;

typedef struct {

  indexType * org;
  indexType * dst;
  indexType * nxt[2];
  indexType * prv[2];
  qeStatusType * status[2];
  int max, ne;
  indexType chEdge;
  int nChEdges;
  int magic;

} qeType;

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

#ifdef TESTMAGIC

static qeType *qeMagic (void *q)

  {
    int (*X) ();

    X = NULL;

    if (((qeType *) q)->magic != QEMAGIC) {
      fprintf (stdout,"ERROR: qeMagic: wrong magic.\n");
      X ();
      exit (1);
    }

    return (qeType *) q;
  }

#else

#define qeMagic(q) ((qeType *) q)

#endif


#define Q (qeMagic(q))

int qeIsQEtype (void *q)

{
  qeMagic (q);
  return 1;
}

/*--------------------------------------------------------------------------*/

#define QE2E(qe) ((qe) >> 1)
#define E2QE(e) ((e) << 1)
#define QEversion(qe) ((qe) & 1)

/*--------------------------------------------------------------------------*/

void 
qeSETONEXT (void *q, indexType qe, indexType next)

{
  Q->nxt[QEversion (qe)][QE2E (qe)] = next;
}

/*--------------------------------------------------------------------------*/

void 
qeSETOPREV (void *q, indexType qe, indexType prev)

{
  Q->prv[QEversion (qe)][QE2E (qe)] = prev;
}

/*--------------------------------------------------------------------------*/

void 
qeSETORG (void *q, indexType qe, int org)

{
  if (QEversion (qe))
    Q->dst[QE2E (qe)] = org;
  else
    Q->org[QE2E (qe)] = org;
}

/*--------------------------------------------------------------------------*/

void 
qeSETDST (void *q, indexType qe, int org)

{
  qeSETORG (q, qeSYM (qe), org);
}

/*--------------------------------------------------------------------------*/

void *
qeNewAdvanced (indexType nMax, indexType nPrescribed, indexType *O, indexType *D)

{
  qeType *tmp;

  tmp = MALLOC (qeType, 1);
  tmp->magic = QEMAGIC;
  
  tmp->max = nMax;
  tmp->ne = nPrescribed;
  tmp->nChEdges = 0;
  tmp->chEdge = -1;

  if (O == NULL) {
    if (D != NULL) {
      fprintf (stdout,"ERROR: Origin Array not specified.\n");
      exit (1);
    }
    tmp->org = MALLOC (indexType, tmp->max);
    tmp->dst = MALLOC (indexType, tmp->max);
  } else
    if (D == NULL) {
      fprintf (stdout,"ERROR: Destination Array not specified.\n");
      exit (1);
    } else {
      tmp->org = O;
      tmp->dst = D;
    }

  tmp->nxt[0] = MALLOC (indexType, tmp->max);
  tmp->nxt[1] = MALLOC (indexType, tmp->max);
  tmp->prv[0] = MALLOC (indexType, tmp->max);
  tmp->prv[1] = MALLOC (indexType, tmp->max);
  tmp->status[0] = MALLOC (qeStatusType, tmp->max);
  tmp->status[1] = MALLOC (qeStatusType, tmp->max);

  return (void *) tmp;
}

/*--------------------------------------------------------------------------*/

void *
qeNew (indexType n)

{
  return qeNewAdvanced (n, (indexType) 0,
			(indexType *) NULL, (indexType *) NULL);
}
  
/*--------------------------------------------------------------------------*/

void 
qeDispose (void *q)

{
  FREE (Q->org);
  FREE (Q->dst);
  FREE (Q->nxt[0]);
  FREE (Q->nxt[1]);
  FREE (Q->prv[0]);
  FREE (Q->prv[1]);
  FREE (Q->status[0]);
  FREE (Q->status[1]);
  FREE (q);
}

/*--------------------------------------------------------------------------*/

void 
qeAdjustSize (void *q, int nNew)

{
  REALLOC (Q->org, indexType, nNew);
  REALLOC (Q->dst, indexType, nNew);
  REALLOC (Q->nxt[0], indexType, nNew);
  REALLOC (Q->nxt[1], indexType, nNew);
  REALLOC (Q->prv[0], indexType, nNew);
  REALLOC (Q->prv[1], indexType, nNew);
  REALLOC (Q->status[0], qeStatusType, nNew);
  REALLOC (Q->status[1], qeStatusType, nNew);
}

/*--------------------------------------------------------------------------*/

indexType 
qeNE (void *q)

{
  return Q->ne;
}

/*--------------------------------------------------------------------------*/

indexType 
qeNEmax (void *q)

{
  return Q->max;
}

/*--------------------------------------------------------------------------*/

indexType 
qeNQE (void *q)

{
  return E2QE (Q->ne);
}

/*--------------------------------------------------------------------------*/

indexType 
qeNQEmax (void *q)

{
  return E2QE (Q->max);
}

/*--------------------------------------------------------------------------*/

void 
qeNEset (void *q, indexType ne)

{
  Q->ne = ne;
}

/*--------------------------------------------------------------------------*/

void 
qeNEreset (void *q)

{
  Q->ne = 0;
  Q->nChEdges = 0;
  Q->chEdge = -1;
}

/*--------------------------------------------------------------------------*/

indexType 
qeCHQE (void *q)

{
  return Q->chEdge;
}

/*--------------------------------------------------------------------------*/

void 
qeSETchQE (void *q, indexType chQE)

{
  Q->chEdge = chQE;
}

/*--------------------------------------------------------------------------*/

indexType 
qeNCHQE (void *q)

{
  return Q->nChEdges;
}


/*--------------------------------------------------------------------------*/

void 
qeSETnChQE (void *q, indexType nofCHQE)

{
  Q->nChEdges = nofCHQE;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

indexType 
qeSYM (indexType qe)

{
  return qe ^ 1;
}

/*--------------------------------------------------------------------------*/

indexType 
qeORG (void *q, indexType qe)

{
  return (QEversion (qe) ? Q->dst[QE2E (qe)] : Q->org[QE2E (qe)]);
}

/*--------------------------------------------------------------------------*/

indexType 
qeDST (void *q, indexType qe)

{
  return qeORG (q, qeSYM (qe));
}

/*--------------------------------------------------------------------------*/

indexType 
qeONEXT (void *q, indexType qe)

{
  return Q->nxt[QEversion (qe)][QE2E (qe)];
}

/*--------------------------------------------------------------------------*/

indexType 
qeOPREV (void *q, indexType qe)

{
  return Q->prv[QEversion (qe)][QE2E (qe)];
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static indexType 
qeMakeQE (void *q)

{
  indexType qenew;
  int i;

  qenew = qeMAKEQE (Q->ne);

  for (i = 0; i < 2; i++) {
    Q->status[i][Q->ne].isCH = 0;
    Q->status[i][Q->ne].isConstrained = 0;
    Q->status[i][Q->ne].isFinal = 0;
    Q->status[i][Q->ne].isDeleted = 0;
  }

  (Q->ne)++;

  return qenew;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static indexType 
qeInsertSiteSite (
    void *q,
    indexType qe,
    indexType s1,
    indexType s2	/* the two sites where the edge will be added */
)

{
  qeSETORG (q, qe, s1);
  qeSETORG (q, qeSYM (qe), s2);

  qeSETONEXT (q, qe, qe);
  qeSETONEXT (q, qe +1, qeSYM (qe));

  if (qe + 1 != qeSYM (qe)) {
    fprintf (stdout,"ERROR: qeInsertSiteSite: strange error.\n");
    exit (1);
  }

  qeSETOPREV (q, qe, qe);
  qeSETOPREV (q, qeSYM (qe), qeSYM (qe));

  return 0;
}  

/*--------------------------------------------------------------------------*/

void 
qeReinsertSiteSite (void *q, indexType qe, indexType newA, indexType newB)

/* this function is similar to the function qeAddSiteSite. The only difference
 *  is that the qe which should be used as qenew can be specified. this is
 *  useful if qe has been deleted with function qeDelete, e.g.. 
 */

{
  if (qeMAKEQE (QE2E (qe)) == qe) {
    qeInsertSiteSite (q, qe, newA, newB);
  } else {
    qeInsertSiteSite (q, qeSYM (qe), newB, newA);
  }
} 

/*--------------------------------------------------------------------------*/

indexType 
qeAddSiteSite (
    void *q,
    indexType s1,
    indexType s2	/* the two sites where the edge will be added */
)

/*
 *   qeAddSiteSite adds an edge from site s1 to site s2, assuming that
 * neither site already has an edge connected to it.
 */

{
  indexType qenew;

  if (qeNE (q) == Q->max) {
    (void) fprintf (stdout, "qeAddSiteSite: edge capacity exceeded.\n");
    exit (1);
  }

  qenew = qeMakeQE (q);
  qeInsertSiteSite (q, qenew, s1, s2);
  return (qenew);
}  

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static indexType 
qeInsertEdgeSite (void *q, indexType qe, indexType a, indexType s)

{
  indexType aRnext;

  aRnext = qeONEXT (q, qeSYM (a));

  qeSETORG (q, qe, qeDST (q, a));
  qeSETORG (q, qeSYM (qe), s);

  qeSETONEXT (q, qe, aRnext);
  qeSETONEXT (q, qe +1, qeSYM (qe));

  qeSETONEXT (q, qeSYM (a), qe);

  if (qe + 1 != qeSYM (qe)) {
    fprintf (stdout,"ERROR: qeInsertEdgeSite: strange error.\n");
    exit (1);
  }
  qeSETOPREV (q, qe, qeSYM (a));
  qeSETOPREV (q, aRnext, qe);
  qeSETOPREV (q, qeSYM (qe), qeSYM (qe));

  return 0;
}  

/*--------------------------------------------------------------------------*/

static indexType 
qeInsertEdgeSiteB (void *q, indexType qe, indexType a, indexType s)

{
  indexType aRnext;

  aRnext = qeONEXT (q, qeSYM (a));

  qeSETORG (q, qe, qeDST (q, a));
  qeSETORG (q, qeSYM (qe), s);

  qeSETONEXT (q, qe, aRnext);
  qeSETONEXT (q, qe -1, qeSYM (qe));

  qeSETONEXT (q, qeSYM (a), qe);

  if (qe - 1 != qeSYM (qe)) {
    fprintf (stdout,"ERROR: qeInsertEdgeSiteB: strange error.\n");
    exit (1);
  }
  qeSETOPREV (q, qe, qeSYM (a));
  qeSETOPREV (q, aRnext, qe);
  qeSETOPREV (q, qeSYM (qe), qeSYM (qe));

  return 0; 
}  

/*--------------------------------------------------------------------------*/

void 
qeReinsertEdgeSite (void *q, indexType qe, indexType newA, indexType newS)

/* this function is similar to the function qeAddEdgeSite. The only difference
 *  is that the qe which should be used as qenew can be specified. this is
 *  useful if qe has been deleted with function qeDelete, e.g.. 
 */

{
  if (qeMAKEQE (QE2E (qe)) == qe) {
    qeInsertEdgeSite (q, qe, newA, newS);
  } else {
    qeInsertEdgeSiteB (q, qe, newA, newS);
  }
} 

/*--------------------------------------------------------------------------*/

indexType 
qeAddEdgeSite (
    void *q,
    indexType a,		/* quadedge pointing to origin of new site */
    indexType s		/* site which will be new edge's destination */
)

/*
 *   qeAddSite adds an edge from the Dest of edge "a" to site s.  It
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
  indexType qenew;

  if (qeNE (q) == Q->max) {
    (void) fprintf (stdout, "qeAddEdgeSite: edge capacity exceeded.\n");
    exit (1);
  }

  qenew = qeMakeQE (q);
  qeInsertEdgeSite (q, qenew, a, s);
  return (qenew);
}  

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static void 
qeInsertEdgeEdge (void *q, indexType qe, indexType a, indexType b)

{
  indexType aRnext, bLprev;
  
  aRnext = qeONEXT (q, qeSYM (a));
  bLprev = qeONEXT (q, b);

  qeSETORG (q, qe, qeDST (q, a));
  qeSETORG (q, qeSYM (qe), qeORG (q, b));

  qeSETONEXT (q, qe, qeONEXT (q, qeSYM (a)));
  qeSETONEXT (q, qe + 1, qeONEXT (q, b));

  qeSETONEXT (q, qeSYM (a), qe);
  qeSETONEXT (q, b, qeSYM (qe));

  if (qe + 1 != qeSYM (qe)) {
    fprintf (stdout,"ERROR: qeInsertEdgeEdge: strange error.\n");
    exit (1);
  }
  qeSETOPREV (q, qe, qeSYM (a));
  qeSETOPREV (q, aRnext, qe);
  qeSETOPREV (q, qeSYM (qe), b);
  qeSETOPREV (q, bLprev, qeSYM (qe));
} 

/*--------------------------------------------------------------------------*/

void 
qeReinsertEdgeEdge (void *q, indexType qe, indexType newA, indexType newB)

/* this function is similar to the function qeAddEdgeEdge. The only difference
 *  is that the qe which should be used as qenew can be specified. this is
 *  useful if qe has been deleted with function qeDelete, e.g.. 
 */

{
  if (qeMAKEQE (QE2E (qe)) == qe) {
    qeInsertEdgeEdge (q, qe, newA, newB);
  } else {
    qeInsertEdgeEdge (q, qeSYM (qe), qeSYM (newB), qeSYM (newA));
  }
} 

/*--------------------------------------------------------------------------*/

indexType 
qeAddEdgeEdge (void *q, indexType a, indexType b)

/*
 *   qeAddEdgeEdge adds an edge to between the site pointed to by edge
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
  indexType qenew;

  if (qeNE (q) == Q->max) {
    (void) fprintf (stdout, "qeAddEdgeEdge: edge capacity exceeded.\n");
    exit (1);
  }

  qenew = qeMakeQE (q);
  qeInsertEdgeEdge (q, qenew, a, b);
  return (qenew);
}  

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

void 
qeFlip (void *q, indexType qe)

/*            *                         *
 * eSymOprev /^\ eSymOnext             / \
 *          / | \                     / e \
 *         * e|  *          ===>     *<----*
 *   eOnext \ | / eOprev              \   /
 *           \|/                       \ /
 *            *                         *
 */
{
  indexType eOnext, eOprev, eSymOnext, eSymOprev;
 
  /* save neighboring edges in original configuration */
  eOnext = qeONEXT (q, qe);
  eOprev = qeOPREV (q, qe);
  eSymOnext = qeONEXT (q, qeSYM (qe));
  eSymOprev = qeOPREV (q, qeSYM (qe));
  
  /* set sites to new values */
  qeSETORG (q, qe, qeDST (q, qeONEXT (q, qeSYM (qe))));	/* new e Org */
  qeSETORG (q, qeSYM (qe), qeDST (q, qeONEXT (q, qe)));	/* new e Dest */

  /* set nxt pointers of the flipped edge */
  qeSETONEXT (q, qe, qeSYM (eOprev));
  qeSETONEXT (q, qeSYM (qe), qeSYM (eSymOprev));

  /* Update pointers of neighboring edges. */
  qeSETONEXT (q, qeSYM (eOnext), qeSYM (qe));
  qeSETONEXT (q, eOprev, eOnext);
  qeSETONEXT (q, qeSYM (eSymOnext), qe);
  qeSETONEXT (q, eSymOprev, eSymOnext);

  qeSETOPREV (q, qe, qeSYM (eSymOnext));
  qeSETOPREV (q, qeSYM (qe), qeSYM (eOnext));

  qeSETOPREV (q, qeSYM (eOprev), qe);
  qeSETOPREV (q, qeSYM (eSymOprev), qeSYM (qe));
  qeSETOPREV (q, eOnext, eOprev);
  qeSETOPREV (q, eSymOnext, eSymOprev);
}  

/*--------------------------------------------------------------------------*/

void 
qeUnlink (void *q, indexType qe)

/*            *                         *
 * eSymOprev /^\ eSymOnext             / \
 *          / | \                     /   \
 *         * e|  *          ===>     *     *
 *   eOnext \ | / eOprev              \   /
 *           \|/                       \ /
 *            *                         *
 */

/* CAUTION: the fields for qe are not updated, and qe is not marked as deleted
            and non of the other functions
            knows about the deleted edge! so the application must take care
	    of this! use qeReinsert to use qe again (not necessarily in the
	    original position). */

{
  indexType eOnext, eOprev, eSymOnext, eSymOprev;

  /* save neighboring edges in original configuration */
  eOnext = qeONEXT (q, qe);
  eOprev = qeOPREV (q, qe);
  eSymOnext = qeONEXT (q, qeSYM (qe));
  eSymOprev = qeOPREV (q, qeSYM (qe));

  /* Update pointers of neighboring edges. */
  qeSETONEXT (q, eOprev, eOnext);
  qeSETONEXT (q, eSymOprev, eSymOnext);

  qeSETOPREV (q, eOnext, eOprev);
  qeSETOPREV (q, eSymOnext, eSymOprev);
}

/*-------------------------------------------------------------------------*/

void 
qeDeleteEdge (void *q, indexType edge)

{
  indexType last, qe;
  int i = 0;

  qe = qeMAKEQE (edge);
  last = qeMAKEQE (qeNE (q) - 1);

  if (qeQETOE (last) != qeQETOE (qe)) {
    do {
      qeSETORG (q, qe, qeORG (q, last));
      if (qeONEXT (q, last) == last) {
	qeSETONEXT (q, qe, qe);
	qeSETOPREV (q, qe, qe);
      } else {
	qeSETONEXT (q, qe, qeONEXT (q, last));
	qeSETOPREV (q, qe, qeOPREV (q, last));
	qeSETONEXT (q, qeOPREV (q, last), qe);
	qeSETOPREV (q, qeONEXT (q, last), qe);
      }
      
      qeISDELETEDQE (q, last) ? qeSETDELETEDQE (q, qe) : 
	qeUNSETDELETEDQE (q, qe);
      qeISCHQE (q, last) ? qeSETCHQE (q, qe) : qeUNSETCHQE (q, qe);
      qeISFINALQE (q, last) ? qeSETFINALQE (q, qe) : qeUNSETFINALQE (q, qe);
      qeISCONSTRQE (q, last) ? qeSETCONSTRQE (q, qe) : 
	                        qeUNSETCONSTRQE (q, qe);
      qe = qeSYM (qe);
      last = qeSYM (last);
    } while (i++ < 1);
  }

  Q->ne -= 1;
}

/*--------------------------------------------------------------------------*/

void 
qeDuplicate (void *qSource, void *qDest, indexType qe)


{
  indexType e;

qeMagic (qSource);
qeMagic (qDest);
  
  e = QE2E (qe);
  
  ((qeType *) qDest)->org[e] = ((qeType *) qSource)->org[e];
  ((qeType *) qDest)->dst[e] = ((qeType *) qSource)->dst[e];
  ((qeType *) qDest)->nxt[0][e] = ((qeType *) qSource)->nxt[0][e];
  ((qeType *) qDest)->nxt[1][e] = ((qeType *) qSource)->nxt[1][e];
  ((qeType *) qDest)->prv[0][e] = ((qeType *) qSource)->prv[0][e];
  ((qeType *) qDest)->prv[1][e] = ((qeType *) qSource)->prv[1][e];
  ((qeType *) qDest)->status[0][e] = ((qeType *) qSource)->status[0][e];
  ((qeType *) qDest)->status[1][e] = ((qeType *) qSource)->status[1][e];
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

void 
qeResetCONSTRstatus (void *q)

{
  int i;

  for (i = 0; i < qeNE (q); i++) {
    qeUNSETCONSTRedge (q, i);
  }
}

/*--------------------------------------------------------------------------*/

void 
qeResetFINALstatus (void *q)

{
  int i;

  for (i = 0; i < qeNE (q); i++) {
    qeUNSETFINALedge (q, i);
  }
}

/*--------------------------------------------------------------------------*/

void 
qeResetDELETEDstatus (void *q)

{
  int i;

  for (i = 0; i < qeNE (q); i++) {
    qeUNSETDELETEDedge (q, i);
  }
}

/*--------------------------------------------------------------------------*/

void 
qeResetCHstatus (void *q)

{
  int i;

  for (i = 0; i < qeNE (q); i++) {
    qeUNSETCHedge (q, i);
  }
}

/*--------------------------------------------------------------------------*/

void 
qeResetALLstati (void *q)

{
  int i;

  for (i = 0; i < qeNE (q); i++) {
    qeUNSETCHedge (q, i);
    qeUNSETFINALedge (q, i);
    qeUNSETDELETEDedge (q, i);
  }
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

/* here are the function equivalents of the macros in the original 
implementation of the quadedges */

/*--------------------------------------------------------------------------*/

indexType 
qeMAKEQE (indexType e)

/* qeMAKEQE takes an edge index and returns quadedge index */

{
  return E2QE (e);
}

/*--------------------------------------------------------------------------*/

indexType 
qeQETOE (indexType qe)

/* qeQETOE takes a quadedge index and returns the edge number */

{
  return QE2E (qe);
}

/*--------------------------------------------------------------------------*/

indexType 
qeQETOQE0 (indexType qe)

/*qeQETOQE0 takes a quadedge index and returns qe index with no rotation */

{
  return (qe | 1) ^ 1;
}

/*--------------------------------------------------------------------------*/

int 
qeISCHQE (void *q, indexType qe)

/* qeISCHQE returns 1 iff qe is convex hull edge */

{
  return Q->status[QEversion (qe)][QE2E (qe)].isCH;
}

/*--------------------------------------------------------------------------*/

void 
qeSETCHQE (void *q, indexType qe)

{
  Q->status[QEversion (qe)][QE2E (qe)].isCH = 1;
}

/*--------------------------------------------------------------------------*/

void 
qeUNSETCHQE (void *q, indexType qe)

{
  Q->status[QEversion (qe)][QE2E (qe)].isCH = 0;
}

/*--------------------------------------------------------------------------*/

int 
qeISCONSTRQE (void *q, indexType qe)

/* ISCONSTRQE returns 1 iff qe is a constrained edge */

{
  return Q->status[QEversion (qe)][QE2E (qe)].isConstrained;
}

/*--------------------------------------------------------------------------*/

void 
qeSETCONSTRQE (void *q, indexType qe)

{
  Q->status[QEversion (qe)][QE2E (qe)].isConstrained = 1;
}

/*--------------------------------------------------------------------------*/

void 
qeUNSETCONSTRQE (void *q, indexType qe)

{
  Q->status[QEversion (qe)][QE2E (qe)].isConstrained = 0;
}

/*--------------------------------------------------------------------------*/

int 
qeISFINALQE (void *q, indexType qe)

/* ISFINAL returns 1 iff qe is an edge which should appear in the final
   triangulation, this variable has to be set and reset by the application
   program */

{
  return Q->status[QEversion (qe)][QE2E (qe)].isFinal;
}

/*--------------------------------------------------------------------------*/

void 
qeSETFINALQE (void *q, indexType qe)

{
  Q->status[QEversion (qe)][QE2E (qe)].isFinal = 1;
}

/*--------------------------------------------------------------------------*/

void 
qeUNSETFINALQE (void *q, indexType qe)

{
  Q->status[QEversion (qe)][QE2E (qe)].isFinal = 0;
}

/*--------------------------------------------------------------------------*/

int 
qeISDELETEDQE (void *q, indexType qe)

{
  return Q->status[QEversion (qe)][QE2E (qe)].isDeleted;
}

/*--------------------------------------------------------------------------*/

void 
qeSETDELETEDQE (void *q, indexType qe)

{
  Q->status[QEversion (qe)][QE2E (qe)].isDeleted = 1;
}

/*--------------------------------------------------------------------------*/

void 
qeUNSETDELETEDQE (void *q, indexType qe)

{
  Q->status[QEversion (qe)][QE2E (qe)].isDeleted = 0;
}

/*--------------------------------------------------------------------------*/

int 
qeISCHedge (void *q, indexType edge)

/* ISCHedge returns 1 iff edge is convex hull edge */

{
  return qeISCHQE (q, qeMAKEQE (edge));
}

/*--------------------------------------------------------------------------*/

void 
qeSETCHedge (void *q, indexType edge)

{
  qeSETCHQE (q, qeMAKEQE (edge));
  qeSETCHQE (q, qeSYM (qeMAKEQE (edge)));
}

/*--------------------------------------------------------------------------*/

void 
qeUNSETCHedge (void *q, indexType edge)

{
  qeUNSETCHQE (q, qeMAKEQE (edge));
  qeUNSETCHQE (q, qeSYM (qeMAKEQE (edge)));
}

/*--------------------------------------------------------------------------*/

int 
qeISCONSTRedge (void *q, indexType edge)

/* ISCONSTRedge returns 1 iff edge is a constrained edge */

{
  return qeISCONSTRQE (q, qeMAKEQE (edge));
}

/*--------------------------------------------------------------------------*/

void 
qeSETCONSTRedge (void *q, indexType edge)

{
  qeSETCONSTRQE (q, qeMAKEQE (edge));
  qeSETCONSTRQE (q, qeSYM (qeMAKEQE (edge)));
}

/*--------------------------------------------------------------------------*/

void 
qeUNSETCONSTRedge (void *q, indexType edge)

{
  qeUNSETCONSTRQE (q, qeMAKEQE (edge));
  qeUNSETCONSTRQE (q, qeSYM (qeMAKEQE (edge)));
}

/*--------------------------------------------------------------------------*/

int 
qeISFINALedge (void *q, indexType edge)

/* ISFINALedge returns 1 iff edge is an edge which should appear in the final
   triangulation, this variable has to be set and reset by the application
   program*/

{
  return qeISFINALQE (q, qeMAKEQE (edge));
}

/*--------------------------------------------------------------------------*/

void 
qeSETFINALedge (void *q, indexType edge)

{
  qeSETFINALQE (q, qeMAKEQE (edge));
  qeSETFINALQE (q, qeSYM (qeMAKEQE (edge)));
}

/*--------------------------------------------------------------------------*/

void 
qeUNSETFINALedge (void *q, indexType edge)

{
  qeUNSETFINALQE (q, qeMAKEQE (edge));
  qeUNSETFINALQE (q, qeSYM (qeMAKEQE (edge)));
}

/*--------------------------------------------------------------------------*/

int 
qeISDELETEDedge (void *q, indexType edge)

/* ISDELETEDedge returns 1 iff edge is an edge in the current graph,
   this variable has to be set and reset by the application
   program*/

{
  return qeISDELETEDQE (q, qeMAKEQE (edge));
}

/*--------------------------------------------------------------------------*/

void 
qeSETDELETEDedge (void *q, indexType edge)

{
  qeSETDELETEDQE (q, qeMAKEQE (edge));
  qeSETDELETEDQE (q, qeSYM (qeMAKEQE (edge)));
}

/*--------------------------------------------------------------------------*/

void 
qeUNSETDELETEDedge (void *q, indexType edge)

{
  qeUNSETDELETEDQE (q, qeMAKEQE (edge));
  qeUNSETDELETEDQE (q, qeSYM (qeMAKEQE (edge)));
}

/*--------------------------------------------------------------------------*/

void 
qeDeleteMarkedEdges (void *q)

{
  indexType edge;

  edge = 0;

  while ((edge < qeNE (q)) && (qeNE (q) > 0))
    if (qeISDELETEDedge (q, qeNE (q) - 1))
      qeNEset (q, qeNE (q) - 1);
    else
      if (qeISDELETEDedge (q, edge))
        qeDeleteEdge (q, edge);
      else
        edge++;
}

/*--------------------------------------------------------------------------*/

static qeType *P;

static int 
qeGT (void *i1, void *i2)

{
  if (qeORG (P, *((indexType *) i1)) > qeORG (P, *((indexType *) i2)))
    return 1;
  if (qeORG (P, *((indexType *) i1)) < qeORG (P, *((indexType *) i2)))
    return -1;
  if (qeDST (P, *((indexType *) i1)) > qeDST (P, *((indexType *) i2)))
    return 1;
  if (qeDST (P, *((indexType *) i1)) < qeDST (P, *((indexType *) i2)))
    return -1;
  return 0;
}

/*--------------------------------------------------------------------------*/

int 
qeMarkDuplicateEdges (void *q)

{
  indexType min, i, j, count, qe, edge;
  indexType * Index;

  P = Q;

  Index = MALLOC (indexType, qeNE (q));
  qeResetDELETEDstatus (q);
  for (edge = 0; edge < qeNE (q); edge++) {
    qe = qeMAKEQE (edge);
    if (qeORG (q, qe) > qeDST (q, qe))
      qe = qeSYM (qe);
    Index [edge] = qe;
  }

  qsort (Index, qeNE (q), sizeof (indexType), qeGT);

  count = 0;

  for (i = 0; i < qeNE (q) - 1;) {
    j = i + 1;
    min = i;

    while ((j < qeNE (q)) && 
	   ((qeORG (q, Index[i]) == qeORG (q, Index[j])) &&
	    (qeDST (q, Index[i]) == qeDST (q, Index[j])))) {
      min = (Index[min] > Index[j] ? j : min);
      j++;
    }

    if (j > i + 1) {
      count += j - i - 1;
      while (i < j) {
	if (i != min) 
	  qeSETDELETEDedge (q, qeQETOE (Index[i]));
	i++;
      }
    } else
      i = j;
  }

/*  for (i = 0; i < siNS (s); i++)
    fprintf (stdout,"%d %lf %lf %d\n", Index[i],
	    siSITEX (s, Index[i]), siSITEY (s, Index[i]),
	    siIsDuplicateSite (s, Index[i]));*/

  fprintf (stdout,"qeMarkDeletedDuplicateEdges: %d duplicate edges marked.\n\n",
	  count);
  
  FREE (Index);

  return count;
}  

/*--------------------------------------------------------------------------*/

int 
qeRemoveDuplicateEdges (void *q)

{
  int nofDup;

  nofDup = qeMarkDuplicateEdges (q);
  qeDeleteMarkedEdges (q);

  return nofDup;
}
  
/*--------------------------------------------------------------------------*/

void 
qeNormalize (void *q)

{
  indexType i, qe, symqe, a, b;

  for (i = 0; i < qeNE (q); i++) {
    symqe = qeSYM (qe = qeMAKEQE (i));
    if (qeORG (q, qe) > qeDST (q, qe)) {
      a = qeSYM (qeOPREV (q, symqe));
      b = qeOPREV (q, qe);
      qeUnlink (q, qe);
      if (qeMAKEQE (qeQETOE (symqe)) == symqe)
	qeReinsertEdgeEdge (q, symqe, a, b);
      else
	qeReinsertEdgeEdge (q, qe, a, b);
    }
  }
}

/*--------------------------------------------------------------------------*/

void 
qePrintEdges (void *q, FILE *fp)

{
  indexType edge, qe;

  fprintf (fp, "# edge origin destination\n");
  fprintf (fp, 
   "# #edges = %d, #chedges = %d\n", qeNE (q), qeNCHQE (q));

  for (edge=0; edge < qeNE (q); edge++) {
    qe = qeMAKEQE (edge);
    if (qeORG (q, qe) < qeDST (q, qe))
      fprintf (fp, "%s%sedge %d %d\n", 
	       (qeISCHQE (q, qe) ? "ch" : ""),
	       (qeISCONSTRQE (q, qe) ? "constr" : ""),
	       qeORG (q, qe), qeDST (q, qe));
    else
      fprintf (fp, "%s%sedge %d %d\n", 
	       (qeISCHQE (q, qeSYM (qe)) ? "ch" : ""),
               (qeISCONSTRQE (q, qeSYM (qe)) ? "constr" : ""),
	       qeDST (q, qe), qeORG (q, qe));
  }

  fprintf (fp, "\n");
}

/*--------------------------------------------------------------------------*/

void 
qePrintQEdges (void *q, FILE *fp)

{
  indexType edge, qe;

  fprintf (fp, 
   "# edge orig dest dest(next) dest(prev) dest(next(sym)) dest(prev(sym))\n");
  fprintf (fp, 
   "# #edges = %d, #chedges = %d\n", qeNE (q), qeNCHQE (q));

  for (edge=0; edge < qeNE (q); edge++) {
    qe = qeMAKEQE (edge);
    if (qeORG (q, qe) < qeDST (q, qe))
      fprintf (fp, "edge %d %d %d %d %d %d\n", 
	       qeORG (q, qe), qeDST (q, qe), qeDST (q, qeONEXT (q, qe)),
	       qeDST (q, qeOPREV (q, qe)), qeDST (q, qeONEXT (q, qeSYM (qe))),
	       qeDST (q, qeOPREV (q, qeSYM (qe))));
    else
      fprintf (fp, "edge %d %d %d %d %d %d\n", 
	       qeDST (q, qe), qeORG (q, qe), qeDST (q, qeONEXT (q, qeSYM (qe))),
	       qeDST (q, qeOPREV (q, qeSYM (qe))), qeDST (q, qeONEXT (q, qe)),
	       qeDST (q, qeOPREV (q, qe)));
  }

  fprintf (fp, "\n");
}

/*--------------------------------------------------------------------------*/

static char *
qeReadLine (FILE *fp)

{
  static char buf [80];
  char * tmp;
  int i;

  for (i = 0; i < 80; i++) buf [i] = ' ';

  while (fgets (buf, 80, fp) != NULL) {

    tmp = buf;
    if (*tmp == '#') 
      continue;

    while (isspace (*tmp))
      tmp++;

    return tmp;

  }

  return NULL;
}

/*--------------------------------------------------------------------------*/

static int 
qeDoReadEdges (void *q, FILE *fp, int examine, int *nofEdges)

{
  static char * inputFormat [4] = {"edge %d %d", "constredge  %d %d", 
				   "chedge %d %d", "chconstredge  %d %d"};
  static int makeConstr [4] = {0, 1, 0, 1};

  int i;
  indexType org, dst, newqe;
  char * line;

  *nofEdges = 0;

  while ((line = qeReadLine (fp)) != NULL) 
    
    for (i = 0; i <= 3; i++)
      if (sscanf (line, inputFormat[i], &org, &dst) == 2) {
	if (examine) {
	  (*nofEdges)++;
	} else {
	  newqe = qeAddSiteSite (q, org, dst);
	  if (makeConstr [i] == 1)
	    qeSETCONSTRedge (q, qeQETOE (newqe));
	}
	break;
      }

      return 0;
}

/*--------------------------------------------------------------------------*/

void qeReadEdges (char *fname, void *q)

{
  int dummy1;
  FILE *fp;

  if ((fp = fopen (fname, "r")) == NULL) {
    fprintf (stdout, 
	     "ERROR: qeReadEdges:  can't open \"%s\" to read.\n", fname);
    exit (1);
  }

  qeDoReadEdges (q, fp, (int) 0, &dummy1);

  if (fclose (fp) == EOF)
    fprintf (stdout, "ERROR: qeReadEdges: could not close file \"%s\".\n",fname);
}

/*--------------------------------------------------------------------------*/

void 
qeExamineEdges (char *fname, int *nofEdges)

{
  FILE *fp;

  if ((fp = fopen (fname, "r")) == NULL) {
    fprintf (stdout, 
	     "ERROR: qeReadEdges:  can't open \"%s\" to read.\n", fname);
    exit (1);
  }

  qeDoReadEdges ((void *) NULL, fp, (int) 1, nofEdges);

  if (fclose (fp) == EOF)
    fprintf (stdout, "ERROR: qeExamineEdges: could not close file \"%s\".\n",fname);

}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
