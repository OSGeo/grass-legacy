#include <math.h>
#include "internoptri.h"
#include "persquadedge.h"

#define MYMAX(a,b) (a > b ? a : b)

/*-------------------------------------------------------------------------*/

static indexType findPrevPqe (
     void *pq,
     indexType pqe,
     indexType time)

{
  indexType tmp;

/*  printPersistQe (&(PQ[pqe]), pqe);*/

  tmp = pOPREV (pq, pqe, time);

  while (tmp != pqe) {
    if (tmp < pqe)
      if ((pORG (pq, tmp) >= 0) && (pDST (pq, tmp) >= 0))
	return tmp;
    tmp = pOPREV (pq, tmp, time);
  }

  return -1;
}

/*-------------------------------------------------------------------------*/

static indexType findCorrespondingQe (

     indexType org,indexType dst,
     void *q,
     indexType *inci)

{
  indexType qe;

  qe = inci [org];
  if (qeDST (q, qe) == dst) {
    return qe;
  }

  for (qe = qeONEXT (q, qe); qe != inci [org]; qe = qeONEXT (q, qe))
    if (qeDST (q, qe) == dst) {
      return qe;
    }
  
  fprintf (stdout,"ERROR: findCorrespondingQe: (%d, %d) has no corresponding qe.\n",
	  org, dst);
  exit (1);
}
  
/*-------------------------------------------------------------------------*/

static void hDagCopyPQEtoQE (
     void *pq,
     indexType pqe,
     void *qDst,
     indexType *inci,
     indexType time)

{
  indexType qeOrg, qeDst, pqeOrg, pqeDst, newqe;

/*  fprintf (stdout,"%d %d %d\n", pORG (pq, pqe), pDST (pq, pqe), pqe);
  fprintf (stdout,"%d %d %d -\n", pDST (pq, pqe), pORG (pq, pqe), pqe);
*/
  pqeOrg = findPrevPqe (pq, pqe, time);
  pqeDst = findPrevPqe (pq, pSYM(pqe), time);

  if (pqeOrg >= 0) {
    qeOrg = findCorrespondingQe (pORG(pq, pqeOrg), pDST (pq, pqeOrg), qDst, 
				 inci);
    if (pqeDst >= 0) {
      qeDst = findCorrespondingQe (pORG(pq, pqeDst), pDST (pq, pqeDst),
				   qDst, inci);
      newqe = qeAddEdgeEdge (qDst, qeSYM (qeOrg), qeDst);
    } else {
      newqe = qeAddEdgeSite (qDst, qeSYM (qeOrg), pDST (pq, pqe));
    }
  } else {
    if (pqeDst >= 0) {
      qeDst = findCorrespondingQe (pORG(pq, pqeDst), pDST (pq, pqeDst),
				   qDst, inci);
      newqe = qeAddEdgeSite (qDst, qeSYM (qeDst), pORG (pq, pqe));
    } else {
      newqe = qeAddSiteSite (qDst, pORG (pq, pqe), pDST (pq, pqe));
    }
  }

  inci [qeORG (qDst, newqe)] = newqe;
  inci [qeDST (qDst, newqe)] = qeSYM (newqe);

  if (pISCHQE (pq, pqe, time)) {
    qeSETCHedge (qDst, qeQETOE (newqe));
    qeSETnChQE (qDst, qeNCHQE (qDst) + 1);
    if (pDST (pq, pOPREV (pq, pqe, time)) < 0)
      qeSETchQE (qDst, newqe);
    else
      qeSETchQE (qDst, qeSYM (newqe));
  } else
    qeUNSETCHedge (qDst, qeQETOE (newqe));
}

/*-------------------------------------------------------------------------*/

void copyHdagToQe (
     void *pq,
     indexType time,
     void *qDst)

/* copies the version of the triangulation in the hdag as it was at */
/* time 'time' to a quadedge datastructure */

{
  indexType pqe, e, maxVertex, vertex;
  indexType *inci; 

  if (time == -1)
    time = pTimeNow (pq);

  maxVertex = 0;
  for (e = 0; e < pNPE (pq); e++) {
    pqe = pMAKEQE (e);
    maxVertex = MYMAX (maxVertex, pORG (pq, pqe));
    maxVertex = MYMAX (maxVertex, pDST (pq, pqe));
  }

  inci = MALLOC (indexType,  maxVertex + 1);
  for (vertex = 0; vertex < maxVertex; vertex++)
    inci [vertex] = -1;

  qeNEreset (qDst);

  for (e = 0; e < pNPE (pq); e++) {
    pqe = pMAKEQE (e);

    if ((pORG (pq, pqe) >= 0) && (pDST (pq, pqe) >= 0)) /* only finite */
						      /* vertices */
						      /* incident ? */
      if (pqeExistsAtTime (pq, pqe, time))  /* does it exist at 'time' ? */
	hDagCopyPQEtoQE (pq, pqe, qDst, inci, time);
  }

  FREE (inci);
}
  
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

