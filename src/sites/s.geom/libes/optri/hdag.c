
#include "internoptri.h"
#include "persquadedge.h"

/*--------------------------------------------------------------------------*/

#define HDAGMAGIC 38765454

/*--------------------------------------------------------------------------*/

typedef struct {

  persistQeType *pq;
  void * s;
  indexType     topTriangleEdge;
  int           (*leftTurnTest) ();
  int           (*terminationCriterion) ();
  int           magic;

} hdagType;

/*--------------------------------------------------------------------------*/

#ifdef TESTMAGIC

  static hdagType *
  hdagMagic (h)

       void *h;

  {
    void (*X) ();

    X = NULL;

    if (((hdagType *) h)->magic != HDAGMAGIC) {
      printf ("ERROR: hdagMagic: wrong magic.\n");
      X();
      exit (1);
    }

    return (hdagType *) h;
  }

#else

#define hdagMagic(h) ((hdagType *) h)

#endif


#define H (hdagMagic(h))
#define HPQ (H->pq)

int 
hdagIsHDAGtype (h)

     void *h;

{
  hdagMagic (h);
  return 1;
}

/*--------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

void *
hdagSI (h)

     void *h;

{
  return H->s;
}

/*---------------------------------------------------------------------------*/

void *
hdagPQ (h)

     void *h;

{
  return HPQ;
}

/*---------------------------------------------------------------------------*/

void
hdagInsertTopTriangle (h, v1, v2, v3)

     void *h;
     indexType v1, v2, v3;


/*
  -2              -1
    *<------------*  
     \           /   
      \    0    /    
       \       /     
        \     / 
         \   /       
          \ /        
           * -3        
*/

/* the coordinates of these points are stored in g->infP */

{
  indexType qe1, qe2, qe3;

  qe1 = pqeAddSiteSite(HPQ, v1, v2);        
  qe2 = pqeAddEdgeSite(HPQ, qe1, v3);
  qe3 = pqeAddEdgeEdge(HPQ, qe2, qe1);
  
  H->topTriangleEdge = qe2;
}


/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

indexType
hdagLocatePoint (h, p, spl)

     void *h;
     indexType p;
     int *spl;

{
  nextRecordType *nxt;
  indexType index;
  indexType maxEntries;
  additionalNextRecordsType ** additional;

  pqeGetPresent (HPQ, H->topTriangleEdge, pTime0 (HPQ) + 3, &nxt, &index, 
		 &maxEntries, &additional);

/*  printf("hdagLocatePoint %d\n", p);
  printPersistQe (&(H->pe[ H->topTriangleEdge]),  H->topTriangleEdge);
  printPersistQe2 (nxt, index, maxEntries, *additional);
*/
  return doLocatePoint (h, H->topTriangleEdge, p, pTime0 (HPQ) + 3, 
			nxt, index, maxEntries, *additional, spl);
}

/*---------------------------------------------------------------------------*/

static indexType
doLocatePoint (h, triEdge, p, time, nxt, index, maxEntries,
	       additional, spl)

     void *h;
     indexType triEdge; /* one quad edge of the triangle which containes p */
			/* at time 'time'. the quadedge is */
			/* counterclockwise. */
     indexType p;
     indexType time;
     nextRecordType *nxt; /* pointer to the array which contains the */
			  /* next pointer for time 'time' as its */
			  /* 'index'th entry */
     indexType index;
     indexType maxEntries;  /* max # of entries nxt can contain */
     additionalNextRecordsType * additional; /* pointer to the next */
					     /* record in case we need */
					     /* to follow the */
					     /* 'triEdge' further to */
					     /* the present */
     int *spl;

{
  nextRecordType *futureNxt, *presentNxt;
  indexType futureIndex, presentIndex;
  indexType futureMaxEntries, presentMaxEntries;
  additionalNextRecordsType ** futureAdditional, ** presentAdditional;

  (*spl)++;

/*  printf("doLocatePoint: %d, %d, %d; %d at %d\n",
	 pORG (HPQ, triEdge), pDST (HPQ, triEdge), 
	 pDST (HPQ, nxt[index].edge), p, time);*/

/*  printPersistQe2 (nxt, index, maxEntries, additional);
*/
  if (*spl % 10 == 9) /* allitle heuristic to speed up the search */
    if ((H->terminationCriterion) (h, pORG (HPQ, triEdge), 
				   pDST (HPQ, triEdge), p,
				   pDST (HPQ, nxt[index].edge)))
      return -1;
  
  if (pqeISLASTENTRY (nxt, index, maxEntries, additional)) {
    /* found triangle of current triangulation which contains p */
    if (*spl % 10 != 9) /* compensation for the heuristic */
      if ((H->terminationCriterion) (h, pORG (HPQ, triEdge), 
				     pDST (HPQ, triEdge), p,
				     pDST (HPQ, nxt[index].edge)))
	return -1;
    
    return triEdge; 
  }

/*  printf ("hallo\n");*/

  pqeGetFuture (nxt, index, maxEntries, &additional, &futureNxt, &futureIndex,
		&futureMaxEntries, &futureAdditional);
		
  if (pqeISDEAD (futureNxt, futureIndex, futureMaxEntries,
		 *futureAdditional)) 
    /* triEdge has been flipped or deleted */

/*    printf("isdead\n");*/
    if (pqeISDEADslow (HPQ, nxt[index].edge, futureNxt[futureIndex].time)) {
      /* edge is dead because of a 3-1 flip, the center of the flip is */
      /* the origin of the edge */
      pqeGetPresent (HPQ, pOPREV (HPQ, pSYM (triEdge), time), 
		     futureNxt[futureIndex].time,
		     &presentNxt, &presentIndex, &presentMaxEntries,
		     &presentAdditional);
/*      printf("halla %d\n", time);*/
      return doLocatePoint (h, pOPREV (HPQ, pSYM (triEdge), time), p,
			    presentNxt[presentIndex].time, presentNxt,
			    presentIndex, presentMaxEntries, 
			    *presentAdditional, spl);
    } else 
      if (pqeISDEADslow (HPQ, pONEXT (HPQ, pSYM (nxt[index].edge), time),
			 futureNxt[futureIndex].time)) {
	/* edge is dead because of a 3-1 flip, the center of the flip is */
	/* the destination of the edge */
	pqeGetPresent (HPQ, pSYM (nxt[index].edge), futureNxt[futureIndex].time,
		       &presentNxt, &presentIndex, &presentMaxEntries,
		       &presentAdditional);
/*	printf("hallo \n");*/
	return doLocatePoint (h, pSYM (nxt[index].edge), p,
			      presentNxt[presentIndex].time, presentNxt,
			      presentIndex, presentMaxEntries, 
			      *presentAdditional, spl);
      } else {
	/* edge is dead because of a 2-2 flip */
	pqeGetPresent (HPQ, pSYM(nxt[index].edge), futureNxt[futureIndex].time,
		       &presentNxt, &presentIndex, &presentMaxEntries,
		       &presentAdditional);  
	return doubleTest (h, pSYM(nxt[index].edge), p,
			   presentNxt[presentIndex].time, presentNxt,
			   presentIndex, presentMaxEntries,
			   *presentAdditional, spl);
      }

  if (pqeISNEW (HPQ, futureNxt[futureIndex].edge, futureNxt[futureIndex].time)) 
    if (pqeISNEW (HPQ, pONEXT (HPQ, pSYM(futureNxt[futureIndex].edge), 
			     futureNxt[futureIndex].time), 
		  futureNxt[futureIndex].time)) 
      /* new point was inserted inside the triangle */
      return tripleTest (h, triEdge, p, futureNxt[futureIndex].time, 
			 futureNxt, futureIndex, futureMaxEntries,
			 *futureAdditional, spl);
    else {
      /* pOPREV(SYM(triEdge)) was flipped */

/*      printf("pOPREV(SYM(triEdge)) was flipped\n");*/

      return doubleTest (h, triEdge, p, futureNxt[futureIndex].time,
			 futureNxt, futureIndex, futureMaxEntries,
                         *futureAdditional, spl);
    }

  if (pqeISNEW (HPQ, pONEXT (HPQ, pSYM(futureNxt[futureIndex].edge), 
			   futureNxt[futureIndex].time), 
		futureNxt[futureIndex].time)) {

    /* pONEXT(triEdge) was flipped */
    pqeGetPresent (HPQ, pSYM(futureNxt[futureIndex].edge), 
		   futureNxt[futureIndex].time,
		   &presentNxt, &presentIndex, &presentMaxEntries,
		   &presentAdditional);
  
    return doubleTest (h, pSYM(futureNxt[futureIndex].edge), p,
		       presentNxt[presentIndex].time, presentNxt, 
		       presentIndex, presentMaxEntries,
		       *presentAdditional, spl);
  } else {
    /* 3-1 flip for which the destination of the nxt edge is the center */
     return doLocatePoint (h, triEdge, p, futureNxt[futureIndex].time, 
			  futureNxt, futureIndex, futureMaxEntries,
			  *futureAdditional, spl);
  }
}

/*---------------------------------------------------------------------------*/

static indexType
doubleTest (h, triEdge, p, time, nxt, index, maxEntries, additional, spl)

     void *h;
     indexType triEdge; /* triEdge is such that the flipped Edge is */
			/* pONEXT (HPQ, triEdge, time) */
     indexType p;
     indexType time;
     nextRecordType *nxt;
     indexType index;
     indexType maxEntries; 
     additionalNextRecordsType * additional; 
     int *spl;

{
  nextRecordType *otherNxt;
  indexType otherIndex;
  indexType otherMaxEntries;
  additionalNextRecordsType ** otherAdditional;
  double dummy;

/*  printf ("double test\n");*/
/*printPersistQe (&(H->pe[pSYM(triEdge)]), pSYM(triEdge));
  printPersistQe (&(H->pe[triEdge]), triEdge);
*/
  if ((H->leftTurnTest) (h, pDST (HPQ, nxt[index].edge), pORG (HPQ, triEdge), 
		       p, &dummy))
    return doLocatePoint (h, triEdge, p, time, nxt, index, maxEntries,
			  additional, spl);

  pqeGetPresent (HPQ, nxt[index].edge, time, &otherNxt, &otherIndex,
		 &otherMaxEntries,  &otherAdditional);

  return doLocatePoint (h, nxt[index].edge, p, time, otherNxt,
			otherIndex, otherMaxEntries, *otherAdditional, spl);
}

/*---------------------------------------------------------------------------*/

static indexType
tripleTest (h, triEdge, p, time, nxt, index, maxEntries, additional, spl)

     void *h;
     indexType triEdge; /* triEdge is an edge of the boundary of the */
			/* triangle which contains the three */
			/* sub-triangles. triEdge is in ccw order on */
			/* the boundary. actually is on the boundary */
			/* of one of the subtriangles since here we */
			/* are already in the future. */
     indexType p;
     indexType time;
     nextRecordType *nxt;
     indexType index;
     indexType maxEntries;
     additionalNextRecordsType * additional;
     int *spl;

{
  nextRecordType *otherNxt;
  indexType otherIndex;
  indexType otherMaxEntries;
  additionalNextRecordsType ** otherAdditional;
  double dummy;

/*  printf ("triple test\n");*/

  if ((H->leftTurnTest) (h, pDST (HPQ, nxt[index].edge), pORG (HPQ, triEdge), 
			 p, &dummy))
    if ((H->leftTurnTest) (h, pDST (HPQ, triEdge), pDST (HPQ, nxt[index].edge), 
			   p, &dummy))
      return doLocatePoint (h, triEdge, p, time, nxt, index, maxEntries,
			    additional, spl);
    else {
      pqeGetPresent (HPQ, pONEXT(HPQ, pSYM(nxt[index].edge), time), time, 
		     &otherNxt, &otherIndex, &otherMaxEntries, 
		     &otherAdditional);
      return doLocatePoint (h, pONEXT(HPQ, pSYM(nxt[index].edge), time), p,
			    time, otherNxt, otherIndex, otherMaxEntries,
			    *otherAdditional, spl);
    }
  else
    if ((H->leftTurnTest) (h, pDST (HPQ, nxt[index].edge), 
			   pDST (HPQ, pONEXT (HPQ, nxt[index].edge, time)), 
			   p, &dummy)) {
      pqeGetPresent (HPQ, nxt[index].edge, time, &otherNxt,
		     &otherIndex, &otherMaxEntries, &otherAdditional);
      return doLocatePoint (h, nxt[index].edge, p, time, otherNxt,
			    otherIndex, otherMaxEntries, *otherAdditional, spl);
    } else {
      pqeGetPresent (HPQ, pONEXT(HPQ, pSYM(nxt[index].edge), time), time, 
		     &otherNxt, &otherIndex, &otherMaxEntries, 
		     &otherAdditional);
      return doLocatePoint (h, pONEXT(HPQ, pSYM(nxt[index].edge), time), p,
			    time, otherNxt, otherIndex, otherMaxEntries,
			    *otherAdditional, spl);  
    }
}

/*---------------------------------------------------------------------------*/

indexType
hdagInsertPoint (h, p, spl)

     void *h;
     indexType p;
     int *spl;

/* inserts a point into the hdag. it returns the index of one of the */
/* quadedges inserted. the orientation of this edge is from one of the */
/* vertices of the triangle containing p to p. the return value is */
/* null when p cannot be inserted (due to 'localTerminationCriterion') */

{
  indexType triEdge;
  double dummy;

  triEdge = 
    hdagLocatePoint (h, p, spl);

  if (triEdge >= 0) 
    return pqeAddTriangleStar (HPQ, triEdge, p);
  else
    return NULL;
}

/*---------------------------------------------------------------------------*/

void *
hdagNew (n, sites, localLeftTurnTest, localTerminationCriterion)

     indexType n;
     void *sites;
     int (*localLeftTurnTest) ();
     int (*localTerminationCriterion) ();

{
  hdagType *tmp;

  tmp = MALLOC (hdagType, 1);
  siIsSItype (sites);
  tmp->s = sites;
  tmp->leftTurnTest = localLeftTurnTest;
  tmp->terminationCriterion = localTerminationCriterion;
  tmp->magic = HDAGMAGIC;

  tmp->pq = pqeNew (n);

  return (void *) tmp;
}

/*---------------------------------------------------------------------------*/

void
hdagDispose (h)

     void *h;

{
  pqeDispose (hdagPQ (H));
  FREE (h);
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
