
#include "internoptri.h"

/*--------------------------------------------------------------------------*/

#define NOT_A_FLIPPABLE_EDGE(g,edge) \
        (grISCHedge (g, edge) || grISCONSTRedge (g, edge))

/*--------------------------------------------------------------------------*/

static queuesType *
initializePool(g, pool, GTfunction)

     graphType *g;
     poolType  *pool;
     int      (*GTfunction) ();
{
  int i;
  queuesType * tree;
  indexType qe;

  tree = (*pool->create) (NE(g), g, GTfunction, POOL_FIXED_SIZE);

  for (i = 0; i < NE(g); i++)
    if (! NOT_A_FLIPPABLE_EDGE (g, i))
      (*pool->insert) (tree, i);

  return tree;
}

/*--------------------------------------------------------------------------*/

void
lawsonFlip (g, visual, pool, flipable, GTfunction,
	    nofFlips, nofAttempts, runTime)

     graphType  *g;
     int        *nofFlips, *nofAttempts;
     poolType   *pool;
     int       (*flipable) (), (*GTfunction) ();
     visualType *visual;
     double * runTime;

{
  int      foundEdge;
  indexType qe, i;
  indexType candidateEdge[4];
  queuesType *edges;
  indexType edge;

  printf ("Lawson Flip:  ");  (void) fflush (stdout);

  *runTime = get_user_time ();
  edges = initializePool (g, pool, GTfunction);

  *nofFlips = 0;
  *nofAttempts = 0;

  do {
    foundEdge = (*pool->top) (edges, &edge);

    if (foundEdge) {

      (*pool->delete) (edges, edge);


      qe = grMAKEQE (edge);

      if ((*flipable) (g, qe)) {

	candidateEdge[0] = ONEXT(g, qe);
	candidateEdge[1] = OPREV(g, qe);
	candidateEdge[2] = ONEXT(g, SYM(qe));
	candidateEdge[3] = OPREV(g, SYM(qe));
	for (i = 0; i < 4; i++) {
	  edge = grQETOE(candidateEdge[i]);
	  if (! (NOT_A_FLIPPABLE_EDGE (g, edge) ||
		 ((*pool->doesContain) (edges, edge)))) {
	    (* pool->insert) (edges, edge);
	  }
        }

	(*visual->eraseEdge) (g, visual, qe);
INTERRUPT_ALGORITHM;	

	grFlipEdge (g, qe);

	(*visual->drawEdge) (g, visual, qe, 1);
INTERRUPT_ALGORITHM;	

	(*nofFlips)++;

      } else {
	(*nofAttempts)++;
      }
    }

  } while (foundEdge);

INTERRUPT_LABEL: ;

  (*pool->dispose) (edges);
  if (! SHOULD_INTERRUPT_ALGORITHM) {
    *runTime = get_user_time () - *runTime;
  }
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
