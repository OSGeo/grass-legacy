#include "internoptri.h"

/*--------------------------------------------------------------------------*/

#define NOT_A_FLIPPABLE_EDGE(g,edge) \
        (grISCHedge (g, edge) || grISCONSTRedge (g, edge))

/*--------------------------------------------------------------------------*/

static queuesType *initializePool (graphType *g, poolType *pool,
  int (*GTfunction)(void))
{
  int i;
  queuesType *tree;

  tree = (*pool->create) (NE(g), g, GTfunction, POOL_FIXED_SIZE);

  for (i = 0; i < NE(g); i++)
    if (! NOT_A_FLIPPABLE_EDGE (g, i))
      (*pool->insert) (tree, i);

  return tree;
}

/*--------------------------------------------------------------------------*/

void lawsonFlip (graphType *g, visualType *visual, poolType *pool,
  int (*flipable)(graphType *,indexType), int (*GTfunction)(void), int *nofFlips,
  int *nofAttempts, double *runTime)

{
  int      foundEdge;
  indexType qe, i;
  indexType candidateEdge[4];
  queuesType *edges;
  indexType edge;

  fprintf (stdout,"Lawson Flip:  ");  (void) fflush (stdout);

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
