
#include "internoptri.h"

/*--------------------------------------------------------------------------*/

/*                     MINMAX HEIGHT TRIANGULATION */

/*--------------------------------------------------------------------------*/

void 
buildMaxMinHeightTriangulation (graphType *mm, int printInfo, visualType *visual, int *nofFlips, int *nofAttempts, double *runTime)

{
  ANGLEMAKETIME = 0;
  NOFANGLECOMPARISONS = 0;

  fprintf (stdout,"MaxMin Height: "); (void) fflush(stdout);

  edgeInsertionN2LOGN (mm, visual, heap (), hhHeightLT, 
		       GT_CAN_DECIDE_RIGHT_TURN, hhHeightLT0, 
		       hhPrintHeight, nofFlips, nofAttempts, runTime);

  fprintf (stdout,"Time used for height comparisons: %f\n", ANGLEMAKETIME);
  if (printInfo) 
    fprintf (stdout,"Number of Height Comparisons: %ld\n",   NOFANGLECOMPARISONS);
}

/*--------------------------------------------------------------------------*/
    
void 
buildMaxMinHeightTriangulation2 (graphType *mm, int printInfo, visualType *visual, int *nofFlips, int *nofAttempts, double *runTime)

{
  ANGLEMAKETIME = 0;
  NOFANGLECOMPARISONS = 0;

  fprintf (stdout,"MaxMin Height: "); (void) fflush(stdout);

  edgeInsertionN3 (mm, visual, heap (), hhHeightLT,
		   GT_CAN_DECIDE_RIGHT_TURN, hhHeightLT0, 
		   hhPrintHeight, nofFlips, nofAttempts, runTime);

  fprintf (stdout,"Time used for height comparisons: %f\n", ANGLEMAKETIME);
  if (printInfo) 
    fprintf (stdout,"Number of Height Comparisons: %ld\n",   NOFANGLECOMPARISONS);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
