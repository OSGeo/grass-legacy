#include "internoptri.h"

/*--------------------------------------------------------------------------*/

/*                     MINMAX SLOPE TRIANGULATION */

/*--------------------------------------------------------------------------*/

void buildMinMaxSlopeTriangulation (
  graphType *mm, int printInfo, visualType *visual,
  int *nofFlips, int *nofAttempts, double *runTime)

{
  ANGLEMAKETIME = 0.0L;
  NOFANGLECOMPARISONS = 0L;

  fprintf (stdout,"MinMax Slope: "); (void) fflush(stdout);

  edgeInsertionN3 (mm, visual, heap (), hsSlopeGT, 
		   GT_CANNOT_DECIDE_RIGHT_TURN, hsSlopeRightTurn,
		   hsPrintSlope, nofFlips, nofAttempts, runTime);

  fprintf (stdout,"Time used for slope comparisons: %f\n", ANGLEMAKETIME);
  if (printInfo) 
    fprintf (stdout,"Number of Slope Comparisons: %ld\n", NOFANGLECOMPARISONS);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

