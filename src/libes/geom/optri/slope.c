#include "internoptri.h"

/*--------------------------------------------------------------------------*/

/*                     MINMAX SLOPE TRIANGULATION */

/*--------------------------------------------------------------------------*/

void buildMinMaxSlopeTriangulation (
  graphType *mm, int printInfo, visualType *visual,
  int *nofFlips, int *nofAttempts, double *runTime)

{
  ANGLEMAKETIME = 0;
  NOFANGLECOMPARISONS = 0;

  fprintf (stdout,"MinMax Slope: "); (void) fflush(stdout);

  edgeInsertionN3 (mm, visual, heap (), hsSlopeGT, 
		   GT_CANNOT_DECIDE_RIGHT_TURN, hsSlopeRightTurn,
		   hsPrintSlope, nofFlips, nofAttempts, runTime);

  fprintf (stdout,"Time used for slope comparisons: %d\n", ANGLEMAKETIME);
  if (printInfo) 
    fprintf (stdout,"Number of Slope Comparisons: %d\n",   NOFANGLECOMPARISONS);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

