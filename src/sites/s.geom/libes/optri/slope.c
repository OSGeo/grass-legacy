
#include "internoptri.h"

/*--------------------------------------------------------------------------*/

/*                     MINMAX SLOPE TRIANGULATION */

/*--------------------------------------------------------------------------*/

void
buildMinMaxSlopeTriangulation (mm, printInfo, visual, 
			       nofFlips, nofAttempts, runTime)

     graphType *mm;
     int printInfo;
     visualType * visual;
     int * nofFlips, * nofAttempts;
     double * runTime;

{
  ANGLEMAKETIME = 0;
  NOFANGLECOMPARISONS = 0;

  printf ("MinMax Slope: "); (void) fflush(stdout);

  edgeInsertionN3 (mm, visual, heap (), hsSlopeGT, 
		   GT_CANNOT_DECIDE_RIGHT_TURN, hsSlopeRightTurn,
		   hsPrintSlope, nofFlips, nofAttempts, runTime);

  printf ("Time used for slope comparisons: %f\n", ANGLEMAKETIME);
  if (printInfo) 
    printf ("Number of Slope Comparisons: %d\n",   NOFANGLECOMPARISONS);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

