
#include "internoptri.h"

/*--------------------------------------------------------------------------*/

/*                     MINMAX ANGLE TRIANGULATION */

/*--------------------------------------------------------------------------*/

void buildMinMaxAngleTriangulation (graphType *mm, int printInfo, visualType *visual, int *nofFlips, int *nofAttempts, double *runTime)

{
  ANGLEMAKETIME = 0;
  NOFANGLECOMPARISONS = 0;

  fprintf (stdout,"MinMax Angle: "); (void) fflush(stdout);

  edgeInsertionN2LOGN (mm, visual, heap (), haAngleGT, 
		       GT_CAN_DECIDE_RIGHT_TURN, haAngleGT180,
		       haPrintAngle, nofFlips, nofAttempts, runTime);

  fprintf (stdout,"Time used for angle comparisons: %f\n", ANGLEMAKETIME);
  if (printInfo) 
    fprintf (stdout,"Number of Angle Comparisons: %ld\n",   NOFANGLECOMPARISONS);
}

/*--------------------------------------------------------------------------*/

void 
buildMinMaxAngleTriangulation2 (graphType *mm, int printInfo, visualType *visual, int *nofFlips, int *nofAttempts, double *runTime)

{
  ANGLEMAKETIME = 0;
  NOFANGLECOMPARISONS = 0;

  fprintf (stdout,"MinMax Angle: "); (void) fflush(stdout);

  edgeInsertionN3 (mm, visual, heap (), haAngleGT, 
		   GT_CAN_DECIDE_RIGHT_TURN, haAngleGT180,
		   haPrintAngle, nofFlips, nofAttempts, runTime);

  fprintf (stdout,"Time used for angle comparisons: %f\n", ANGLEMAKETIME);
  if (printInfo) 
    fprintf (stdout,"Number of Angle Comparisons: %ld\n",   NOFANGLECOMPARISONS);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
