
#include "internoptri.h"

/*--------------------------------------------------------------------------*/

/*                     MINMAX ANGLE TRIANGULATION */

/*--------------------------------------------------------------------------*/

void
buildMinMaxAngleTriangulation (mm, printInfo, visual, 
			       nofFlips, nofAttempts, runTime)

     graphType *mm;
     int printInfo;
     visualType * visual;
     int * nofFlips, * nofAttempts;
     double * runTime;

{
  ANGLEMAKETIME = 0;
  NOFANGLECOMPARISONS = 0;

  printf ("MinMax Angle: "); (void) fflush(stdout);

  edgeInsertionN2LOGN (mm, visual, heap (), haAngleGT, 
		       GT_CAN_DECIDE_RIGHT_TURN, haAngleGT180,
		       haPrintAngle, nofFlips, nofAttempts, runTime);

  printf ("Time used for angle comparisons: %f\n", ANGLEMAKETIME);
  if (printInfo) 
    printf ("Number of Angle Comparisons: %d\n",   NOFANGLECOMPARISONS);
}

/*--------------------------------------------------------------------------*/

void     
buildMinMaxAngleTriangulation2 (mm, printInfo, visual, 
				nofFlips, nofAttempts, runTime)

     graphType *mm;
     int printInfo;
     visualType * visual;
     int * nofFlips, * nofAttempts;
     double * runTime;

{
  ANGLEMAKETIME = 0;
  NOFANGLECOMPARISONS = 0;

  printf ("MinMax Angle: "); (void) fflush(stdout);

  edgeInsertionN3 (mm, visual, heap (), haAngleGT, 
		   GT_CAN_DECIDE_RIGHT_TURN, haAngleGT180,
		   haPrintAngle, nofFlips, nofAttempts, runTime);

  printf ("Time used for angle comparisons: %f\n", ANGLEMAKETIME);
  if (printInfo) 
    printf ("Number of Angle Comparisons: %d\n",   NOFANGLECOMPARISONS);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
