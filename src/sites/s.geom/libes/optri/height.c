
#include "internoptri.h"

/*--------------------------------------------------------------------------*/

/*                     MINMAX HEIGHT TRIANGULATION */

/*--------------------------------------------------------------------------*/

void     
buildMaxMinHeightTriangulation (mm, printInfo, visual, 
				nofFlips, nofAttempts, runTime)

     graphType *mm;
     int printInfo;
     visualType * visual;
     int * nofFlips, * nofAttempts;
     double * runTime;

{
  ANGLEMAKETIME = 0;
  NOFANGLECOMPARISONS = 0;

  printf ("MaxMin Height: "); (void) fflush(stdout);

  edgeInsertionN2LOGN (mm, visual, heap (), hhHeightLT, 
		       GT_CAN_DECIDE_RIGHT_TURN, hhHeightLT0, 
		       hhPrintHeight, nofFlips, nofAttempts, runTime);

  printf ("Time used for height comparisons: %f\n", ANGLEMAKETIME);
  if (printInfo) 
    printf ("Number of Height Comparisons: %d\n",   NOFANGLECOMPARISONS);
}

/*--------------------------------------------------------------------------*/
    
void 
buildMaxMinHeightTriangulation2 (mm, printInfo, visual, 
				 nofFlips, nofAttempts, runTime)

     graphType *mm;
     int printInfo;
     visualType * visual;
     int * nofFlips, * nofAttempts;
     double * runTime;

{
  ANGLEMAKETIME = 0;
  NOFANGLECOMPARISONS = 0;

  printf ("MaxMin Height: "); (void) fflush(stdout);

  edgeInsertionN3 (mm, visual, heap (), hhHeightLT,
		   GT_CAN_DECIDE_RIGHT_TURN, hhHeightLT0, 
		   hhPrintHeight, nofFlips, nofAttempts, runTime);

  printf ("Time used for height comparisons: %f\n", ANGLEMAKETIME);
  if (printInfo) 
    printf ("Number of Height Comparisons: %d\n",   NOFANGLECOMPARISONS);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
