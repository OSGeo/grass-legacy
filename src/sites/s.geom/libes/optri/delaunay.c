
#include "internoptri.h"

/*---------------------------------------------------------------------------*/

/*                     DELAUNAY TRIANGULATION */

/*---------------------------------------------------------------------------*/

int
incirc (g, qe)

     graphType * g;
     indexType qe;

{
  double dummy;

  return (int) sosrInCircle (g, DST(g, qe), DST(g, ONEXT(g, qe)), ORG(g, qe),
			     DST(g, ONEXT(g, SYM(qe))), &dummy);
}

/*---------------------------------------------------------------------------*/

void     
buildDelaunayTriangulation (dt, printInfo, visual, 
			    nofFlips, nofAttempts, runTime)

     graphType *dt;
     int printInfo;
     visualType * visual;
     int * nofFlips, * nofAttempts;
     double * runTime;

{
  if (printInfo)
    printf ("Delaunay: "); (void) fflush(stdout);

  lawsonFlip (dt, visual, queue (), incirc, (void *) 0,
	      nofFlips, nofAttempts, runTime);
  printf ("completed, cpu used ... %f,\n", *runTime);
  if (printInfo) 
    printf ("                       # flips ... %d, #attempts %d.\n\n",
	    *nofFlips, *nofAttempts);
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

