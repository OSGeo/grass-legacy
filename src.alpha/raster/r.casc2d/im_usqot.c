#include "all.h"

/*******************************************************************/
 void usqot(t,qus,j,qmin,yes_drain)

/*     
 *    GIVEN ARRAYS OF Q(T) FOR ALL FIRST ORDER STREAMS, THIS SUBROUTINE
 *    LINEARLY INTERPOLATES Q(T) VALUES FOR INTERMEDIATE TIMES.
 */

     float t,*qus,qmin;
     int   j;
     int   yes_drain;
{
     float tpeak,tplat,qmax,tmax;

     qmax=10.0;

     /* change the logic here to put in an inflow hydrograph on 1st order */
     /* streams.                                                          */
/**************************
     if(yes_drain)
	{
	if(t<=200.0)
	   {
           *qus=qmin+qmax*t/200.0;
	   }
        else
	   {
	   if(t<=600.0)
	      {
	      *qus=qmax;
	      }
           else
	      {
	      *qus=(qmin+qmax)+(t-600.0)*(qmax/(600.0-800.0));
	      if(*qus<qmin) *qus=qmin;
	      }
           }
        }
     else
	{
	*qus=qmin;
	}
*********************/

     *qus=qmin;
     return;
}
