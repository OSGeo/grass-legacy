#include "all.h"

/*******************************************************************/
void dsyot(t,ynew,dltype,lowspot,highspot,tdrain,normout)
/*
 *    THIS SUBROUTINE CALCULATES THE DOWNSTREAM DISCHARGE BOUNDARY
 *    CONDITION Y(T)
 */

   float t;
   double *ynew,lowspot,highspot,tdrain,normout;
   int   dltype;

{
   float lakelev,highelev,lowelev,ystep;
   lakelev=217.28;
   if(dltype==0) 
      {
      /* this is the outlet */
      highelev=highspot;
      lowelev=lowspot+normout;
      ystep=highelev-lowelev;
      }
   if(dltype==4)
      {
      /* this is a lake */
      highelev=237.0;
      lowelev=lakelev;
      ystep=highelev-lowelev;
      }
   if(t<=tdrain)
      {
      *ynew=highelev-ystep/tdrain*t;
      }
   else
      {
      *ynew=lowelev;
      }

/*   fprintf(stderr,"ynew=%f\n",*ynew);
 *   fflush(stderr);
 */
   return;
}
