
#include "all.h"

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
void RAIN_SQ_DIS(space,nrg,grow,gcol,rgint,rint)
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

/* RAINGAGE RAINFALL INTENSITY UNITS ARE IN INCHES PER HOUR. */

int     nrg;
int     **space;
int     *grow,*gcol;
double  *rgint;
float   *rint;

{
int     j,k,ig,vectmp;
double  sqrt();
double  dist,totdist,totrain,rowdif,coldif;

for(j=0; j<nrows; j++)
{
   for(k=0; k<ncols; k++)
   {

      vectmp=space[j][k];
      if(vectmp==0) continue;
      if(nrg ==1)
      {
		/* Assume uniform rainfall */
         rint[vectmp]=rgint[0]*2.54/(100.*3600.);
         continue;
      }
      else
      {
         totdist=0;
         totrain=0;
         rint[vectmp]=0;
         for(ig=0; ig<nrg; ig++)
         {
            rowdif=(double)(grow[ig]-j);
            coldif=(double)(gcol[ig]-k);
            dist=sqrt((double)(rowdif*rowdif+coldif*coldif));
            if(dist<1e-3) 
	    {
	       totdist=1;
	       totrain=rgint[ig];
               break;
            }
	    else
	    {
               totdist=totdist+(1.0/(dist*dist));
               totrain=totrain+rgint[ig]/(dist*dist);
            }
         }

         rint[vectmp]=totrain/totdist;
         rint[vectmp]=rint[vectmp]*2.54*0.01/3600.;

      }
   }
}

return;
}

