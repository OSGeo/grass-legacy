
#include "all.h"

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
void RAIN_THIESSEN(space,nrg,grow,gcol,rgint,rint)
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

/* RAINGAGE RAINFALL INTENSITY UNITS ARE IN INCHES PER HOUR. */

int     nrg;
int     **space;
int     *grow,*gcol;
double  *rgint;
float   *rint;

{
int     j,k,ig,vectmp;
int     mingage;
double  sqrt();
double  dist,mindist,rowdif,coldif;

for(j=0; j<nrows; j++)
{
   for(k=0; k<ncols; k++)
   {
      vectmp=space[j][k];
      if(vectmp==0) continue;
      if(nrg==1)
      {
		   /* Assume uniform rainfall */
         rint[vectmp]=rgint[0]*2.54/(100.*3600.);
	 continue;
      }
      mingage=0;
      rint[vectmp]=0;
      for(ig=0; ig<nrg; ig++)
      {
         rowdif=(double)(grow[ig]-j);
         coldif=(double)(gcol[ig]-k);

         dist=sqrt((double)(rowdif*rowdif+coldif*coldif));
         if(ig==0) mindist=dist;
         if(dist<mindist) 
	 {
            mingage=ig;
            mindist=dist;
         }
      }

      rint[vectmp]=rgint[mingage]*2.54/(100*3600.);
   }
}

return;
}

