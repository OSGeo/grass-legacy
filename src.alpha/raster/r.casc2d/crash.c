#include "all.h"

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
void CRASH(discharge_fd,coindex,vect,link,node,itime,
	   space,dt,window,Inter,h,vinf,yes_intercep,
	   yes_inf,vintercep,vsur2,yes_lake,vlake2,elev,vinftot)
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

int     discharge_fd,coindex,vect,itime;
int     link,node;
int     **space;
int     yes_intercep,yes_inf,yes_lake;
double  dt,*vintercep,*vsur2,*vlake2,*vinftot;
float   *Inter,*h,*vinf,*elev;
struct  Cell_head window;

{

int     j,k;
double  dj,dk,east,north;

for(j=0; j<nrows; j++)
{
   for(k=0; k<ncols; k++)
   {
       if(space[j][k] != vect) continue;
       dj=(double)j+0.5;
       dk=(double)k+0.5;
       east=G_col_to_easting(dk,&window);
       north=G_row_to_northing(dj,&window);

       if(coindex==1)
       {
          fprintf(discharge_fd, "program crashed due to oscillations resulting in negative depth at overland cell: \n row= %d \n col= %d \n northing= %lf \n easting= %lf \n time= %7.1f  (min).\n Time step must be reduced or the surface slopes in the neighborhood of above location be smoothed.\n",j,k,north,east,(float)itime*dt/60.);

          fprintf(stderr, "program crashed due to oscillations resulting in negative depth at overland cell: \n row= %d \n col= %d \n northing= %lf \n easting= %lf \n time= %7.1f  (min).\n Time step must be reduced or the surface slopes in the neighborhood of above location be smoothed.\n",j,k,north,east,(float)itime*dt/60.);
       }
       else
       {
          fprintf(discharge_fd, "program crashed due to oscillations resulting in negative channel depth at link= %d \n node= %d \n which is embedded in cell: \n row= %d \n col= %d \n northing= %lf \n easting= %lf \n time= %7.1f  (min).\n Time step must be reduced or the surface slopes in the neighborhood of above location be smoothed.\n",link,node,j,k,north,east,(float)itime*dt/60.);

          fprintf(stderr, "program crashed due to oscillations resulting in negative channel depth at link= %d \n node= %d \n which is embedded in cell: \n row= %d \n col= %d \n northing= %lf \n easting= %lf \n time= %7.1f  (min).\n Time step must be reduced or the surface slopes in the neighborhood of above location be smoothed.\n",link,node,j,k,north,east,(float)itime*dt/60.);
       }
    }
}

for(j=1; j<=GNUM; j++)
{
   if(yes_intercep) (*vintercep)=(*vintercep)+Inter[j]*w*w;
   if(yes_inf) (*vinftot)=(*vinftot)+vinf[j]*w*w;
   if(yes_lake)
   {
     if(lake_cat[j]!=0) 
     {
	(*vlake2)=(*vlake2)+(lake_el[lake_cat[j]]-(double)elev[j])*w*w;
     }
     else
     {
	(*vsur2)=(*vsur2)+h[j]*w*w;
     }
   }
   else
   {
      (*vsur2)=(*vsur2)+h[j]*w*w;
   }

}

return;
}
