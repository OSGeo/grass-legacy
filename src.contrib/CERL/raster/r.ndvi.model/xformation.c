#include <stdio.h>
#include <math.h>					
#include <signal.h>					

					/* NDVI form */
ndvi(xhong,nxhong,ndata,x,nx)
        double *xhong,**x;
        int *nxhong,*nx,*ndata;
{
        double xtemp1,xtemp2;
        int i,k;
        double *vector();
					/* check input existance 
printf("in ndvi: ndata=%d,nxhong=%d\n",*ndata,*nxhong);
for(i=0;i<(*ndata);i++)
{
  for(k=0;k<*nxhong;k++)
  {
  printf("in ndvi: i=%d, k=%d\n",i,k);
  printf("in ndvi: xhong[%d]=%10.4f\n",i*(*nxhong)+k,
			*(xhong+i*(*nxhong)+k));
  }
}
					*/

*nx=1;
*x = vector(*nx*(*ndata));
if(*nxhong == 2)
        {
	for(i=0;i<*ndata;i++)
		{
		xtemp1= *((xhong)+i*(*nxhong)+1);
		xtemp2= *((xhong)+i*(*nxhong));
		*((*x)+i)=(xtemp1-xtemp2)
			/(xtemp1+ xtemp2);
/*
                printf("x[%d]=%10.4f\n",i,*(*x+i));
*/
                }
        }
if(*nxhong == 3)
        {
        for (i=0;i<*ndata;i++)
		{
		xtemp1= *((xhong)+i*(*nxhong)+2);
		xtemp2= *((xhong)+i*(*nxhong))
			+(*((xhong)+i*(*nxhong)+1));
		*((*x)+i)=(xtemp1-xtemp2)
			/(xtemp1+ xtemp2);
/*
                printf("x[%d]=%10.4f\n",i,*(*x+i));
*/
                }
        }
}
