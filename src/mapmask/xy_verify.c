/* %W% %G% */
#include "mapmask.h"

xy_verify(n,err_num)
    int n,err_num;
{
    int i,j,t;
    extern double *Ux,*Uy;

    if(n == -1) return(-2); /* flag circle specification    */

    *(Ux+n) = *Ux;

    if(err_num >= 4)
	    exit(-1);

    if(n < 3)
    {
	G_clear_screen() ;
	fprintf(stderr,"\n\n\n\n\n\n\n\n\n");
	fprintf(stderr,"Need Three or More Sides to Define a Polygon\n");
	fprintf(stderr,"Try Again");
	system("sleep 10");

	for(j = 0; j <= n; j++)
	{
	    *(Ux+j) = 0;
	    *(Uy+j) = 0;
	}
	return(-1);
    }
    return(1);
}
