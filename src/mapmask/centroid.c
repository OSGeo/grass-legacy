/* %W% %G% */
#include "mapmask.h"

centroid(radius,sides,x_center,y_center)
    int x_center,y_center,radius,sides;
{
    int r2,r4,i,sector_pts;
    float arc,PI;
    double cos(),sin();
    extern double *Ux,*Uy;

    PI = 3.14159;

    r2 = sides % 2;
    if(r2 == 0)
    {
	r4  = sides % 4;
	arc = PI/sides;
	*Ux  = cos((double)arc) * (double)radius;
	*Uy  = sin((double)arc) * (double)radius;
	if(r4 == 0)
	{
	    sector_pts = sides / 4;
	    for(i = 1; i < sector_pts; i++)
	    {
		arc = ((i*2*PI)+PI)/sides;
		*(Ux+i) = cos((double)arc) * (double)radius;
		*(Uy+i) = sin((double)arc) * (double)radius;
	    }
	    flip_flop((2*sector_pts-1),(sector_pts-1),0);
	    flip_flop((sides-1),(2*sector_pts-1),1);
	}
	else
	{
	    sector_pts = sides / 2;
	    for(i = 1; i < sector_pts; i++)
	    {
		arc = ((i*2*PI)+PI)/sides;
		*(Ux+i) = cos((double)arc) * (double)radius;
		*(Uy+i) = sin((double)arc) * (double)radius;
	    }
	    flip_flop((sides-1),(sector_pts-1),1);
	}
    }
    else
    {
	*Ux = 0;
	*Uy = (double)radius;
	for(i = 1; i < sides; i++)
	{
	    arc = PI/2;
	    arc -= (2 * i * PI)/sides;
	    *(Ux+i) = cos((double)arc) * (double)radius;
	    *(Uy+i) = sin((double)arc) * (double)radius;
	}
    }

    for(i = 0; i < sides; i++)
    {
	*(Ux+i) += (double)x_center;
	*(Uy+i) += (double)y_center;
    }
} 
