/* %W% %G% */
#include "mapmask.h"

flip_flop(start,end,axis)
    int start,end,axis;
{
    int x_adj,y_adj,i,j;
    extern double *Ux,*Uy;

    if(axis == 0)
    {
	x_adj = -1;
	y_adj = 1;
    }
    else
    {
	x_adj = 1;
	y_adj = -1;
    }

    i = 0;
    for(j = start; j > end; j--)
    {
	*(Ux+j) = *(Ux+i) * (float)x_adj;
	*(Uy+j) = *(Uy+i) * (float)y_adj;
	i++;
    }
}
