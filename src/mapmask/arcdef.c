/* %W% %G% */
#include "mapmask.h"

arcdef(radius,row,col,number)
    int radius,*number;
    int row[],col[];
{
    int i;
    register double atx,aty,Radius2;
    double sqrt();

    *number = 0;
    Radius2 = radius * radius;

    aty = 0.0;
    for(atx = (double)radius; atx >= aty; i++)
    {
	*(col + *number) = (int)(atx+.5);
	*(row + *number) = (int)(aty+.5);
	(*number)++;
	aty += 1.0;
	atx = sqrt(Radius2-aty*aty);
    }
}
