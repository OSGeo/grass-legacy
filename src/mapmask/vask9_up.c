/* %W% %G% */
#include "mapmask.h"

vask9_up(start,stop)
    int start,stop;
{
    int i,j,k,num,line[10],corner[10];
    char coordask[10][60];

    num = stop - start + 1;

    i = 0;
    for(j = start; j <= stop; j++)
    {
	corner[i] = j + 1;
	i++;
    }

    if(num < 0 || num > 10) exit(-1);

    for(k = 0; k < num; k++)
    {
	sprintf(coordask[k],
	    "          corner %2d Easting:              Northing:", corner[k]);
	V_line(k+8,coordask[k]);
    }
}
