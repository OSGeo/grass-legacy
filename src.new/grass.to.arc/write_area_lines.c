
#include "digit.h"

write_area_lines(binary, ascii)
    FILE *binary, *ascii;
{
    int count=0;
    int type;
    double *xptr, *yptr;
    static int alloc_points;
    static struct line_pnts Points;
    long offset;

    /* set up for alloc_points()  called by Read_line() */
    if (alloc_points == 0)
    {
	alloc_points = -1;	/* will never get here again */
	Points.alloc_points = 0;
    }

    while(1)
    {
	offset = ftell (binary);
	if (-1 == (type = dig__Read_line (&Points, binary, offset)))
	    return (-1);
	if (type == -2)	/* EOF */
	    goto done;
        else if (type==AREA)
           {
	   fprintf(ascii,"%d\n",++count);
           xptr = Points.x;
	   yptr = Points.y;
	   while (Points.n_points--)
	       fprintf(ascii, " %12.2lf %12.2lf\n", *xptr++, *yptr++);
           fprintf(ascii,"END\n");
           }
    }

done:
    {
    fprintf(ascii,"END\n");
    return (0);
    }
}
