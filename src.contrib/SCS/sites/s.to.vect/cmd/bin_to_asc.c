/*  @(#)bin_to_asc.c    2.1  6/26/87  */

#include "digit.h"

bin_to_asc(binary, ascii)
    FILE *binary, *ascii;
{
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

	switch(type)
	{
	case AREA:
	    fprintf(ascii, "A  %d\n", Points.n_points);
	    break;
	case LINE:
	    fprintf(ascii, "L  %d\n", Points.n_points);
	    break;
	case DOT:
	    fprintf(ascii, "P  %d\n", Points.n_points);
	    break;
	case DEAD_AREA:
	    fprintf(ascii, "a  %d\n", Points.n_points);
	    break;
	case DEAD_LINE:
	    fprintf(ascii, "l  %d\n", Points.n_points);
	    break;
	case DEAD_DOT:
	    fprintf(ascii, "p  %d\n", Points.n_points);
	    break;
	default:
	    fprintf(ascii, "X  %d\n", Points.n_points);
/*DEBUG*/	    fprintf (stderr, "got type %d\n", (int) type);
	    break;
	}

	xptr = Points.x;
	yptr = Points.y;
	while (Points.n_points--)
	    fprintf(ascii, " %12.2lf %12.2lf\n", *yptr++, *xptr++);
    }

done:
    return (0);
}
