/*  @(#)plot.c    2.1  6/26/87  */
/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "wind.h"

/*
#define LINE    0
#define AREA    1
#define DOT    2
*/

nplot_points (type, Points, line_color, N1, N2, pnts)
    char type;
    struct line_pnts *Points;
    int    line_color;
    int    N1, N2;		/* end point colors */
    int pnts;
{
    double *xptr, *yptr;
    int i;

    if (type == LINE || type == AREA)
    {
	if (line_color != 0)
	{
	    R_standard_color (dcolors[line_color]);
	    xptr = Points->x;
	    yptr = Points->y;

	    First (xptr++, yptr++);

	    for (i = Points->n_points-1 ; i > 0 ; i--)
	    {
		Next (xptr++, yptr++);
	    }
	}

	if (pnts != 0)
	{
	    R_standard_color( dcolors[pnts]);
	    xptr = Points->x;
	    yptr = Points->y;
	    for(i=0 ; i < Points->n_points ; i++)
		Dot(xptr++, yptr++);
	    /*
	    for(i=Points->n_points-1; i>1; i--)
		Dot(xptr++, yptr++);
	    */
	}

	if (N1 != 0)
	{
	    R_standard_color( dcolors[N1]);
	    _Blot(Points->x, Points->y);
	    if (N2 != N1)
		R_standard_color( dcolors[N2]);
	    _Blot(Points->x + Points->n_points - 1, Points->y + Points->n_points - 1);
	}
    }
    else
    {
	if (type == DOT)
	{
	    double x1, y1;
	    double x2, y2;
	    int sx1, sy1;
	    int sx2, sy2;

	    x1 = Points->x[0];
	    y1 = Points->y[0];

	    /* remember to check Disp_lines */

	    R_standard_color( dcolors[line_color]);
	    _BigBlot (&x1, &y1);
	}
	else
	{
	    /*DEBUG*/ debugf ("nplot:  NOT LINE OR AREA\n");
	    R_standard_color (dcolors[N1]);
	    xptr = Points->x;
	    yptr = Points->y;
	    for(i = Points->n_points ; i > 0 ; i--)
		_Blot (xptr++, yptr++);
	}
    }
}
