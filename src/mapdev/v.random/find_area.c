/*  @(#)find_area.c    2.1  6/26/87  */
#include <stdlib.h>
#include <math.h>
#include "Vect.h"
#include "dots.h"

/*#define DEBUG*/

int tmp_find_area (
    struct Map_info *map,
    P_AREA *Area,
    double *totalarea,
    double *cent_x,double *cent_y,
    double *n,double *e,double *w,double *s)
{
    int cur_line;
    int ab_line;
    int i;
    double area;
    double *xptr1, *yptr1;
    double *xptr2, *yptr2;
    double cent_weight_x, cent_weight_y;
    double len, tot_len;
    static struct line_pnts points;
    static int first_time;	/* 0 on startup */

#ifdef DEBUG
fprintf (stdout,"find_area:\n");
fprintf (stdout,"Area->n_lines= %ld\n",Area->n_lines);
#endif

    if (! Area->n_lines)
	return(-1);

    if (first_time == 0)
    {
	points.alloc_points = 0;
	first_time = -1;
    }

    *totalarea = 0.0;
    tot_len = 0.0;
    cent_weight_x = 0.0;
    cent_weight_y = 0.0;

    for(cur_line = 0; cur_line < Area->n_lines ; cur_line++)
    {
	ab_line = abs(Area->lines[cur_line]);
                                 /* read line */
	V2_read_line (map, &points, ab_line);
	
	area = 0.0;

	xptr1 = points.x;
	yptr1 = points.y;
	xptr2 = points.x + 1;
	yptr2 = points.y + 1;

	for(i=1; i<points.n_points; i++)
	{
            if (*yptr1 > *n) *n = *yptr1;
            if (*xptr1 > *e) *e = *xptr1;
            if (*yptr1 < *s) *s = *yptr1;
            if (*xptr1 < *w) *w = *xptr1;
	    area += (*xptr2 - *xptr1) * ((*yptr2 + *yptr1) / 2.0 );
	    len = hypot(*xptr1-*xptr2, *yptr1-*yptr2);

#ifdef DEBUG
fprintf (stdout,"xptr1= %.2lf, xptr2= %.2lf, len= %.2lf\n",*xptr1,*xptr2,len);
#endif

	    cent_weight_x += len * ((*xptr1 + *xptr2) / 2.);
	    cent_weight_y += len * ((*yptr1 + *yptr2) / 2.);
	    tot_len += len;

#ifdef DEBUG
fprintf (stdout,"centrdx= %.2lf, tot_len= %.2lf\n",cent_weight_x,tot_len);
#endif

	    xptr1++ ; xptr2++ ; yptr1++; yptr2++;
	}

	if (Area->lines[cur_line] > 0)
	    *totalarea += area;
	else
	    *totalarea -= area;

    }

    if (tot_len != 0.0)
    {
	*cent_x = cent_weight_x / tot_len;
	*cent_y = cent_weight_y / tot_len;
    }

#ifdef DEBUG
fprintf (stdout,"\n\n");
#endif

    return(0);
}
