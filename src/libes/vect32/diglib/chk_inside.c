/*  @(#)chk_inside.c    2.1  6/26/87  */
/* check_inside()  determines if a point (X,Y) is inside an area
 *   described by the "num_lines" in the list "line_list".  If it is
 *   a positive floating point number is returned giving the distance
 *   to the area.  If not, "0.0" is returned.
 */
#include <math.h>
#include "Vect.h"

#ifndef HUGE_VAL
#define HUGE_VAL 9999999999999.0
#endif


double dig_point_in_area(struct Map_info *map, double X,double Y, P_AREA *Area)
{
    static int first_time;
    static struct line_pnts Points;
    double *x, *y;
    double cur_min;
    double cur_x, cur_y;
    double x_inter;
    double x_dist;
    int at_line;
    int n_intersects;
    int n_segs;
    int l;
    int n;
    P_LINE *Line;

    cur_min = HUGE_VAL;
    cur_x = 0.0;
    cur_y = 0.0;
    n_intersects = 0;

    if (first_time == 0)	/* executes once */
    {
	Points.alloc_points = Points.n_points = 0;
	first_time = -1;
    }

/* Get-line loop */
    for(l=0; l<Area->n_lines; l++)
    {
    /* Read in line coordinates */
	at_line = abs(Area->lines[l]);

	Line = &(map->Line[at_line]);

	/* dont check lines that obviously do not 
	** intersect with test ray    -dpg 3.1
	*/
	if ((Line->N < Y) || (Line->S > Y) || (Line->E < X))
	    continue;

	if (0 > V2_read_line (map, &Points, at_line))
	    return (-1.0);
    
    /* adjust yarray coordinates */
	y = Points.y;
	for(n=0; n<Points.n_points; n++)
	{
/*  THIS WONT WORK IF Y == 0.0   12/89 */
/* THIS is changing USERS data!! */
	    if (*y == Y)
		*y = Y * 1.000000001;
		/* *y = Y + .001;  changed to depend more on scale -dpg 6/89 */
	    y++;
	}

    /* Point loop */
	x = Points.x;
	y = Points.y;
	cur_x = *x ; x++;
	cur_y = *y ; y++;
	n_segs = Points.n_points - 1;
    
	for(n=0; n<n_segs; n++)
	{
	    if((cur_y < Y && *y < Y)
	    || (cur_y > Y && *y > Y)
	    || (cur_x < X && *x < X))
	    {
		cur_x = *x ; x++;
		cur_y = *y ; y++;
		continue;
	    }

	    x_inter = dig_x_intersect (cur_x, *x, cur_y, *y, Y);
	    if (x_inter > X)
	    {
		n_intersects++;

		x_dist = fabs(x_inter - X);
		if (x_dist < cur_min)
		    cur_min = x_dist;
	    }

	    cur_x = *x ; x++;
	    cur_y = *y ; y++;
	}
    }
#ifdef DEBUG
debugf ("PNT_IN_AREA:  intersects = %d\n", n_intersects);
#endif
    if (n_intersects % 2)
	return(cur_min);
    else
	return(0.0);
}

double dig_x_intersect (
    double beg_x,
    double end_x,
    double beg_y,
    double end_y,
    double Y)
{
    double b, a;

    b = (end_x - beg_x) / (end_y - beg_y);
    a = beg_x - b * beg_y;
    return(a + b * Y);
}

int dig_in_area_bbox (
    P_AREA *Area,
    double x,double y)
{
#ifdef FOO
/*DEBUG*/ fprintf (stderr, "BBOX: (x,y) (%lf, %lf)\n", x, y);
/*DEBUG*/ fprintf (stderr, "NSEW:  %lf, %lf, %lf, %lf\n", Area->N, Area->S, Area->E, Area->W);
#endif
    if (x < Area->W)
	return(0);
    if (x > Area->E)
	return(0);
    if (y < Area->S)
	return(0);
    if (y > Area->N)
	return(0);
    
    return(1);
}

/*
** same as dig_point_in_area ()  above, execpt that it works for 
** a generic polygon, build with 'Points'
**   dpg 12/89  (for Vcontour or whatever it may be called by the time
**               this is read)
**
**   WARNING:  if poly is an area,  this will NOT tell you if it inside
**     an Island w/in the area.
*/
double
dig_point_in_poly (
    double X,double Y,
    struct line_pnts *Points)
{
    double *x, *y;
    double cur_min;
    double cur_x, cur_y;
    double dig_x_intersect();
    double x_inter;
    double x_dist;
    int n_intersects;
    int n_segs;
    int n;

    cur_min = HUGE_VAL;
    cur_x = 0.0;
    cur_y = 0.0;
    n_intersects = 0;
    
/* adjust yarray coordinates */
    y = Points->y;
    for(n=0; n<Points->n_points; n++)
    {
	if (*y == Y)
	    *y = Y * 1.000000001;		/* TODO actually changing data*/
	y++;
    }

/* Point loop */
    x = Points->x;
    y = Points->y;
    cur_x = *x ; x++;
    cur_y = *y ; y++;
    n_segs = Points->n_points - 1;

    for(n=0; n<n_segs; n++)
    {
	if((cur_y < Y && *y < Y)
	|| (cur_y > Y && *y > Y)
	|| (cur_x < X && *x < X))
	{
	    cur_x = *x ; x++;
	    cur_y = *y ; y++;
	    continue;
	}

	x_inter = dig_x_intersect (cur_x, *x, cur_y, *y, Y);
	if (x_inter > X)
	{
	    n_intersects++;

	    x_dist = fabs(x_inter - X);
	    if (x_dist < cur_min)
		cur_min = x_dist;
	}

	cur_x = *x ; x++;
	cur_y = *y ; y++;
    }

    if (n_intersects % 2)
	return(cur_min);
    else
	return(0.0);
}
