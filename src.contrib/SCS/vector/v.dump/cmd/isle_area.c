/*  @(#)isle_area.c     1.0  04/02/91   */
/*  created by:         R.L.Glenn, SCS */

#include "Vect.h"

isle_area (map, isle, totalarea)
    struct Map_info *map;
    int  isle;
    double *totalarea;
{
    int cur_line;
    int ab_line;
    int i;
    double *x, *y;
    static struct line_pnts points;
    static int first_time;	/* 0 on startup */
    double tot_area, sum_area;


    *totalarea = 0.;

    if (! map->Isle[isle].n_lines)
	return(-1);

    tot_area = 0.0;

    if (first_time == 0)
    {
	points.alloc_points = 0;
	first_time = -1;
    }

    tot_area = 0.0;
    for (cur_line = 0; cur_line < map->Isle[isle].n_lines ; cur_line++)
    {
	ab_line = ABS(map->Isle[isle].lines[cur_line]);
        V2_read_line (map, &points, ab_line);
	if (points.n_points < 2)
	    continue;
	
	x= points.x;
	y= points.y;

	sum_area = 0.0;
	for (i=1; i < points.n_points; i++) 
	    sum_area += (x[i]-x[i-1]) * (y[i] + y[i-1]);
	if (map->Isle[isle].lines[cur_line] > 0)
	    tot_area += sum_area;
	else
	    tot_area -= sum_area;
    }
    *totalarea =  0.5 * tot_area;
    
    return(0);
}
