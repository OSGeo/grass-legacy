#include "digit.h"

/*
** same as dig_point_in_area ()  above, execpt that it works for 
** a generic polygon, build with 'Points'
**   dpg 12/89  (for Vcontour or whatever it may be called by the time
**               this is read)
*/
double
dig_point_in_poly (X, Y, Points)
    double X, Y;
    struct line_pnts *Points;
{
    double *x, *y;
    double cur_min;
    double cur_x, cur_y;
    double dig_x_intersect();
    double x_inter;
    double fabs();
    double x_dist;
    int at_line;
    int n_intersects;
    int n_segs;
    int l;
    int n;
    P_LINE *Line;

    cur_min = 999999999.;
    cur_x = 0.0;
    cur_y = 0.0;
    n_intersects = 0;
    
/* adjust yarray coordinates */
    y = Points->y;
    for(n=0; n<Points->n_points; n++)
    {
	if (*y == Y)
	    *y = Y * 1.000000001;
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
