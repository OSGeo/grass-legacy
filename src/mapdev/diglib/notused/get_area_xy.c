/*
**  Written by:  Mike Higgins 5 1988
** 		 Dave Gerdes
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"

/* returns 0  or -1 on error */
dig_P_get_area_xy (Map, area, n_points, x, y)
	struct Map_info *Map;
	int area;
	int *n_points;
	double **x;
	double **y;
{
	struct line_pnts *Points ;

	Points = dig__P_get_area_xy (Map, area) ;

	if (Points == NULL)
		return (-1) ;

	*x = Points->x ;
	*y = Points->y ;
	*n_points = Points->n_points ;

	return (0) ;
}

struct line_pnts *
dig__P_get_area_xy (Map, area)
    struct Map_info *Map;
    int area;
{
	register int i, line;
	int start, end, to, from, inc;
	P_AREA *Area;
	int done_yet;

	static int init_points;	/* zero at startup */
	static struct line_pnts Points;
	static struct line_pnts BPoints;


	BPoints.n_points = 0;
	Area =  &(Map->Area[area]) ;

	if (init_points == 0)
	{
		Points.alloc_points = 0;	/* executed only once */
		init_points = -1; 
    		BPoints.alloc_points = BPoints.n_points = 0;
    		if (0 > dig_alloc_points (&BPoints, 500))
			return (NULL);
	}


	for (i = 0 ; i < Area->n_lines ; i++)
	{
		line = abs(Area->lines[i]);

		if (0 > dig__Read_line (&Points, Map->dig_fp, Map->Line[line].offset))
			return (NULL);

		if (0 > dig_alloc_points (&BPoints, Points.n_points + BPoints.n_points + 1))
			return(NULL) ;

		if (Area->lines[i] < 0)
		{
			start = Points.n_points - 1;
			inc = -1 ;
			end = 0;
		}
		else
		{
			end = Points.n_points - 1;
			inc = 1 ;
			start = 0;
		}

		done_yet = 0;
		for(from = start, to = BPoints.n_points ; !done_yet ; from+=inc, to++)
		{
			if (from == end)
				done_yet = 1;
			BPoints.x[to] = Points.x[from];
			BPoints.y[to] = Points.y[from];
		}
		BPoints.n_points = Points.n_points + BPoints.n_points ;

	}

	return (&BPoints);
}

