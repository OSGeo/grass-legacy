/*
*    These functions are almost identical to the level two access functions
*    contained in $GIS/src/lib/get_area_xy.c, but they return the coordinates
*    of an island rather than an area.
*
*/
#include "Vect.h"

#define ABS(x) ((x) < 0 ? -(x) : (x))


struct line_pnts *
get_isle_xy (Map, isle)
    struct Map_info *Map;
    int isle;
{
	register int i, line;
	int start, end, to, from, inc;
	P_ISLE *Isle;
	int done_yet;
        char ans[4];
	static int init_points;	/* zero at startup */
	static struct line_pnts Points;
	static struct line_pnts BPoints;

	BPoints.n_points = 0;
	Isle =  &(Map->Isle[isle]) ;

	if (init_points == 0)
	{
		Points.alloc_points = 0;	/* executed only once */
		init_points = -1; 
    		BPoints.alloc_points = BPoints.n_points = 0;
    		if (0 > dig_alloc_points (&BPoints, 500))
			return (NULL);
	}

	for (i = 0 ; i < Isle->n_lines ; i++)
	{
		line = ABS(Isle->lines[i]);

		if (0 > V1_read_line (Map, &Points, Map->Line[line].offset))
			return (NULL);

		if (0 > dig_alloc_points (&BPoints, Points.n_points + BPoints.n_points + 1))
			return(NULL) ;

		if (Isle->lines[i] < 0)
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
/*
for (i = 0; i<BPoints.n_points; i++) 
fprintf(stderr, "%d  x: %10.5f  y: %10.5f\n",i,BPoints.x[i], BPoints.y[i]);
fprintf(stderr, "\ncontinue?"); gets(ans);
*/
  	return (&BPoints);
}

