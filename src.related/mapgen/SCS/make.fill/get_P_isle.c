
/* %W% %G% */
/*
**  The following are functions developed by modifying
**  the dig_P_get_area functions written by
** 		 Dave Gerdes
**  US Army Construction Engineering Research Lab
**   MLH SCS
*/

#include "digit.h"


struct line_pnts *
dig__P_get_isle (Map, isle, n_isle, n_points)
    struct Map_info *Map;
    int isle[], n_isle, n_points[];
{
	register int i, line, n;
	int start, end, to, from, inc;
	P_ISLE *Area;
	int done_yet,islepts;

	static int init_points;	/* zero at startup */
	static struct line_pnts Points;
	static struct line_pnts BPoints;

	BPoints.n_points = islepts = 0;

	if (init_points == 0)
	{
		Points.alloc_points = 0;	/* executed only once */
		init_points = -1; 
    		BPoints.alloc_points = BPoints.n_points = 0;
    		if (0 > dig_alloc_points (&BPoints, 500))
			return (NULL);
	}


for (n=0;n<n_isle;n++) {
	islepts = 0;
	Area = &(Map->Isle[isle[n]]);
	for (i = 0 ; i < Area->n_lines ; i++)
	{
		line = ABS(Area->lines[i]);
/* DEBUG  fprintf(stderr,"l_%d o_%d",line,Map->Line[line].offset);*/

		if (0 > V2_read_line (Map, &Points, line))
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
/* DEBUG  fprintf(stderr,"x_%lf y_%lf ",Points.x[from],Points.y[from]);*/
			BPoints.x[to] = Points.x[from];
			BPoints.y[to] = Points.y[from];
		}
		BPoints.n_points = Points.n_points + BPoints.n_points ;
		islepts = Points.n_points + islepts ;

/* DEBUG  fprintf(stderr,"n=%d pts=%d\n",n,BPoints.n_points);*/
	}
	n_points[n] = islepts;
}

	return (&BPoints);
}



/* returns 0  or -1 on error */
dig_P_get_isle (Map, isle, n_isle, n_points, x, y)
	struct Map_info *Map;
	int isle[], n_isle;
	int n_points[];
	double **x;
	double **y;
	
{

	struct line_pnts *Points ;

	Points = dig__P_get_isle (Map, isle, n_isle, n_points) ;

	if (Points == NULL)
		return (-1) ;

	*x = Points->x ;
	*y = Points->y ;

	return (0) ;
}
