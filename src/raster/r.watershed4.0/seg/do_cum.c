#include "Gwater.h"

do_cum ()
{
    SHORT	r, c, dr, dc;
    CELL	is_swale, value, valued;
    POINT	point;
    int		killer, threshold, count;

printf ("\nSECTION 3: Accumulating Surface Flow.       Percent complete: ");
count = 0;
if (bas_thres <= 0) threshold = 60;
else	threshold = bas_thres;
while (first_cum != -1) {
	G_percent (count++, do_points, 1);
	killer = first_cum;
	seg_get (&astar_pts, (char *) &point, 0, killer);
	first_cum = point.nxt;
        if ((dr = point.downr) > -1) { 
		r = point.r;
                c = point.c;
                dc = point.downc;
		cseg_get (&wat, &value, r, c);
		if (ABS(value) >= threshold)
			bseg_put (&swale, &one, r, c);
		cseg_get (&wat, &valued, dr, dc);
                if (value > 0) {
			if (valued > 0)
                    		valued += value;
			else
                    		valued -= value;
		} else {
			if (valued < 0)
                    		valued += value;
			else
                    		valued = value - valued;
                }
		cseg_put (&wat, &valued, dr, dc);
		bseg_get (&swale, &is_swale, r, c);
		if (is_swale || ABS(valued) >= threshold) {
			bseg_put (&swale, &one, dr, dc); 
		} else {
			if (er_flag && !is_swale)
				slope_length (r, c, dr, dc);
		}
        }
}
seg_close (&astar_pts);
printf ("\n");
}
