#include "digit.h"


/*  Looks for all duplicate lines.
**  Lines must be IDENTICAL 
*/
display_duplicate_lines (Map)
    struct Map_info *Map;
{
    P_LINE *Line1, *Line2;
    int line1, line2;
    static struct line_pnts Points1, Points2;
    static int first = 1;
    int start, dir;
    char buf[20];
    int ret;
    

    ret = 0;
    if (first)
    {
	Points1.alloc_points = 0;
	Points2.alloc_points = 0;
	first = 0;
    }
    TimeOutCursor (1);
    for (line1 = 1 ; line1 <= Map->n_lines ; line1++)
    {
	Line1 = &(Map->Line[line1]);
        if (!line_in_window (Line1) || LINE_ALIVE(Line1))
	    continue;

	for (line2 = line1+1 ; line2 <= Map->n_lines ; line2++)
	{
	         if (Check_for_interrupt())
		 {
		     ret = -1;
		     goto leave;
		 }


	    Line2 = &(Map->Line[line2]);
	    if (!line_in_window (Line2) || LINE_ALIVE(Line2))
		continue;


	    /* check for same nodes */
	    if (Line1->N1 == Line2->N1 && Line1->N2 == Line2->N2  ||
	        Line1->N1 == Line2->N2 && Line1->N2 == Line2->N1)
	    {
		int i, j;

		if (0 > V1_read_line (Map, &Points1, Line1->offset))
		{
		    BEEP;
		    write_info (1, "Error reading line.");
		    sleep (3);
		    ret = -1;
		    goto leave;
		}
		if (0 > V1_read_line (Map, &Points2, Line2->offset))
		{
		    BEEP;
		    write_info (1, "Error reading line.");
		    sleep (3);
		    ret = -1;
		    goto leave;
		}

		if (Points1.n_points != Points2.n_points)
		{
		    continue;
		}
/*DEBUG*/ debugf ("Lines %d and %d match n_points.\n", line1, line2);

		if (Line1->N1 != Line2->N1)
		{
		    dir = -1;
		    start = Points2.n_points-1;
		}
		else
		{
		    dir = 1;
		    start = 0;
		}
		for (i = 0, j = start ; i < Points1.n_points ; i++, j+= dir)
		{
		    if (Points1.x[i] != Points2.x[j] || 
			Points1.y[i] != Points2.y[j])
		    {
/*DEBUG*/ debugf ("Points %d(%d) do not match (%lf,%lf) (%lf,%lf)\n",i,j,
/*DEBUG*/ Points1.x[i], Points1.y[i], Points2.x[j], Points2.y[j]);
			goto bottom;
		    }
		}

		/* Identical.  Highlight one of them */
		_highlight_line(Line1->type, &Points1, line1, Map);
/*DEBUG*/ fprintf (stderr, "duplicate lines %d & %d\n", line1, line2);
		ret++;
	    }
bottom:
	    continue;
	}
    }

leave:
    TimeOutCursor (0);

    return ret;
}
