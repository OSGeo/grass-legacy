/*
**  Written by: Dave Gerdes 1/1991
**  US Army Construction Engineering Research Lab
*/

#include "gis.h"
#include "V_.h"



/*
** reads any specified line   This is NOT affected by constraints
*/
V2_read_line (Map, line_p, line)
    struct Map_info *Map; 
    struct line_pnts *line_p;
    int line;
{
    if (line < 1 || line > Map->n_lines)		/* ALL DONE */
	return -2;

    return Vect__Read_line (Map, line_p, Map->Line[line].offset);
}

/* reads next unread line each time called.  use Vect_rewind to reset */
/*  returns -2 on end of lines */

V2_read_next_line (Map, line_p)
    struct Map_info *Map; 
    struct line_pnts *line_p;
{
    register int line;
    register P_LINE *Line;

    while (1)
    {
	line = Map->next_line;

	if (line > Map->n_lines)
	    return (-2);

	Line = &(Map->Line[line]);

	if ((Map->Constraint_type_flag && !(Line->type & Map->Constraint_type)))
	{
	    Map->next_line++;
	    continue;
	}

	if (Map->Constraint_region_flag)
	    if (! V__map_overlap (Map, Line->N, Line->S, Line->E, Line->W))
	    {
		Map->next_line++;
		continue;
	    }

	return V2_read_line (Map, line_p, Map->next_line++);
    }
    /* NOTREACHED */
}

V__map_overlap (Map, n, s, e, w)
    struct Map_info *Map;
    double n, s;
    double e, w;
{
    struct Cell_head W;

    /* updated for Lat lon support 21 Jun 91 */
    W.north = Map->Constraint_N;
    W.south = Map->Constraint_S;
    W.east  = Map->Constraint_E;
    W.west  = Map->Constraint_W;
    W.proj  = Map->proj;

    return G_window_overlap (&W, n, s, e, w);
}
