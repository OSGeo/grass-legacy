
/*  notice that all LINES, NODES, and AREA   indexes start at 1  */

#include "digit.h"
#include "head.h"

#define    ALLOC_AMT    512

struct line_pnts Gpoints;  /* make this available to makup_isles *//*ISLE*/

read_digit(Map, Plus)
    struct  Map_info *Map;
    struct  Plus_head *Plus;
{
    char chartype;
    int type;
    long    offset, offset2;
    long ftell();
    struct new_node node;
    int snapped;
    int new_line;
    double x1, y1, x2, y2;
    /*DEBUG*/ long filsiz;

    /*DEBUG*/ 
    {
	long tmp;

	tmp = ftell (Map->digit);
	fseek (Map->digit, 0l, 2);
	filsiz = ftell (Map->digit);
	fseek (Map->digit, tmp, 0);
    }

    Gpoints.alloc_points = Gpoints.n_points = 0;
    if (0 > dig_alloc_points (&Gpoints, 1000))
    {
	dig_out_of_memory ();
	return (-1);
    }
    snapped = 0;

    /* this is the guts of the work */
    /*  reading lines from digit file and building info for plus file  */
    while(1)
    {
	offset = ftell (Map->digit);
	G_percent (offset, filsiz, 5);
	type =   dig__Read_line (&Gpoints, Map->digit, offset);
	chartype = (char) type;
	if (type == -2)	/* EOF */
	    return (snapped);
	if (type < 0)
	{
	    if (type == -1)
		fprintf (stderr, "Read_line: Out of Memory\n");
	    else
		fprintf (stderr, "Unknown error\n");
	    return (-1);
	}
	/* This is right, but not enough
	   also need to change the one in lib
	if (-9.0 == dig_calc_begin_angle (&Gpoints, Map->snap_thresh))
	*/
	if (-9.0 == dig_calc_begin_angle (&Gpoints, head.map_thresh))
	{
/*DEBUG*/ fprintf (stderr, "Degenerate line (%d), skipping.  offset %ld (%lf,%lf) (%lf,%lf)\n", Map->n_lines+1, offset, Gpoints.x[0], Gpoints.y[0], Gpoints.x[Gpoints.n_points-1], Gpoints.y[Gpoints.n_points-1]);
	    continue;
	}

	x1 = Gpoints.x[0];
	y1 = Gpoints.y[0];
	x2 = Gpoints.x[Gpoints.n_points -1];
	y2 = Gpoints.y[Gpoints.n_points -1];

	switch (chartype) {
	    case LINE:
	    case AREA:
	    case DOT:
		break;
	    case DEAD_LINE:	/* we want to skip DEAD lines */
	    case DEAD_AREA:
	    case DEAD_DOT:
	    default:
		continue;
		break;
	}

/*
** check_nodes finds other nodes w/in snapping distance
** import_line creates all internal references to new line and snaps
**    nodes if needed
*/
	dig_check_nodes (Map, &node, &Gpoints);
	new_line = import_line (Map, chartype, &node, &Gpoints, offset);
	if (new_line < 0)
	{
	    return (-1);
	}
	if (x1 != Gpoints.x[0] || y1 != Gpoints.y[0] ||
	    x2!=Gpoints.x[Gpoints.n_points-1] || y2!=Gpoints.y[Gpoints.n_points-1])
	{
	    snapped ++;
	    offset2 = ftell (Map->digit);
	    dig__Rewrite_line (Map->digit, offset, chartype, &Gpoints);
	    /* dont remove this!  it is needed */
	    fseek (Map->digit, offset2, 0);	/* set up for next read */
	}

    }    /*  while (1)  */
    /* return is at EOF */
}
