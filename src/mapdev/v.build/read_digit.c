
/*  notice that all LINES, NODES, and AREA   indexes start at 1  */

#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "vbuildlib.h"

#define    ALLOC_AMT    512

static double  W_N;
static double  W_S;
static double  W_E;
static double  W_W;

struct line_pnts Gpoints;  /* make this available to makup_isles *//*ISLE*/

int 
read_digit (struct Map_info *Map, struct Plus_head *Plus)
{
    int chartype;
    int type;
    long    offset, offset2;
    struct new_node node;
    int snapped;
    int new_line;
    double x1, y1, x2, y2;
    int zero_lines = 1;		/* set FALSE after reading 1 line */
    /*DEBUG*/ long filsiz;

    /*DEBUG*/ 
    {
	long tmp;

	tmp = ftell (Map->dig_fp);
	fseek (Map->dig_fp, 0l, 2);
	filsiz = ftell (Map->dig_fp);
	fseek (Map->dig_fp, tmp, 0);
    }

    Gpoints.alloc_points = Gpoints.n_points = 0;
    if (0 > dig_alloc_points (&Gpoints, 1000))
    {
	dig_out_of_memory ();
	return (-1);
    }
    snapped = 0;

    /****************************************************************/
    /* PRE-alloc arrays for lines, nodes, areas, based on line info */
    /* NEW 4.0 */
    {
	struct dig_head dhead;	/* junk */
	int num_lines = 0;
	int num_areas = 0;

	/*
	fprintf (stderr, "Pre-allocating memory ...\n");
	*/
	while(1)
	{
	    offset = ftell (Map->dig_fp);
	    /*type = dig__Read_line (&Gpoints, Map->dig_fp, offset);*/
	    type = V1_read_line (Map, &Gpoints, offset);
	    if (type < 0)	/* EOF (or error...)*/
		break;

	    /*if (LINE_ALIVE (type))*/
	    if (type < 16)
	    {
		switch (type) {
		    case DOT:	/*FALLTHROUGH*/
		    case LINE:  num_lines++; break;
		    case AREA:  num_areas++; break;
		}
	    }
	}
	dig_alloc_line (Map, num_lines+num_areas);
	dig_alloc_node (Map, (int) ((num_lines+num_areas) * 1.3)); /* TWEEK */
	dig_alloc_area (Map, (int) (num_areas/3));

	/* get back to beginning of data  again, not kosher for 4.0 */
	Vect__read_head_binary (Map, &dhead);
    }

    /****************************************************************/
    /* this is the guts of the work */
    /*  reading lines from digit file and building info for plus file  */
    while(1)
    {
	offset = ftell (Map->dig_fp);
	G_percent (offset, filsiz, 5);
	/*type =   dig__Read_line (&Gpoints, Map->dig_fp, offset);*/
	type = V1_read_line (Map, &Gpoints, offset);
	chartype = (char) type;
	if (type == -2)	/* EOF */
	{
	    if (zero_lines)
		init_extents_from_head (&(Map->head));
	    return (snapped);
	}
	zero_lines = 0;
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
	/* if (-9.0 == dig_calc_begin_angle (&Gpoints, head.map_thresh)) 4.0 */
	if (-9.0 == dig_calc_begin_angle (&Gpoints, Map->head.map_thresh))
	{
/*DEBUG*/ fprintf (stderr, "Degenerate line (%d), skipping.  offset %ld (%f,%f) (%f,%f)\n", Map->n_lines+1, offset, Gpoints.x[0], Gpoints.y[0], Gpoints.x[Gpoints.n_points-1], Gpoints.y[Gpoints.n_points-1]);
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
	    offset2 = ftell (Map->dig_fp);
	    Vect__Rewrite_line (Map, offset, chartype, &Gpoints);
	    /* dont remove this!  it is needed */
	    fseek (Map->dig_fp, offset2, 0);	/* set up for next read */
	}

	/* 4.0   Check window extents against data */
	/*  This is done, cuz g.region now looks at header for window */
	/*  and we want to make sure it is accurate */
	{
	    int i;

	    for (i = 0 ; i < Gpoints.n_points ; i++)
	    {
		 if (Gpoints.x[i] < W_W)  W_W = Gpoints.x[i];
	    else if (Gpoints.x[i] > W_E)  W_E = Gpoints.x[i];
		 if (Gpoints.y[i] < W_S)  W_S = Gpoints.y[i];
	    else if (Gpoints.y[i] > W_N)  W_N = Gpoints.y[i];
	    }
	}

    }    /*  while (1)  */
    /* return is at EOF */
    /*NOTREACHED*/
}


#ifndef HUGE_VAL
#define HUGE_VAL 99999999999.9
#endif
int init_extents (void)
{
    W_N = -HUGE_VAL;
    W_S = HUGE_VAL;
    W_E = -HUGE_VAL;
    W_W = HUGE_VAL;

    return 0;
}

int init_extents_from_head (struct dig_head *head)
{
    W_N = head->N;
    W_S = head->S;
    W_E = head->E;
    W_W = head->W;

    return 0;
}

int update_head_from_ext (struct dig_head *head)
{
    double NS, EW;

    NS = (W_N - W_S) * .05;
    EW = (W_E - W_W) * .05;

    head->N = W_N + NS;
    head->S = W_S - NS;
    head->E = W_E + EW;
    head->W = W_W - EW;

    return 0;
}
