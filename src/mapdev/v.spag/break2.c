#include <unistd.h>
#include "Vect.h"
#include "local_proto.h"

static int _do_break_line(struct Map_info *,struct line_pnts *,
                          int,char,struct new_node *);

    /*
    static struct line_pnts Points;
    */
    static struct line_pnts NPoints;
    static int first_time = 1;

int break_lines_at_segs_w_point (
    struct Map_info *map,
    double ux,double uy,
    int  A,		/* line indexes */
    int Aseg,	/* segment to break line at */
    struct line_pnts *APoints,
    int  B,		/* line indexes */
    int Bseg,	/* segment to break line at */
    struct line_pnts *BPoints,
    int node)
{
    {
	if (A && A == B)
	{
	    break_line_at_segs_w_point (map, ux,uy, A, Aseg,Bseg,APoints,node);
	    return (0);
	}

	if (A)
	    break_line_at_seg_w_point (map, ux, uy, A, Aseg, APoints, node);
	if (B && A != B)
	    break_line_at_seg_w_point (map, ux, uy, B, Bseg, BPoints, node);

	return (0);
    }
}

int break_line_at_seg_w_point (
    struct Map_info *map,
    double ux,double uy,
    int  line,		/* line indexe */
    int segment,	/* segment to break line at */
    struct line_pnts *Points,
    int on_node)
{
    struct new_node node;
    char type;
    register int i;
    P_LINE *Line;
    int n_points;
    int label;
    int N1, N2;
    int line1, line2;

    /* segment  ranges from 1 -> */
    

    if (first_time)
    {
	first_time = 0;
	NPoints.alloc_points = 0;
    }

    Line = &(map->Line[line]);

    /* hold on to node numbers for new lines */
    N1 = Line->N1;
    N2 = Line->N2;

    /* save attribute info */
    /* NOTE for now, I don't bother trying to restore area atts */
    if (Line->att)
    {
	label = map->Att[Line->att].cat;
    }
    else
	label = 0;

    type = Line->type;



#ifdef DEBUG2
  debugf ("Breaking on segment %d\n", segment);
#endif


    /* TODO??, Check past end of segment?? */

/*-------------------------------------------------------------*/
    /* DO FIRST LINE */
    n_points = segment + 1;
    if (0 > dig_alloc_points (&NPoints, n_points))
    {
	fprintf (stderr, "Out of memory.  Failed");
	return (-1);
    }
    for (i = 0 ; i < n_points-1 ; i++)
    {
	NPoints.x[i] = Points->x[i];
	NPoints.y[i] = Points->y[i];
    }
    NPoints.x[n_points-1] = ux ; 
    NPoints.y[n_points-1] = uy ; 
    NPoints.n_points = n_points;
#ifdef DEBUG
 debugf ("calling 1st _do_break\n");
#endif

    /* TEST */
    if (on_node) /*  cause nodes to snap to existing nodes change below also */
	dig_check_nodes (map, &node, &NPoints);
    else
    {
	/* cause new nodes to be created.  This is better for
	** hanging nodes, where existing hanging node would cause the
	**  intersecting line to bend away from more true course
	*/
	node.N1 = N1;
	node.N2 = map->n_nodes + 1;
    }

    line1 = _do_break_line (map, &NPoints, label, type, &node);
#ifdef DEBUG
 debugf ("returned ok from 1st _do_break\n");
#endif



/*-------------------------------------------------------------*/
    /* DO SECOND LINE */
    n_points = Points->n_points - segment + 1;
#ifdef DEBUG
 debugf ("BREAK: n_points 2 = %d\n", n_points);
#endif
    if (0 > dig_alloc_points (&NPoints, n_points))
    {
	fprintf (stderr, "Out of memory.  Failed");
	sleep (1);
	return (-1);
    }
    NPoints.x[0] = ux ; 
    NPoints.y[0] = uy ; 
    NPoints.n_points = n_points;
    for (i = 1 ; i < n_points ; i++)
    {
	NPoints.x[i] = Points->x[segment-1+i];
	NPoints.y[i] = Points->y[segment-1+i];
    }
#ifdef DEBUG
 debugf ("calling 2nd _do_break\n");
#endif
    node.N1 = node.N2;
    node.N2 = N2;
    line2 = _do_break_line (map, &NPoints, label, type, &node);
#ifdef DEBUG
 debugf ("returned ok from 2nd _do_break\n");
#endif

    /* get rid of old line */
    /* remove old line AFTER adding new lines, to hold on to nodes that
    ** would have been deleted otherwise
    */
    _remove_line (map, line);

    return (0);
}

/*
**  This routine is for breaking a line with itself
**  
**  If one line intersects itself, then
**  Three lines will form:


       ---------------
       \      3      /
        \           /
         \         /
          \       /
           \     /
            \   /
             \ /
	      X
             / \
          1 /   \  2
           /     \
**
*/

int break_line_at_segs_w_point (
    struct Map_info *map,
    double ux,double uy,
    int  line,		/* line indexe */
    int Aseg,int Bseg,	/* segment to break line at */
    struct line_pnts *Points,
    int on_node)
{
    struct new_node node;
    char type;
    register int i;
    P_LINE *Line;
    int n_points;
    int label;
    int N1, N2;
    int line1, line2;

    /* segment  ranges from 1 -> */
    

    if (first_time)
    {
	first_time = 0;
	NPoints.alloc_points = 0;
    }

    Line = &(map->Line[line]);

    /* hold on to node numbers for new lines */
    N1 = Line->N1;
    N2 = Line->N2;

    /* save attribute info */
    /* NOTE for now, I don't bother trying to restore area atts */
    if (Line->att)
    {
	label = map->Att[Line->att].cat;
    }
    else
	label = 0;

    type = Line->type;



#ifdef DEBUG2
  debugf ("Breaking on segment %d\n", Aseg);
#endif


    /* TODO??, Check past end of segment?? */

/*-------------------------------------------------------------*/
    /* DO FIRST LINE */
    n_points = Aseg + 1;
    if (0 > dig_alloc_points (&NPoints, n_points))
    {
	fprintf (stderr, "Out of memory.  Failed");
	return (-1);
    }
    for (i = 0 ; i < n_points-1 ; i++)
    {
	NPoints.x[i] = Points->x[i];
	NPoints.y[i] = Points->y[i];
    }
    NPoints.x[n_points-1] = ux ; 
    NPoints.y[n_points-1] = uy ; 
    NPoints.n_points = n_points;
#ifdef DEBUG
 debugf ("calling 1st _do_break\n");
#endif

    /* TEST */
    if (on_node) /*  cause nodes to snap to existing nodes change below also */
	dig_check_nodes (map, &node, &NPoints);
    else
    {
	/* cause new nodes to be created.  This is better for
	** overshoots, where existing node would cause the
	**  intersecting line to bend away from more true course
	*/
	node.N1 = N1;
	node.N2 = map->n_nodes + 1;
    }

    line1 = _do_break_line (map, &NPoints, label, type, &node);
#ifdef DEBUG
 debugf ("returned ok from 1st _do_break\n");
#endif



/*-------------------------------------------------------------*/
    /* DO SECOND LINE */
    /*NOTE, this can lead to some 0 length segments IFF
    **  intersection is right on point of line
    */
    n_points = Bseg - Aseg + 2;
#ifdef DEBUG
 debugf ("BREAK: n_points 2 = %d\n", n_points);
#endif
    if (0 > dig_alloc_points (&NPoints, n_points))
    {
	fprintf (stderr, "Out of memory.  Failed");
	sleep (1);
	return (-1);
    }
    NPoints.x[0] = ux ; 
    NPoints.y[0] = uy ; 
    NPoints.x[n_points-1] = ux ; 
    NPoints.y[n_points-1] = uy ; 
    NPoints.n_points = n_points;
    for (i = 1 ; i < n_points-1 ; i++)
    {
	NPoints.x[i] = Points->x[Aseg-1+i];
	NPoints.y[i] = Points->y[Aseg-1+i];
    }
#ifdef DEBUG
 debugf ("calling 2nd _do_break\n");
#endif
    node.N1 = node.N2;
    node.N2 = node.N2;
    line2 = _do_break_line (map, &NPoints, label, type, &node);
#ifdef DEBUG
 debugf ("returned ok from 2nd _do_break\n");
#endif


/*-------------------------------------------------------------*/
    /* DO THIRD LINE */
    n_points = Points->n_points - Bseg + 1;
#ifdef DEBUG
 debugf ("BREAK: n_points 2 = %d\n", n_points);
#endif
    if (0 > dig_alloc_points (&NPoints, n_points))
    {
	fprintf (stderr, "Out of memory.  Failed");
	sleep (1);
	return (-1);
    }
    NPoints.x[0] = ux ; 
    NPoints.y[0] = uy ; 
    NPoints.n_points = n_points;
    for (i = 1 ; i < n_points ; i++)
    {
	NPoints.x[i] = Points->x[Bseg-1+i];
	NPoints.y[i] = Points->y[Bseg-1+i];
    }
#ifdef DEBUG
 debugf ("calling 2nd _do_break\n");
#endif
    node.N1 = node.N2;
    node.N2 = N2;
    line2 = _do_break_line (map, &NPoints, label, type, &node);
#ifdef DEBUG
 debugf ("returned ok from 2nd _do_break\n");
#endif

    /* get rid of old line */
    /* remove old line AFTER adding new lines, to hold on to nodes that
    ** would have been deleted otherwise
    */
    _remove_line (map, line);

    return (0);
}


/* returns new line number */
static int _do_break_line (
    struct Map_info *map,
    struct line_pnts *Points,
    int label,
    char type,
    struct new_node *node)
{
    int nline;

#ifdef DEBUG
 debugf ("calling new_line\n");
#endif
    nline = new_line (map, type, node, Points);
    if (nline < 0)
    {
	BEEP;
	fprintf (stderr, "Error creating new line.");
	return (-1);
    }
    /* put the label back on */
#ifdef LABELS_TOO
    if (label)
    {
	get_line_center (&x, &y, Points);
	att = dig_new_att (map, x, y, type, nline, label);
	if (att < 0)
	    return (-1);
	map->Line[nline].att = att;
    }
#endif
    return (nline);
}

/*
** This is copied from  dig__check_dist in file mapdev/lib/point_t_line.c
**   because I needed those segments where the point of interest is NOT
**    a node on the line.
*/
int local_check_dist (
    struct Map_info *map,
    struct line_pnts *points,
    double ux,
    double uy,
    double *dist)
{
    register int i;
    register double distance;
    register double new_dist;
    register int n_points;
    int segment;


    /*  dpg  2 aug 1989
       corrected this code to work with 1 point lines,  for DOT
    if (points->n_points < 2)
    {
        *dist = 99999999.;
        return (-1);
    }
    */
    n_points = points->n_points;
    segment = 1;

    /* initialize for segment not at end of line */
    i = GREATER (n_points-2, 1);
    distance = dig_distance2_point_to_line(ux, uy, points->x[i-1], points->y[i-1],
            points->x[i], points->y[i]);
    segment = i;
    for (/* i = i */ ; i < n_points-1 ; i++)
    {
        new_dist= dig_distance2_point_to_line(ux, uy, points->x[i],points->y[i],
                points->x[i+1], points->y[i+1]);
	if (! ((ux == points->x[0] && uy == points->y[0]) ||
		     (points->x[points->n_points-1] && uy == 
		     points->y[points->n_points-1])))
	    if (new_dist < distance)
	    {
		distance = new_dist;
		segment = i+1;
	    }
    }
    *dist = distance;
    return (segment);
}
