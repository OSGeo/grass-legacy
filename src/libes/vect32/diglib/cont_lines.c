#include <math.h>
#include <stdlib.h>
#include "Vect.h"
/*
**  Written by Dave Gerdes  12/1989
**  US Army Construction Engineering Research Lab
*/

#define PI 3.14159265

static double normalize_angle(double);

#define LINES_INCR 10

/* returns pointer to two pointers to arrays of plus_t
**    return[0]  is array of the lines behind and including 'line'
**	   the order starts with 'line' and goes away from it
**    return[1]  is array of the lines after (not including) 'line'
**	   the order is away from 'line'
**    These arrays get over-written with each call
**
**   Each array will be terminated with a 0 value
**
**   returns NULL on error
*/
plus_t **dig_get_cont_lines (
    struct Map_info *map,
    plus_t line,
    double angle,
    int twoway)
{
    register int cnt;
    plus_t prev_line;
    int i;
    static int first_time = 1;
    static plus_t *lines[2] ;
    static int     lines_alloc[2] ;

    line = abs(line);

    if (first_time)
    {
	if (NULL==(lines[0] = (plus_t *) malloc (LINES_INCR * sizeof (plus_t))))
	    return ((dig_out_of_memory (), (plus_t **)NULL));
	if (NULL==(lines[1] = (plus_t *) malloc (LINES_INCR * sizeof (plus_t))))
	    return ((dig_out_of_memory (), (plus_t **)NULL));
	lines_alloc[0] = LINES_INCR;
	lines_alloc[1] = LINES_INCR;
	first_time = 0;
    }

    /* follow behind then ahead */
    for (i = 0 ; i < 2 ; i++)	 /* 0 behind,  1 ahead */
    {
	if (i == 0)
	{
	    lines[i][0] = line;
	    prev_line = -line;
	}
	else
	    prev_line = line;

	/* for 0 loop, start at lines[0][1] */
	for (cnt = 1-i ; ; cnt++)
	{
	    if (cnt >= lines_alloc[i])
	    {
		while ((lines_alloc[i] += LINES_INCR) <= cnt)
		    ;
		lines[i] = (plus_t *) realloc ((char *)lines[i], lines_alloc[i] * sizeof (plus_t));
		if (lines[i] == NULL)
		    return ((dig_out_of_memory (), (plus_t **)NULL));
	    }
	    lines[i][cnt] = dig_get_next_cont_line (map, -prev_line, angle, twoway);
	    if (!lines[i][cnt])	/* no more lines */
	    {
		break;
	    }
	    if (abs (lines[i][cnt]) == line)	/* ran into ourself */
	    {
#ifdef DEBUG
debugf ("RAN INTO MYSELF! line %d\n", line);
#endif
		lines[i][cnt] = 0;
		break;
	    }

	    prev_line = lines[i][cnt];
	}
    }
    return (lines);
}


/*
**  given previous line determine next node and
**  find and return (if it exists) the number of the line that 
**  is a continuation of the previous line through this node
**
**  Note that it must leave the node at the same angle as the previous line
**  (within a tolerance "angle").
**
**  if twoway is non-zero, then all two-way intersections will be
**   considered continuous, regardless of angle.
*/

plus_t dig_get_next_cont_line (
    struct Map_info *map,
    plus_t prev_line,
    double angle,
    int twoway)
{
    register int i;
    P_NODE *Node;
    P_LINE *Line;
    plus_t closest_line;
    double next_angle;
    double closest_angle, tmp;
    plus_t next_node;
    int found;

    closest_line = 0;

    if (prev_line < 0)
	next_node = map->Line[abs(prev_line)].N2;
    else
	next_node = map->Line[prev_line].N1;


    Node = &(map->Node[next_node]);


    if (Node->n_lines < 2)
	return 0;

    /* if twoway is set, then go ahead and check for 2 line intersection
    ** and if found, then just return the next line
    **   If twoway is not set, then the next line will have to pass
    **    the rest of the tests
    */
    if (twoway)
    {
	if (Node->n_lines == 2)
	{
	    if (Node->lines[0] == prev_line)
		return (Node->lines[1]);
	    else
		return (Node->lines[0]);
	}
    }

    /* find current line in node info and get angle */
    found = 0;
    for (i = 0 ; i < Node->n_lines ; i++)
    {
	if (Node->lines[i] == prev_line)
	{
	    double prev_angle;

	    prev_angle = Node->angles[i];
	    next_angle = normalize_angle (prev_angle + PI); 
#ifdef DEBUG
debugf ("PREV ANGLE %lf,  NEXT_ANGLE %lf\n", prev_angle, next_angle);
#endif
	    found = 1;
	    break;
	}
    }
    if (!found)	 /* ERROR: Line NOT attached to node! */
    {
	fprintf (stderr, "INTERNAL ERROR: Line(%d) NOT attached to node(%d)!\n", prev_line, next_node);
	return (0);
    }

    for (i = 0 ; i < Node->n_lines ; i++)
    {
	if (Node->lines[i] == prev_line)
	    continue;

	Line = &(map->Line[abs (Node->lines[i])]);

#ifdef DEFUNCT
	/* these are now up to the caller to check on return */

	/* if different line types, continue */
	if (type != Line->type) 
	    continue;
	
	/* if line is labelled a different value then continue */
	/* this needs to be cleaned up to allow RE-labelling of cont lines */
	if (cat)	/* else UNLABELLING */
	    if (LINE_LABELED (Line) && cat != map->Att[Line->att].cat)
		continue;
#endif /*DEFUNCT*/

	if (angle < (tmp = fabs (normalize_angle (Node->angles[i]-next_angle))))
	{
#ifdef DEBUG
debugf ("    NOT IN TOLERANCE (%lf)\n", tmp);
#endif
	    continue;
	}
#ifdef DEBUG
debugf ("    *** IN TOLERANCE (%lf)\n", tmp);
#endif

	if (!closest_line)
	{
	    closest_line = Node->lines[i];
	    closest_angle = tmp;
	}
	else
	{
	    if (tmp < closest_angle)
	    {
		closest_line = Node->lines[i];
		closest_angle = tmp;
	    }
	}
    }

#ifdef DEBUG
debugf ("Returning closest line: %d\n", closest_line);
#endif
    return (closest_line);
}

/* 
** take angle in radians and return a normalized value
**  between -PI and PI.
*/
static double normalize_angle(double angle)
{
    while (angle < -PI)
	angle += 2*PI;
    while (angle > PI)
	angle -= 2*PI;

    return angle;
}
