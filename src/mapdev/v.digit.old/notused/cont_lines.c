#include "digit.h"
/*
**  Written by Dave Gerdes  11/1989
**  US Army Construction Engineering Research Lab
*/

#define PI 3.14159265

double atof ();
double fabs ();
/*DEBUG*/ char *getenv ();
int tell_line_label();	/* function called by find_line_with_mouse() */
static double normalize_angle ();
static double ANGLE_VARIANCE = (PI/16.);

/*
**  after labelling chosen line, continue on labelling all other
** lines that appear to be continuations.  I.E. they are the same
**  type, they continue in the same direction, +/- a given range, and
**  they are not already labelled with a different value.
*
**  returns -1 on error or 0
*/
label_clines (map, cat)
    struct Map_info *map;
    int cat;
{
    plus_t line, prev_line, next_line;
    P_LINE *Line;
    int type, prev_node;
    plus_t **lines;
    int doit, pass, i, j;
/*DEBUG*/    char *p;

/*DEBUG*/    if (NULL != (p = getenv ("ANGLE")))
/*DEBUG*/	ANGLE_VARIANCE = PI / atof (p);

/*DEBUG*/ debugf ("Tolerance = %lf\n", ANGLE_VARIANCE); while (1)
    while (1)
    {
	Clear_info ();
	/* find_line_with_mouse  fills Gpoints */
	if (0 >= (line = find_line_with_mouse (LINE | AREA, "Choose line:", tell_line_label)))
	{
	    return (-1);
	}

/*
	if (cat)
	{
	    if (0 > label_line (map, line, cat, &Gpoints))
		return (-1);
	}
	else
	    unlabel_line (map, line, &Gpoints);
*/

	type = map->Line[line].type;


	lines = dig_get_cont_lines (map, line, ANGLE_VARIANCE, 0);

	/* go through it all twice,  first to highlight, then
	** to either act or unhighlight, based on user response
	*/
	for (pass = 1 ; pass <= 2 ; pass++)
	{
	    for (i = 0 ; i < 2 ; i++)	/* do both directions */
	    {
		for (j = 0 ; next_line = lines[i][j] ; j++)
		{
		    if (!next_line)	/* no more lines */
			break;

		    Line = &(map->Line[abs(next_line)]);

		    if (pass == 1)
		    {
		    /* throw away those lines we're not interested in */
			/* if different line types, continue */
			if (type != Line->type) 
			{
			    lines[i][j] = 0;
			    break;
			}
			
			/* if line is labelled a different value then continue */
			/* this needs to be cleaned up to allow RE-labelling of cont lines */
			if (cat)	/* else UNLABELLING */
			    if (LINE_LABELED (Line) && cat != map->Att[Line->att].cat)
			    {
				lines[i][j] = 0;
				break;
			    }
		    }


		    if(0 > V1_read_line(map, &Gpoints, Line->offset))
			return (-1);

		    if (pass == 1)
			_highlight_line(Line->type, &Gpoints, next_line, map);
		    else	/* pass 2 */
		    {
			if (!doit)
			{
			    _display_line(Line->type, &Gpoints, next_line, map);
			}
			else
			{
			    Changes_Made = 1;
			    if (cat)
			    {
				if (0 > label_line (map, abs(next_line), cat, &Gpoints))
				    return (-1);
			    }
			    else
				unlabel_line (map, abs(next_line), &Gpoints);
			}
		    }
		}
	    }
	    R_flush ();
	    if (pass == 1)
	    {
		char *str;
		if (cat)
		    str = "Label these lines?";
		else
		    str = "Un-label these lines?";
		doit = mouse_yes_no (str);
	    }
	}
    }
}

unlabel_line (map, line, Points)
    struct Map_info *map;
    plus_t line;
    struct line_pnts *Points;
{
    if (map->Line[line].att)
    {
	erase_line (map->Line[line].type, Points, line, map);
	dig_del_att (map, map->Line[line].att);
	map->Line[line].att = 0;
	display_line (map->Line[line].type, Points, line, map);
	return (1);
    }
    return (0);
}


#ifdef FOO
/*
**  given previous line, and all restrictive information, 
**  determine next node and
**  find and return (if it exists) the number of the line that 
**  is a continuation of the previous line through this node
**
**  Note that it must leave the node at the same angle as the previous line
**  (within a tolerance "ANGLE_VARIANCE"), must be of the same type (line, area)
**  and must be unlabelled or labelled with the same value as the previous line.
**  Note that the calling function is required to handle stop conditions
**  and act correctly if the next line is already labelled.
*/

get_next_cont_line (map, prev_line, type, cat)
    struct Map_info *map;
    plus_t prev_line;
    int type, cat;
{
    register int i;
    P_NODE *Node;
    P_LINE *Line;
    plus_t closest_line;
    double next_angle;
    double closest_angle, tmp;
    plus_t next_node, only_line;
    int found;

    closest_line = 0;
    only_line = 0;

    if (prev_line < 0)
	next_node = map->Line[abs(prev_line)].N2;
    else
	next_node = map->Line[prev_line].N1;


/*DEBUG*/ debugf ("In GET_NEXT: next_node = %d\n", next_node);
    Node = &(map->Node[next_node]);

    /* find current line in node info and get angle */
    found = 0;
    for (i = 0 ; i < Node->n_lines ; i++)
    {
	if (Node->lines[i] == prev_line)
	{
	    double prev_angle;

	    prev_angle = Node->angles[i];
	    next_angle = normalize_angle (prev_angle + PI); 
/*DEBUG*/ debugf ("PREV ANGLE %lf,  NEXT_ANGLE %lf\n", prev_angle, next_angle);
	    found = 1;
	    break;
	}
    }
    if (!found)	 /* ERROR: Line NOT attached to node! */
    {
/*DEBUG*/ debugf ("ERROR: Line(%d) NOT attached to node(%d)!\n", prev_line, next_node);
	return (0);
    }

    for (i = 0 ; i < Node->n_lines ; i++)
    {
	if (Node->lines[i] == prev_line)
	    continue;

	Line = &(map->Line[abs (Node->lines[i])]);

	/* if different line types, continue */
	if (type != Line->type) 
	    continue;
	
	/* if line is labelled a different value then continue */
	/* this needs to be cleaned up to allow RE-labelling of cont lines */
	if (cat)	/* else UNLABELLING */
	    if (LINE_LABELED (Line) && cat != map->Att[Line->att].cat)
		continue;

	if (ANGLE_VARIANCE < (tmp = 
	  fabs (normalize_angle (Node->angles[i]-next_angle))))
	{
/*DEBUG*/ debugf ("    NOT IN TOLERANCE (%lf)\n", tmp);
	    only_line = Node->lines[i];
	    continue;
	}
/*DEBUG*/ debugf ("    *** IN TOLERANCE (%lf)\n", tmp);

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



    /*
    **  If there is only one line continuing on, but it is not
    **  within threshold, go ahead and include it anyway
    **  this could be set up to be controlled by a flag if necessary
    */
    if (closest_line)
    {
/*DEBUG*/ debugf ("RETURNING  Closest_line = %d\n", closest_line);
	return (closest_line);
    }
    else
    {
/*DEBUG*/ debugf ("RETURNING  Only_line = %d\n", only_line);
	if (Node->n_lines == 2)
	    return (only_line);
	else
	    return (0);
    }
}

/* 
** take angle in radians and return a normalized value
**  between -PI and PI.
*/
static double
normalize_angle (angle)
    double angle;
{
    while (angle < -PI)
	angle += 2*PI;
    while (angle > PI)
	angle -= 2*PI;

    return angle;
}

#endif /* FOO */
