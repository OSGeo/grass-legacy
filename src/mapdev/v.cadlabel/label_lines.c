#include "Vect.h"
#include "Vect.h"
#include "local_proto.h"

/***********************************************************************
** code for labelling contour lines follows 
**  this should be changed to call dig_get_cont_lines ()
***********************************************************************/
int label_lines (
    struct Map_info *map,
    int line,
    int cat)
{
    int tmp, next_line, prev_line;

    line = abs (line);

    label_line (map, line, cat);

    prev_line = line;
    while (1)
    {
	if (0 == (next_line = get_next_line (map, -prev_line)))
	    break;
	if (abs (line) == abs (next_line))
	    break;
	/* if ran into another label */
	if (map->Line[abs(next_line)].att &&
	    (tmp = map->Att[map->Line[abs(next_line)].att].cat) != cat)
	{
	    fprintf (stderr, "Label %d ran into label %d, Line %d\n", cat, tmp, next_line);
	    break;
	}

	label_line (map, next_line, cat);
	prev_line = next_line;
    }

    prev_line = -line;
    while (1)
    {
	if (0 == (next_line = get_next_line (map, -prev_line)))
	    break;
	if (abs (line) == abs (next_line))
	    break;
	/* if ran into another label */
	if (map->Line[abs(next_line)].att &&
	    (tmp = map->Att[map->Line[abs(next_line)].att].cat) != cat)
	{
	    fprintf (stderr, "Label %d ran into label %d, Line %d\n", cat, tmp, next_line);
	    break;
	}

	label_line (map, next_line, cat);
	prev_line = next_line;
    }

    return (0);
}

int get_next_line (
    struct Map_info *map,
    int prev_line)
{
    P_NODE *Node;
    int next_node;

    if (prev_line < 0)
	next_node = map->Line[abs(prev_line)].N2;
    else
	next_node = map->Line[prev_line].N1;

    Node = &(map->Node[next_node]);
    if (Node->n_lines == 1)
	return (0);
    if (Node->n_lines > 2)
    {
	fprintf (stderr, "Node #%d: %d lines intersect here\n", next_node, Node->n_lines);
	return (0);
    }

    if (Node->lines[0] == prev_line)
	return Node->lines[1];
    else
	return Node->lines[0];
}
    

int label_line (
    struct Map_info *map,
    int line,
    int cat)
{
    int att;
    double x, y;
    int line_type;
    P_LINE *Line;
    P_ATT *Att;
    static struct line_pnts *Points;
    static int first_time = 1;

    if (first_time)
    {
	Points = Vect_new_line_struct ();
	first_time = 0;
    }

    line = abs (line);
    Line = &(map->Line[line]);
    if (0 > V1_read_line (map, Points, Line->offset)) 
    {
	fprintf (stderr, "Label_line: Can't read line %d offset %ld\n", line, Line->offset);
	exit (-1);
    }

    line_type = Line->type;

    /* area and line lines all get labelled as LINE */
    if (line_type == AREA)
        line_type = LINE;
 
    /* remove old label from screen */
 
    get_line_center (&x, &y, Points);
 
    if (Line->att) /* if already exists, change it */
    {
        att = Line->att;
        Att = &(map->Att[att]);
        Att->cat = cat;
        Att->x = x;
        Att->y = y;
        dig_update_att (map, att);
    }
    else
    {   
        att = dig_new_att (map, x, y, line_type, line, cat);
        if (att < 0)
            return (-1);
        Line->att = att;
    }
    return (0);
}
