#include "Vect.h"

/* swap two P_LINEs and all references to them:
**   Area->lines
**   Node->lines
**   Isle->lines
**   Att->index
**
**  This is destined to become a library routine along w/ area,node,etc_swap
*/
line_swap (map, a, b)
    struct Map_info *map;
    int a, b;
{

    P_LINE *Line, tmp;

    Line = map->Line;
    dig_struct_copy (Line + a, &tmp, sizeof (P_LINE));
    dig_struct_copy (Line + b, Line + a, sizeof (P_LINE));
    line_fixup (map, b, a);
    dig_struct_copy (&tmp, Line + b, sizeof (P_LINE));
    line_fixup (map, a, b);
}


/* 
**  fixes all references to a P_LINE struct that has moved
**
**  note that the 'from' structure is never directly referenced
*/
static
line_fixup (map, from, to)
    struct Map_info *map;
    int from, to;
{
    register int line, i;
    register int node1 ,node2;
    int area, isle;
    P_NODE *Node;
    P_LINE *Line;
    P_AREA *Area;
    P_ISLE *Isle;
    P_ATT  *Att;

    Area = map->Area;
    Node = map->Node;
    Line = map->Line;
    Isle = map->Isle;
    Att  = map->Att;

    Line = map->Line;

    /* if it was dead, dont bother fixing it up */
    if (!LINE_ALIVE (Line+to))
	return;

    /* FIXUP Nodes */
    node1 = Line[to].N1;
    node2 = Line[to].N2;
    for (line = 0 ; line < Node[node1].n_lines ; line++)
	if (abs (Node[node1].lines[line]) == from)
	    Node[node1].lines[line] = Node[node1].lines[line] < 0 ? -to : to;
    for (line = 0 ; line < Node[node2].n_lines ; line++)
	if (abs (Node[node2].lines[line]) == from)
	    Node[node2].lines[line] = Node[node2].lines[line] < 0 ? -to : to;

    /* FIXUP atts */
    if (Line[to].att)
    {
/*DEBUG*/	    if (Att[Line[to].att].index != from)
/*DEBUG*/ 		debugf ("COMPRESS: att %d  doesnt match line %d\n",
/*DEBUG*/ 		    Line[to].att, from);
/*DEBUG*/	    else
	    Att[Line[to].att].index = to;
    }

    /* FIXUP AREAS */
    if (Line[to].right > 0)
    {
	area = Line[to].right;
	for (i = 0 ; i < Area[area].n_lines ; i++)
	{
	    if (from == abs(Area[area].lines[i]))
	    {
		Area[area].lines[i] = Area[area].lines[i] < 0 ? -to : to;
		break;
	    }
	}
/*DEBUG*/	    if (i >= Area[area].n_lines)
/*DEBUG*/ 		debugf ("Line %d reference Area %d. Not in area\n",  from, area);
    }
    if (Line[to].left > 0)
    {
	area = Line[to].left;
	for (i = 0 ; i < Area[area].n_lines ; i++)
	{
	    if (from == abs(Area[area].lines[i]))
	    {
		Area[area].lines[i] = Area[area].lines[i] < 0 ? -to : to;
		break;
	    }
	}
/*DEBUG*/	    if (i >= Area[area].n_lines)
/*DEBUG*/ 		debugf ("Line %d reference Area %d. Not in area\n",  from, area);
    }

    /* FIXUP ISLES */
    if (Line[to].right < 0)
    {
	isle = abs (Line[to].right);
	for (i = 0 ; i < Isle[isle].n_lines ; i++)
	{
	    if (from == abs(Isle[isle].lines[i]))
	    {
		Isle[isle].lines[i] = Isle[isle].lines[i] < 0 ? -to : to;
		break;
	    }
	}
/*DEBUG*/	    if (i >= Isle[isle].n_lines)
/*DEBUG*/ 		debugf ("Line %d reference Isle %d. Not in isle\n",  from, isle);
    }
    if (Line[to].left < 0)
    {
	isle = abs (Line[to].left);
	for (i = 0 ; i < Isle[isle].n_lines ; i++)
	{
	    if (from == abs(Isle[isle].lines[i]))
	    {
		Isle[isle].lines[i] = Isle[isle].lines[i] < 0 ? -to : to;
		break;
	    }
	}
/*DEBUG*/	    if (i >= Isle[isle].n_lines)
/*DEBUG*/ 		debugf ("Line %d reference Isle %d. Not in isle\n",  from, isle);
    }
}
