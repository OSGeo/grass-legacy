/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include <unistd.h>
#include "digit.h"
#include "debug.h"
#include "Map_proto.h"
#include "dig_curses.h"
#include "local_proto.h"

int write_out (int tell)
{
    FILE *fp;
    struct Plus_head phead;

    compress (CMap, tell);

    if ((fp = fopen (CMap->plus_file, "w")) == NULL)
    {
	fprintf (stderr, "Cannot open '%s' for write\n", CMap->plus_file);
	return (-1);
    }

    phead.all_areas = 0;
    phead.all_isles = 0;
    Write_info (3, "Writing Plus File ...");
    dig_map_to_head (CMap, &phead);
    if (0 > dig_write_plus_file (fp, CMap, &phead))
    {
	fclose (fp);
	return (-1);
    }
    fclose (fp);
    if (tell)
    {
	Write_info (3, "Writing Plus File ... DONE.");
	sleep (1);
    }
    return (0);
}

int compress (struct Map_info *map, int tell)
{
    register int to, from;
    P_NODE *Node;
    P_LINE *Line;
    P_AREA *Area;
    P_ISLE *Isle;
    P_ATT  *Att;
    /* int n_nodes, n_lines, n_areas, n_atts, n_isles; */
    int area, isle;
    int new_count;

/*DEBUG*/ debugf ("\nCOMPRESS: Map = %lx  Tell: %d  Map->digit_file = '%s'\n", map, tell, map->digit_file);


/*DEBUG*/ debugf ("Checkpoint A\n");
    if (tell)
	Write_info (2, "Compressing Data: ");
    Node = map->Node;
    Area = map->Area;
    Line = map->Line;
    Isle = map->Isle;
    Att  = map->Att;
    /*
    n_nodes = map->n_nodes;
    n_lines = map->n_lines;
    n_areas = map->n_areas;
    n_isles = map->n_isles;
    n_atts = map->n_atts;
    */

	 /********************/
	/*  COMPRESS NODES  */
       /********************/

    if (tell)
    Write_info (3, " Compressing NODES.");
    new_count = map->n_nodes;
    for (to = from = 1 ; from <= map->n_nodes ; from++)
    {
	/* note if node has no lines but is alive, (point marker) */
	/* we will remove it */
	if (NODE_ALIVE (&(Node[from])) && Node[from].n_lines)
	{
/*DEBUG*/ if (Node[from].n_lines == 0) debugf ("ZERO Node %d\n", from);
	    if (from != to)
	    {
		register int num_lines;
		register int line;

		dig_struct_copy (Node + from, Node + to, sizeof (P_NODE));

		num_lines = Node[to].n_lines;
		for (line = 0 ; line < num_lines ; line++)
		{
		    if (Node[to].lines[line] < 0)
			Line[abs(Node[to].lines[line])].N2 = to;
		    else
			Line[Node[to].lines[line]].N1 = to;
		}
	    }
	    to++;
	}
	else new_count--;
    }


/*DEBUG*/ debugf ("Deleted %d NODES\n", map->n_nodes - new_count);
    map->n_nodes = new_count;


	 /********************/
	/*  COMPRESS LINES  */
       /********************/

    if (tell)
    Write_info (3, " Compressing NODES.  LINES.");
    new_count = map->n_lines;
    for (to = from = 1 ; from <= map->n_lines ; from++)
    {
	if (LINE_ALIVE (&(Line[from])))
	{
	    if (from != to)
	    {
		register int line, i;
		register int node1 ,node2;

		dig_struct_copy (Line + from, Line + to, sizeof (P_LINE));

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
	    to++;
	}
	else  new_count--;
    }
/*DEBUG*/ debugf ("Deleted %d LINES\n", map->n_lines - new_count);
    map->n_lines = new_count;

    

	 /********************/
	/*  COMPRESS AREAS  */
       /********************/

    if (tell)
    Write_info (3, " Compressing NODES.  LINES.  AREAS.");
    new_count = map->n_areas;
    for (to = from = 1 ; from <= map->n_areas ; from++)
    {
	if (AREA_ALIVE (&(Area[from])))
	{
	    if (from != to)
	    {
		register int i;
		dig_struct_copy (Area + from, Area + to, sizeof (P_AREA));

		/* FIXUP atts */
		if (Area[to].att)
		{
/*DEBUG*/	    if (Att[Area[to].att].index != from)
/*DEBUG*/ 		debugf ("COMPRESS: att %d  doesnt match area %d\n",
/*DEBUG*/ 		    Area[to].att, from);
/*DEBUG*/	    else
			Att[Area[to].att].index = to;
		}
		/* FIXUP lines */
		for (i = 0 ; i < Area[to].n_lines ; i++)
		{
		    if (LINE_ALIVE (&(Line[abs(Area[to].lines[i])])))
		    {
			if (Line[abs(Area[to].lines[i])].right == from)
			    Line[abs(Area[to].lines[i])].right = to;
			if (Line[abs(Area[to].lines[i])].left == from)
			    Line[abs(Area[to].lines[i])].left = to;
		    }
		}

		/* FIXUP ISLES */
		for (i = 0 ; i < Area[to].n_isles ; i++)
		{
		    if (Isle[Area[to].isles[i]].area == from)
			Isle[Area[to].isles[i]].area = to;
		}
	    }
	    to++;
	}
	else  new_count--;
    }
/*DEBUG*/ debugf ("Deleted %d AREAS\n", map->n_areas - new_count);
    map->n_areas = new_count;


	 /********************/
	/*  COMPRESS ISLES  */
       /********************/

    if (tell)
    Write_info (3, " Compressing NODES.  LINES.  AREAS.  ISLES.");
/*DEBUG*/ debugf ("number of isles: %d\n", map->n_isles);
    new_count = map->n_isles;
    for (to = from = 1 ; from <= map->n_isles ; from++)
    {
/*DEBUG*/ debugf ("Checkpoint  1\n");
	if (ISLE_ALIVE (&(Isle[from])))
	{
/*DEBUG*/ debugf ("Checkpoint  2\n");
	    if (from != to)
	    {
		register int i;
		dig_struct_copy (Isle + from, Isle + to, sizeof (P_ISLE));

/*DEBUG*/ debugf ("Checkpoint  3\n");

		/* FIXUP lines */
		for (i = 0 ; i < Isle[to].n_lines ; i++)
		{
		    if (LINE_ALIVE (&(Line[abs(Isle[to].lines[i])])))
		    {
			if (Line[abs(Isle[to].lines[i])].right == -from)
			    Line[abs(Isle[to].lines[i])].right = -to;
			if (Line[abs(Isle[to].lines[i])].left == -from)
			    Line[abs(Isle[to].lines[i])].left = -to;
		    }
		}
/*DEBUG*/ debugf ("Checkpoint  4\n");

		/* FIXUP AREA */
		if (AREA_ALIVE (&(Area[Isle[to].area])))
		    for (i = 0 ; i < Area[Isle[to].area].n_isles ; i++)
			if (Area[Isle[to].area].isles[i] == from)
			    Area[Isle[to].area].isles[i] = to;
	    }
	    to++;
	}
	else  new_count--;
    }
/*DEBUG*/ debugf ("Deleted %d ISLES\n", map->n_isles - new_count);
    map->n_isles = new_count;


	 /********************/
	/*  COMPRESS ATTS   */
       /********************/


    if (tell)
    Write_info (3, " Compressing NODES.  LINES.  AREAS.  ISLES. ATTS.");
    new_count = map->n_atts;
    for (to = from = 1 ; from <= map->n_atts ; from++)
    {
	if (ATT_ALIVE (&(Att[from])))
	{
	    if (from != to)
	    {
		dig_struct_copy (Att + from, Att + to, sizeof (P_ATT));

		/* FIXUP lines and areas */
		switch (Att[to].type) {
		    case AREA:
			Area[Att[to].index].att = to;
			break;
		    case LINE:
		    case DOT:
			Line[Att[to].index].att = to;
			break;
		    default:
			break;
		}
	    }
	    to++;
	}
	else  new_count--;
    }
/*DEBUG*/ debugf ("Deleted %d ATTS\n", map->n_atts - new_count);
    map->n_atts = new_count;
    if (tell)
    {
	_Write_info (2, "Compressing Data:   DONE. ");
	Write_info (3, "");
	sleep (1);
    }

    return 0;
}
