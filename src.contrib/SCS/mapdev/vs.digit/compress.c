/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
/* modified by RL Glenn  12/1991
** USDA, SCS, Tech. Infor. Sys. Division
*/


#include "digit.h"
#include "dig_head.h"
#include <stdio.h>

write_out (tell)
    int tell;
{
    FILE *fp;
    struct Plus_head phead;
    char buf[50];

    compress (CM, tell);

    if ((fp = fopen (CM->plus_file, "w")) == NULL)
    {
        sprintf (buf, "Cannot open to write   ");
        message[0] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[0],"%s", buf);
        sprintf (buf, " '%s'    ", CM->plus_file);
        message[1] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[1],"%s", buf);
        message[2] = " ";
        message[3] = '\0';

        Dchoose(MEN.name) ;
        popup_messg( "warning", 1) ;
	sleep(3);
        erase_popup("warning");
	return (-1);
    }

    phead.all_areas = 0;
    phead.all_isles = 0;

    sprintf (buf, "Writing Plus File ...         ");
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    message[1] = " ";
    message[2] = '\0';

    Dchoose(MEN.name) ;
    popup_messg( "info", 1) ;
    dig_map_to_head (CM, &phead);
    if (0 > dig_write_plus_file (fp, CM, &phead))
    {
	fclose (fp);
        erase_popup("info");
	return (-1);
    }
    fclose (fp);
    erase_popup("info");
    file_savd = 1;
    if (tell)
    {
        sprintf (buf, "Writing Plus File ... DONE    ");
        message[0] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[0],"%s", buf);
        message[1] = " ";
        message[2] = '\0';

        Dchoose(MEN.name) ;
        popup_messg( "info2", 1) ;
	sleep (1);
        erase_popup("info2");
    }
    return (0);
}

compress (map, tell)
    struct Map_info *map;
    int tell;
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
    char buf[50];

/*DEBUG*/ debugf ("\nCOMPRESS: Map = %lx  Tell: %d  Map->digit_file = '%s'\n", map, tell, map->digit_file);


/*DEBUG*/ debugf ("Checkpoint A\n");
    if (tell)
	{
        sprintf (buf, "Compressing Data:   ");
        message[0] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[0],"%s", buf);
        message[1] = " ";
        message[2] = '\0';

        Dchoose(MEN.name) ;
        popup_messg( "info1", 1) ;
	sleep (1);
	}
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
       {
       erase_popup("info1");
       sprintf (buf, "  Compressing NODES   ");
       message[1] = (char *) malloc (strlen (buf) + 1);
       sprintf(message[1],"%s", buf);
       message[2] = " ";
       message[3] = '\0';

       Dchoose(MEN.name) ;
       popup_messg( "nodes", 1) ;
       sleep (1);
       }
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
			Line[ABS(Node[to].lines[line])].N2 = to;
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
       {
       erase_popup("nodes");
       sprintf (buf, "  Compressing LINES   ");
       message[2] = (char *) malloc (strlen (buf) + 1);
       sprintf(message[2],"%s", buf);
       message[3] = " ";
       message[4] = '\0';

       Dchoose(MEN.name) ;
       popup_messg( "lines", 1) ;
       sleep(1);
       }

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
		    if (ABS (Node[node1].lines[line]) == from)
			Node[node1].lines[line] = Node[node1].lines[line] < 0 ? -to : to;
		for (line = 0 ; line < Node[node2].n_lines ; line++)
		    if (ABS (Node[node2].lines[line]) == from)
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
			if (from == ABS(Area[area].lines[i]))
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
			if (from == ABS(Area[area].lines[i]))
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
		    isle = ABS (Line[to].right);
		    for (i = 0 ; i < Isle[isle].n_lines ; i++)
		    {
			if (from == ABS(Isle[isle].lines[i]))
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
		    isle = ABS (Line[to].left);
		    for (i = 0 ; i < Isle[isle].n_lines ; i++)
		    {
			if (from == ABS(Isle[isle].lines[i]))
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
       {
       erase_popup("lines");
       sprintf (buf, "  Compressing AREAS   ");
       message[3] = (char *) malloc (strlen (buf) + 1);
       sprintf(message[3],"%s", buf);
       message[4] = " ";
       message[5] = '\0';

       Dchoose(MEN.name) ;
       popup_messg( "areas", 1) ;
       sleep(1);
       }
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
		    if (LINE_ALIVE (&(Line[ABS(Area[to].lines[i])])))
		    {
			if (Line[ABS(Area[to].lines[i])].right == from)
			    Line[ABS(Area[to].lines[i])].right = to;
			if (Line[ABS(Area[to].lines[i])].left == from)
			    Line[ABS(Area[to].lines[i])].left = to;
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
       {
       erase_popup("areas");
       sprintf (buf, "  Compressing ISLES   ");
       message[4] = (char *) malloc (strlen (buf) + 1);
       sprintf(message[4],"%s", buf);
       message[5] = " ";
       message[6] = '\0';

       Dchoose(MEN.name) ;
       popup_messg( "isles", 1) ;
       sleep(1);
       }
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
		    if (LINE_ALIVE (&(Line[ABS(Isle[to].lines[i])])))
		    {
			if (Line[ABS(Isle[to].lines[i])].right == -from)
			    Line[ABS(Isle[to].lines[i])].right = -to;
			if (Line[ABS(Isle[to].lines[i])].left == -from)
			    Line[ABS(Isle[to].lines[i])].left = -to;
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
       {
       erase_popup("isles");
       sprintf (buf, "  Compressing ATTS    ");
       message[5] = (char *) malloc (strlen (buf) + 1);
       sprintf(message[5],"%s", buf);
       message[6] = " ";
       message[7] = '\0';

       Dchoose(MEN.name) ;
       popup_messg( "atts", 1) ;
       sleep(1);
       }
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
       erase_popup("atts");
       sprintf (buf, "  Compressing Data:  DONE   ");
       message[6] = (char *) malloc (strlen (buf) + 1);
       sprintf(message[6],"%s", buf);
       message[7] = " ";
       message[8] = '\0';

       Dchoose(MEN.name) ;
       popup_messg( "done", 1) ;
       sleep (1);
       erase_popup("done");
    }
}
