/*
**  Written by Dave Gerdes  6/1989
**  US Army Construction Engineering Research Lab
*/
#include <Vect.h>
#include "local_proto.h"

static int open_space ();

int build_area_one (struct Map_info *map)
{
    double N, S, E, W;

    get_univ_bbox (map, &N, &S, &E, &W);
    open_space (map);  /* make room for universe box */
    fill_univ_info (map, N, S, E, W);

    return 0;
}
    
/*
** 
** this routine is a clone of compress nodes from digit 
** its purpose is to scoot Nodes,lines and areas up by one(two)
**  to allow space for A1 and A2 information for the DLG file
*/

static int open_space (struct Map_info *map)
{
    register int to, from;
    P_NODE *Node;
    P_LINE *Line;
    P_AREA *Area;
    P_ISLE *Isle;
    P_ATT  *Att;
    int n_nodes, n_lines, n_areas, n_atts, n_isles;
    int area, isle;
    int new_count;


    Area = map->Area;
    Node = map->Node;
    Line = map->Line;
    Isle = map->Isle;
    Att  = map->Att;
    n_nodes = map->n_nodes;
    n_lines = map->n_lines;
    n_areas = map->n_areas;
    n_isles = map->n_isles;
    n_atts = map->n_atts;

	 /********************/
	/*  Scootch nodes   */
       /********************/

    dig_alloc_node (map, 1);
    Node = map->Node;
    new_count = ++map->n_nodes;
    from = 1; to = new_count;
    {
	register int num_lines;
	register int line;

	dig_struct_copy (Node + from, Node + to, sizeof (P_NODE));

	num_lines = Node[to].n_lines;
	for (line = 0 ; line < num_lines ; line++)
	{
	    if (Node[to].lines[line] < 0)
	    {
		Line[abs(Node[to].lines[line])].N2 = to;
	    }
	    else
	    {
		Line[Node[to].lines[line]].N1 = to;
	    }
	}
    }


	 /********************/
	/*  Scootch  LINES  */
       /********************/

    dig_alloc_line (map, 1);
    Line = map->Line;
    new_count = ++(map->n_lines);
    from = 1;  to = new_count;
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

    

	 /********************/
	/*  Scootch  AREAS  */
       /********************/

    dig_alloc_area (map, 2);
    Area = map->Area;
    new_count = map->n_areas + 2;
    for (to = map->n_areas + 1, from = 1 ; from <= 2 ; from++, to++)
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
    map->n_areas += 2;

    Area[1].alloc_lines = Area[1].n_lines = 0;
    Area[2].alloc_lines = Area[2].n_lines = 0;
    Area[1].alloc_isles = Area[1].n_isles = 0;
    Area[2].alloc_isles = Area[2].n_isles = 0;
    Area[1].lines = Area[2].lines = NULL;

    Node[1].alloc_lines = Node[1].n_lines = 0;

    return 0;
}

#ifndef PI
#define PI 3.141592
#endif

/* 
**  fill in map data structures with info about A1 and A2 which
**  area created soley for the DLG file
**  Note that the line point information does not really exist.
*/
int fill_univ_info (struct Map_info *map,
    double N, double S, double E, double W)
{
    register int i;
    P_LINE *Line;
    P_NODE *Node;
    P_AREA *Area;

    Line = &(map->Line[1]);
    Node = &(map->Node[1]);

    Line->N1 = Line->N2 = 1;
    Line->left =  1;  /* Area 1 NOT Island 1 */
#ifdef FOO
    Line->left = -1;  /* Area 1 NOT Island 1 */
#endif
    Line->right = 2;
    Line->N = N;
    Line->S = S;
    Line->E = E;
    Line->W = W;
    Line->offset = 0;  /* line does NOT really exist */
    Line->att = 0;
    Line->type = AREA;

    Node->x = W;
    Node->y = N;
    dig_node_alloc_line (Node, 2);
    Node->n_lines = 2;
    Node->lines[0] = 1;
    Node->lines[1] = -1;
    Node->angles[0] = 0.0;
    Node->angles[0] = PI * 3./2.;
    Node->alive = 1;

    /* build area A2  universe outside bounding line */
    Area = &(map->Area[1]);
    Area->N = N;
    Area->S = S;
    Area->E = E;
    Area->W = W;
    Area->att = 0;
    Area->n_lines = 1;
    Area->alive = 1;
    dig_area_alloc_line (Area, 1);
    Area->lines[0] = -1;
    Area->n_isles = 0;

    /* build area A2  inside bounding line */
    Area = &(map->Area[2]);
    Area->N = N;
    Area->S = S;
    Area->E = E;
    Area->W = W;
    Area->att = 0;
    Area->n_lines = 1;
    Area->alive = 1;
    dig_area_alloc_line (Area, 1);
    Area->lines[0] = 1;
    for (i = 1 ; i <= map->n_isles ; i++)
    {
	if (map->Isle[i].area == 0)  /* universe island */
	{
	    /* dig_area_alloc_line (Area, 1);  dpg 1/2/92 */
	    dig_area_alloc_isle (Area, Area->n_isles+1);
	    Area->isles[Area->n_isles++] = i;
	    map->Isle[i].area = 2;
	}
    }

    return 0;
}

/*
** go through all lines and find the Bounding Box that encloses
** every line
**  then make it a little bigger 
*/
#define XTRA_PERC  0.05

int get_univ_bbox (struct Map_info *map,
    double *rN, double *rS, double *rE, double *rW)
{
    register int i;
    double N, S, E, W;
    double xtra;
    double ytra;
    P_LINE *Line;

    Line = &(map->Line[1]);
    N = Line->N; S = Line->S;  /* get init values */
    E = Line->E; W = Line->W;

    for (i = 1 ; i <= map->n_lines ; i++)
    {
	Line = &(map->Line[i]);
	if (Line->N > N)
	    N = Line->N;
	if (Line->S < S)
	    S = Line->S;
	if (Line->E > E)
	    E = Line->E;
	if (Line->W < W)
	    W = Line->W;
    }

    xtra = (E - W) * XTRA_PERC;
    ytra = (N - S) * XTRA_PERC;
    *rN = N + ytra;
    *rS = S - ytra;
    *rE = E + xtra;
    *rW = W - ytra;

    return 0;
}
