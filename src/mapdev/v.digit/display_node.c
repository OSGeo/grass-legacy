/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "raster.h"
#include "display_node.h"
#include "Map_proto.h"
#include "local_proto.h"

int display_node (int node, struct Map_info *map)
{
    P_NODE *Node;

    Node = &(map->Node[node]);
    if (NODE_ALIVE (Node))
    {
	R_standard_color( dcolors[dig_node_color (Node->n_lines)]);
	_Blot (&(Node->x), &(Node->y));
    }
    V_flush ();
    return 0;
}

int highlight_node (int node, struct Map_info *map)
{
    P_NODE *Node;

    Node = &(map->Node[node]);
    if (NODE_ALIVE (Node))
    {
	R_standard_color( dcolors[CLR_HIGHLIGHT]);
	_Blot (&(Node->x), &(Node->y));
    }
    V_flush ();
    return 0;
}

int color_node (int node, struct Map_info *map, int color)
{
    P_NODE *Node;

    Node = &(map->Node[node]);
    if (NODE_ALIVE (Node))
    {
	R_standard_color( dcolors[color]);
	_Blot (&(Node->x), &(Node->y));
    }
    V_flush ();
    return 0;
}

int erase_node (int node, struct Map_info *map)
{
    P_NODE *Node;

    Node = &(map->Node[node]);
    if (NODE_ALIVE (Node))
    {
	R_standard_color( dcolors[CLR_ERASE]);
	_Blot (&(Node->x), &(Node->y));
    }
    V_flush ();
    return 0;
}
