/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/
#include "Vect.h"
#include "export_dlg.h"
#include "local_proto.h"


#define NODE_FMT "N%5d%12.2f%12.2f%6d%6d%6d%6d%6d            \n"

/*
**  node atts are currenlty not supported in digit, but this code should
**  work when it is.
**
*/

int write_dlg_nodes (
    struct Map_info *map,
    FILE *fp)
{
    char buf[100];
    P_NODE *Node;
    P_LINE *Line;
    int n_atts;
    int n_lines;
    register int node, i;

    for (node = 1 ; node <= map->n_nodes ; node++)
    {
	n_atts = 0;
	n_lines = 0;

	Node = &(map->Node[node]);

	/* count how many atts will have */
	for (i = 0 ; i < Node->n_lines ; i++)
	{
	    if (map->Line[abs (Node->lines[i])].type == DOT)
	    {
		/* only get the positive lines.  remember that both
		** ends of the DOT line will come into this node 
		*/
		if (Node->lines[i] > 0 && map->Line[Node->lines[i]].att)
		    n_atts++;
	    }
	    else
		n_lines++;
	}

	fprintf (fp, NODE_FMT, 
		node, 		 	/* index of element */
		Node->x, Node->y,	/* coordinates */
		0,			/* unused */
		n_lines,		/* number of attached lines */
		0,			/* unused */
		n_atts,			/* # of att codes */
		0);			/* unused */

	if (Node->n_lines)
	{
	    start_ints ();
	    for (i = 0 ; i < Node->n_lines ; i++)
	    {
		if (map->Line[abs (Node->lines[i])].type == DOT)
		    continue;
		write_ints (fp, 1, &(Node->lines[i]));
	    }
	    end_ints (fp);
	}

	if (n_atts)
	{
	    start_att ();
	    for (i = 0 ; i < Node->n_lines ; i++)
	    {
		/* 
		** only look at the positive ends of DOTs
		*/
		if (Node->lines[i] > 0)
		{
		    Line = &(map->Line[Node->lines[i]]);
		    if (Line->type == DOT)
			if (Line->att)
			    write_dlg_att (fp, DEF_MAJOR, map->Att[Line->att].cat);
		}
	    }
	    end_att (fp);
	}
    }

  return 0;
}
