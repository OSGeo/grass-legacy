#include <unistd.h>
#include <math.h>
#include "digit.h"
#include "raster.h"
#include "dig_curses.h"
#include "keyboard.h"
#include "Map_proto.h"
#include "local_proto.h"

/*  turns on (highlights) any beginning or ending nodes within the user set
*    threshold of any other beginning or ending node.
*/


int 
within_a_thresh (struct Map_info *map)
{

    register int i,j;

    int	have_match;
    double    new_thresh;
    P_NODE *node;

    char    buf[80];
    int ret;

    sprintf ( buf, " Current threshold: %8.2f .    Enter temp threshold:  ",
    map->snap_thresh);
    Write_info ( 2, buf);

    Get_curses_text(buf);
    sscanf (buf, "%lf", &new_thresh);

    if (new_thresh <= map->snap_thresh)
     {
	Write_info ( 3, "  The threshold entered is less then the current threshold.");
	Write_info ( 4, "  No action taken.");

	sleep (3);
	return (-1);
     }

    Write_info ( 3, " .. processing ..                 ...Press <ESC> key to stop");

    /*  loop thru coord.[] for each coord   */

    node = map->Node;

    R_standard_color (dcolors[CLR_HIGHLIGHT]);
    set_keyboard ();
    for (i=1 ; i <= map->n_nodes; i++)
    {
	if (key_hit (buf))
	{
	    if (*buf == ESC)
	    {
		ret = -1;
		break;
	    }
	}

	if (! NODE_ALIVE (&(node[i])))	/* removed node  */
	    continue;
    
	have_match = 0;
	for (j=1 ; j <= map->n_nodes ; j++)
	{

	    if ( ! NODE_ALIVE (&(node[j])))
		continue;
    
	    if (j == i)	    /*  node we're looking at  */
		continue;
    
	    if ( (fabs(node[j].x - node[i].x) < new_thresh) &&
		 (fabs(node[j].y - node[i].y) < new_thresh) )
	     {
		Blot (&(node[j].x), &(node[j].y));
		have_match = 1;
	     }
	}
    
	if (have_match)
	    Blot (&(node[i].x), &(node[i].y));
    }
    unset_keyboard ();
    return 1;
}	/*  within_a_thresh ()  */
