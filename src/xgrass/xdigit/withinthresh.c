#include "digit.h"
#include "dig_head.h"

/*  turns on (highlights) any beginning or ending nodes within the user set
*    threshold of any other beginning or ending node.
*/
static double old_thresh = 0.0;
get_thresh(w)
    Widget w;
{
    static char *answer;
    double thresh = 0.0;
    int ret;

    if (w)
    {
        answer = XmTextGetString 
			(XmSelectionBoxGetChild(w, XmDIALOG_COMMAND_TEXT));
        sscanf (answer, "%lf", &thresh);
        XtFree (answer);
        old_thresh = thresh;
        ret = within_a_thresh (thresh);
    }
    else
        ret = within_a_thresh (old_thresh);
    return (ret);
}


within_a_thresh (new_thresh)
    double    new_thresh;

{

    register int i,j;

    int	have_match;
    double fabs();
    P_NODE *node;
    struct Map_info *map;

    char    buf[80];
    int ret;

    map = CM;

    ret = 0;
    if (!new_thresh)
    {
	make_monolog(1, "No threshold specified");
	return (0);
    }
    
    if (new_thresh <= map->snap_thresh)
     {
	sprintf (buf, 
	    "Threshold entered is less than current threshold (%lf). ", 
		   map->snap_thresh);
	make_monolog(1, buf);

	return (-1);
     }


    /*  loop thru coord.[] for each coord   */

    node = map->Node;

    standard_color (dcolors[CLR_HIGHLIGHT]);
    TimeOutCursor (1);

    for (i=1 ; i <= map->n_nodes; i++)
    {
	 if (Check_for_interrupt())
	     break;
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
	ret += have_match;
    }
    TimeOutCursor (0);
    return (ret);

}	/*  within_a_thresh ()  */
