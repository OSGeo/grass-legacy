
#include "digit.h"
#include "dig_head.h"

/*  turns on (highlights) any beginning or ending nodes within the user set
*    threshold of any other beginning or ending node.
*/


within_a_thresh (map)
    struct Map_info *map;
{

    register int i,j;

    int	have_match;
    double    new_thresh;
    double fabs();
    P_NODE *node;

    char    buf[80];
    int ret;

    sprintf ( buf, " Current threshold: %8.3lf    ", map->snap_thresh);
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    sprintf ( buf, "    Enter temp threshold:  ");
    message[1] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[1],"%s", buf);
    message[2] = " ";
    message[3] = " ";
    message[4] = '\0';

    Dchoose(MEN.name) ;
    popup_messg( "info", 1) ;
    popup_ques(20,&buf[0]);
    erase_popup("info");
    sscanf (buf, "%lf", &new_thresh);

    if (new_thresh <= map->snap_thresh)
     {

        sprintf (buf,"The threshold entered is less than .");
        message[0] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[0],"%s", buf);
	sprintf(buf,"or equal to the current threshold .");
        message[1] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[1],"%s", buf);
        message[2] = " ";
	sprintf(buf,"  No action taken .");
        message[3] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[3],"%s", buf);
        message[4] = '\0';

        Dchoose(MEN.name) ;
        popup_messg( "warning", 1) ;
	sleep(2);
        erase_popup("warning");
        Dchoose(DIG.name) ;
	return (-1);
     }

    sprintf(buf," .. processing ..");
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    sprintf(buf,"...Press < ESC > key to stop redraw .");
    message[1] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[1],"%s", buf);
    message[2] = '\0';
	
    Dchoose(MEN.name) ;
    popup_messg( "disp_over", 1) ;

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
	   /* ret = -1; */
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
    R_flush();
    erase_popup("disp_over");
}	/*  within_a_thresh ()  */
