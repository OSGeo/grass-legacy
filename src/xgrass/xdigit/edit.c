/*
**  Written by Mike Higgins 
**    Last modified by Dave Gerdes  4/1988
**  US Army Construction Engineering Research Lab
*/


#include "digit.h"
#include "dig_head.h"
#include <stdio.h>

/*
**  Edit menu 
**
**  driver routine for Edit menu
**
*/
void
Edit(w, command)
    int w;
    char command;
{
	    switch(command)
	    {
	    case MEC_REMOVE:
		remove_line (CM, LINE | AREA);
		break;
	    case MEC_BLOCK:
		remove_block (CM);
		break;
	    case MEC_RMVSIT:
		remove_line (CM, DOT);
		break;
	    case MEC_SNAP:
		snap_nodes(CM);
		break;
	    case MEC_MOVE:
		move_point (CM);
		break;
	    case MEC_MOVEL:
		move_line (CM);
		break;
#ifdef SCS_MODS 
            case MEC_INSEC:
                intersect_line (CM);
                break;
#endif /* SCS_MODS */
	    case MEC_BREAK:
		break_line (CM);
		break;
	    case MEC_TYPE:
		retype_line (CM);
		break;
	    default:
		break;	 /* should not get here */
	    }
}
