/*
**  Written by Dave Gerdes  8/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"


/*
** highlight every area line that does not have an area attached to it. 
** this is to aid finding bad areas that were not completed by support.vect 
** this should NOT be used untill after labeling entire map OR running  
** support.vect on the file 
*/
unfinished_areas (map)
    struct Map_info *map;
{
    register int i, error;
    char buf[10];
    P_LINE *Line;
    int ret;

    _Clear_info ();
    ret = 0;
    error = 0;
    set_keyboard ();
    Write_info (3, "                                  ...Press <ESC> key to stop redraw");
    for (i = 1 ; i <= map->n_lines ; i++)
    {
	if (key_hit (buf))
	{
	    if (*buf == ESC)
	    {
		ret = -1;
		break;
	    }
	}

	Line = &(map->Line[i]);

	/* note this code is cool for islands */
	/* Even if line is attached to an island, if not to an */
	/* area, something is wrong */

	if (Line->type != AREA)
	    continue;
	if (LINE_ALIVE (Line) && (Line->right <= 0 && Line->left <= 0))
	{
	    if(0 > V1_read_line (map, &Gpoints, Line->offset))
	    {
		if (error > 10)  /* ... it's late.. */
		{
		    Write_info (3, "Multiple Errors reading digit file!");
		    sleep (2);
		    return (-1);
		}
		error++;
		continue;
	    }
		
	    _highlight_line(Line->type, &Gpoints, i, map);
	}
    }
    unset_keyboard ();
    V_flush ();
    return (ret);
}
