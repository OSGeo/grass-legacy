/*
**  Written by Mike Higgins
**     Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "debug.h"
#include "dig_curses.h"
#include "display_line.h"
#include "Map_proto.h"
#include "local_proto.h"

/* ask user to pick line w/ mouse   then delete it. 
** 
**  Returns      1 deleted
** 		 0 aborted
**	 	-1 error  (out of memory or PREMATURE EOF)
*/

int remove_line (struct Map_info *map, int mtype)
{
    char *str;
    int line;

    str = (mtype == DOT) ? " Remove a site:" : " Remove a line:";
    while (1)
    {
	Clear_info();
	/* find_line fills Gpoints */
	if (0 >= (line = find_line_with_mouse (mtype,  str, NULL)))
	{
	    return (0);
	}

#ifdef FOO
	/* this is redundant, as find_line already got it for me */
	if (0 > V1_read_line (map, &Gpoints, map->Line[line].offset))
	    return (-1);
#endif

	_remove_line (map, line);
    }
}

/* delete a line, erasing it from the screen  
**  on entry Gpoints must contain the line 
**  -  updates dig file
**  -  does not delete its attribute
*/
int 
_remove_line (struct Map_info *map, int line)
{
    char type;
    int att;

    Write_info ( 2, "");

    if (!LINE_ALIVE (&(map->Line[line])) )
	return (0);
    Changes_Made = 1;
    if (map->Line[line].type == AREA)
    {
/*DEBUG*/ debugf ("Removing Areas on each side R: %d L: %d\n", map->Line[line].right, map->Line[line].left);
	/* remove any bounded areas */
	if (map->Line[line].right > 0)
	    Del_area (map, map->Line[line].right);
	if (map->Line[line].right < 0)			/* ISLE */
	    Del_isle (map, abs(map->Line[line].right));
	if (map->Line[line].left > 0)
	    Del_area (map, map->Line[line].left);
	if (map->Line[line].left < 0)   		/* ISLE */
	    Del_isle (map, abs (map->Line[line].left));
    }
    if (0 > V1_read_line (map, &Gpoints, map->Line[line].offset))
	return (-1);
    erase_line (map->Line[line].type, &Gpoints, line, map);

    dig_node_del_line (&(map->Node[map->Line[line].N1]), line);
    dig_node_del_line (&(map->Node[map->Line[line].N2]), -line);

    /* map->n_lines does not change, as they are still allocated */
    /* till compress () */
    switch(map->Line[line].type)
    {
	case AREA:
	    type = DEAD_AREA;
	    map->n_alines--;
	    break;
	case LINE:
	    type = DEAD_LINE;
	    map->n_llines--;
	    break;
	case DOT:
	    type = DEAD_DOT;
	    map->n_plines--;
	    break;
	default:
/*DEBUG*/ debugf ("REMOVE: BAD 'TYPE' Code.  %d\n", (int) map->Line[line].type);
	    type = DEAD_LINE;
	    break;
    }
    map->Line[line].type = type;
    map->n_points -= Gpoints.n_points;

    /* delete its attribute */
    if (att = map->Line[line].att)
	dig_del_att (map, att);

    Vect__Rewrite_line (map, map->Line[line].offset, type, &Gpoints);
	return(0) ;
}



/* Same as _remove_line() except does not update screen */
/* added Nov 1992 -dpg  */

int __remove_line (struct Map_info *map, int line)
{
    char type;
    int att;

    Write_info ( 2, "");

    if (!LINE_ALIVE (&(map->Line[line])) )
	return (0);
    Changes_Made = 1;
    if (map->Line[line].type == AREA)
    {
/*DEBUG*/ debugf ("Removing Areas on each side R: %d L: %d\n", map->Line[line].right, map->Line[line].left);
	/* remove any bounded areas */
	if (map->Line[line].right > 0)
	    _Del_area (map, map->Line[line].right);
	if (map->Line[line].right < 0)			/* ISLE */
	    Del_isle (map, abs(map->Line[line].right));
	if (map->Line[line].left > 0)
	    _Del_area (map, map->Line[line].left);
	if (map->Line[line].left < 0)   		/* ISLE */
	    Del_isle (map, abs (map->Line[line].left));
    }
    if (0 > V1_read_line (map, &Gpoints, map->Line[line].offset))
	return (-1);
    /*
    erase_line (map->Line[line].type, &Gpoints, line, map);
    */

    dig_node_del_line (&(map->Node[map->Line[line].N1]), line);
    dig_node_del_line (&(map->Node[map->Line[line].N2]), -line);

    /* map->n_lines does not change, as they are still allocated */
    /* till compress () */
    switch(map->Line[line].type)
    {
	case AREA:
	    type = DEAD_AREA;
	    map->n_alines--;
	    break;
	case LINE:
	    type = DEAD_LINE;
	    map->n_llines--;
	    break;
	case DOT:
	    type = DEAD_DOT;
	    map->n_plines--;
	    break;
	default:
/*DEBUG*/ debugf ("REMOVE: BAD 'TYPE' Code.  %d\n", (int) map->Line[line].type);
	    type = DEAD_LINE;
	    break;
    }
    map->Line[line].type = type;
    map->n_points -= Gpoints.n_points;

    /* delete its attribute */
    if (att = map->Line[line].att)
	dig_del_att (map, att);

    Vect__Rewrite_line (map, map->Line[line].offset, type, &Gpoints);
	return(0) ;
}
