
/*  @(#)mark.c    2.1  6/26/87  */
/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/


/*
*    puts a point on the graphic monitor to mark lines
*    and temporarily creates a node, that can be snapped to.
*/

#ifdef FOO

#include "digit.h"
#include "dig_head.h"


mark_point (map)
    struct Map_info *map;
{

    int	num, key;
    double    x,    y;
    int first_key;
    char buff[60];
    char tmp[255];

    first_key = D_start_button ();

    if (Point_Device == DIGITIZER)
    {
	_Write_info(1, "  Move digitizer cursor to point to mark");
	if (D_cursor_buttons ())
	{
	    sprintf (tmp, "  Press CURSOR key <%d> to quit, or any other to mark point: ", first_key);
	    Write_info(2, tmp);
	}
	else
	{
	    Write_info(2, "  And press <RETURN>. To quit enter 'q': ");
	}
    }
    else
    {
	_Write_info(1, "  Move mouse to point to mark");
    }
    /*
    Clear_info();
    */

    num = 0;
    while (1)
    {
	
    sprintf (buff, "	     Points Marked: %d  ", num++);
	Write_info (3, buff);

	/*
	buff[0] = NULL;
	*/

	key = get_point ( &x, &y, "" );
	if (key == -2 || key == 1)	/* user entered 'q' or hit first key */
	    break;

    /* This block creates a true node w/ 0 lines attached */
    /*  these are currently removed durring the compress step */
	 {
	    int node;

	    if (0 > dig_alloc_node (map, 1))
	    {
		return (-1);
	    }
	    node = dig_which_node (map, &x, &y, map->snap_thresh);
	    if (node > 0)
	    {
		BEEP;
		Write_info (3, "  NODE ALREADY EXISTS.");
		sleep (2);
		num--;
		continue;
	    }
	    node = ++(map->n_nodes);
	    map->Node[node].x = x;
	    map->Node[node].y = y;
	    map->Node[node].n_lines = 0;
		map->Node[node].alloc_lines = 0;

	    map->Node[node].alive = 1;
	}

	R_standard_color (dcolors[CLR_0_NODE]);
	Blot (&x, &y);
    }	    /*  while (1)  */

    Clear_info();
}	    /*  mark_point ()  */
#endif
