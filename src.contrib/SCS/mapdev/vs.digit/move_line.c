/*  @(#)snap.c    2.1  6/26/87  */
/* modified by RL Glenn  12/1991
** USDA, SCS, Tech. Infor. Sys. Division
*/

#include "digit.h"
#include "dig_head.h"

#define	FRONT    0
#define	BACK    1

#define	YES    1
#define	NO    2
#define	QUIT    3

move_line (map)
    struct Map_info *map;
{
    double thresh ;
    double fabs() ;
    double ux1, uy1 ;
    double ux2, uy2 ;
    int i ;
    int point_num ;
    int ret;
    int line;
    int Save_Disp;
    double dx, dy;
    char type, buf[80];
    struct new_node node;
    int is_site;
    int att;

    screen_to_utm (0, 0, &ux1, &uy1) ;
    screen_to_utm (5, 0, &ux2, &uy2) ;
    thresh = fabs ( ux1 - ux2);

    Save_Disp = Disp_points;
    Disp_points = 1;


    while (1)
    {
	line = find_line_with_mouse (LINE|AREA|DOT, "Choose line or site to move:", NULL);
	if (line <= 0)
	{
	    ret = 0;
	    goto done;
	}

    /*  find which point in line to move  */
	if (is_site = (map->Line[line].type == DOT))
	    point_num = 0;
	else
	{
	    point_num = find_point_with_mouse(Gpoints.x, Gpoints.y,Gpoints.n_points,
		thresh) ;
	    if (point_num < 0)
	    {
		ret = 0;
		display_line( map->Line[line].type, &Gpoints, line, map);
		goto done;
	    }
	}

    /*  find where to place the point   */

	if (is_site)
	    new_point_with_mouse ( &ux1, &uy1,
		"  Place mouse on new position for this site:");
	else
	    new_point_with_mouse ( &ux1, &uy1,
		"  Place mouse on new position for this point on line:");
	if ( ! ux1 && !uy1)
	{
	    ret = 0;
	    display_line( map->Line[line].type, &Gpoints, line, map);
	    goto done;
	}
/*-->  added by SCS, RL Glenn, 9/20/91
       remove the mouse selected point highlight */
	R_standard_color (dcolors[CLR_ERASE]);
	Blot (&ux1, &uy1);

	dx = ux1 - Gpoints.x[point_num];
	dy = uy1 - Gpoints.y[point_num];

	R_standard_color (dcolors[CLR_ERASE]);
	Blot (&Gpoints.x[point_num], &Gpoints.y[point_num]);
	erase_line( map->Line[line].type, &Gpoints, line, map);
	{
	    if (map->Line[line].att)
		att = map->Att[map->Line[line].att].cat;
	    else
		att = 0;

	    type = map->Line[line].type;
	/* Gpoints loaded here */
	    _remove_line (map, line);

	    for ( i = 0 ; i < Gpoints.n_points ; i++)
	    {
		Gpoints.x[i] += dx;
		Gpoints.y[i] += dy;
	    }

	    dig_check_nodes (map, &node, &Gpoints);

	    line = new_line (map, type, &node, &Gpoints);
	    if (line < 0)
	    {
                sprintf (buf,"Error creating new line .");
                message[0] = (char *) malloc (strlen (buf) + 1);
                sprintf(message[0],"%s", buf);
                message[1] = " ";
                message[2] = '\0';

                Dchoose(MEN.name) ;
                popup_messg( "warning", 1) ;
	        sleep(2);
                erase_popup("warning");
                Dchoose(DIG.name) ;
		return (-1);
	    }

	    if (att)
	    {
		if (is_site)
		    map->Line[line].att = dig_new_att (map, Gpoints.x[0], Gpoints.y[0], type, line, att);
		else
		{
		    double x, y;

		    get_line_center (&x, &y, &Gpoints);
		    map->Line[line].att = dig_new_att (map, x, y, type, line, att);
		}
	    }
	}

	Changes_Made = 1;
	display_line( map->Line[line].type, &Gpoints, line, map);

    /*  Rewind, write out altered-code  */
	Vect__Rewrite_line (map, map->Line[line].offset, map->Line[line].type, &Gpoints);

    }

done:
    G_clear_screen() ;
    Disp_points = Save_Disp;
    return (ret);
}
