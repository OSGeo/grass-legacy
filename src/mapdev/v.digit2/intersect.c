/*
**-->  Written by R.L.Glenn, SCS, 1/1990
**  
*/
#include "digit.h"
#include <math.h>

#define	YES    1
#define	NO    2
#define	QUIT    3

#ifdef SCS_MODS

double hypot ();
double fabs();
long ftell();

intersect_line (map)
    struct Map_info *map;
{
    int line_from, line_to;
    int line;
    double ux, uy;
    double thresh;
    double ux1, uy1;
    double ux2, uy2;
    double hyp1, hyp2;
    int end_point;
    int area;
    int node_num;
    char message[67];

    screen_to_utm (0, 0, &ux1, &uy1);
    screen_to_utm (5, 0, &ux2, &uy2);
    thresh = fabs ( ux1 - ux2);

    while(1)
      {
#ifdef BREAKCNT
      if (++breakcnt > 4)
         {
         _Clear_info ();
         if ( 0 > write_out (1))
           {
           BEEP;
           Write_info (2, "Write FAILED!!!");
           sleep (4);
           return(-1);
           }
         else
           {
           Changes_Made = 0;
           Write_info (2, "File updated...");
           sleep (2);
           breakcnt = 0;
           }
         }
#endif /* BREAKCNT */
      Clear_info ();
    /* find_line_with_mouse  fills Gpoints */
      line_to = find_line_with_mouse (LINE|AREA, "Line to snap to:", NULL);
      line = line_to;
      if (line_to <= 0)
/*	  return (0);*/
          break;

      get_point (&ux, &uy, "Select snap intersection:");
      if ( ux == 0.0 && uy == 0.0)
        {
	/* reset the highlit line */
	display_line (map->Line[line].type, &Gpoints, line, map);
/*	return (0);*/
        break;
        }

      break_line_w_point (map, line, ux, uy);
    /* let break_line redraw the new lines */

      if ( (node_num = dig_which_node (CM, &ux, &uy, thresh))  < 0)
         Write_info (2, " no node found ");
      else
         {
         sprintf (message, " node#: %d", node_num);
         Write_info( 2, message);
         line_from = find_line_with_mouse (LINE|AREA, " Line to be snapped:", NULL);
         line = line_from;

         if (line_from <= 0)
/*           return (0);*/
             break;

         Clear_info();
         erase_line( map->Line[line].type, &Gpoints, line, map);

         end_point = Gpoints.n_points-1;
	    /* snap to closest node on 'line_from' */

	    {
		register double X, Y;

		X = Gpoints.x[0] - map->Node[node_num].x; 
		Y = Gpoints.y[0] - map->Node[node_num].y;
		if (X == 0.0 && Y == 0.0)
		    hyp1 = 0.0;
		else
		    hyp1 = hypot (X, Y);
		/*
		if (near_zero (hyp1))
		    hyp1 = 0.0;
		*/

		X = Gpoints.x[end_point] - map->Node[node_num].x; 
		Y = Gpoints.y[end_point] - map->Node[node_num].y;
		if (X == 0.0 && Y == 0.0)
		    hyp2 = 0.0;
		else
		    hyp2 = hypot (X, Y);
		/*
		if (near_zero (hyp2))
		    hyp2 = 0.0;
		*/
	    }


    /* choose which end point to snap from:  
    **       Do not want to snap to itself so check hyp for 0.0 
    **	 otherwise  endpoint closest to node gets snapped
    */
	    if (hyp1 == 0.0)
		line_from = -line_from;
	    else
		if (hyp2 == 0.0)
		    ;  /* leave positive */
		else
		    if (hyp1 > hyp2)
			line_from = -line_from;
		    else
			;

	    /* delete areas bounded by line  because areas have changed */
	    /* ISLE
	    **   Del_area will call Del_isle automatically
	    */
	    if (map->Line[line].type == AREA)
	    {
		if (map->Line[line].right)
		    Del_area (map, map->Line[line].right);
		if (map->Line[line].left)
		    Del_area (map, map->Line[line].left);
	    }

    /*DEBUG*/ debugf ("Calling Snap_line_to_node (line %d, node, %d)\n", line_from, node_num);
	    dig_snap_line_to_node (map, line_from, node_num, &Gpoints);

	    /* check for possibly affecting existing areas at the snapped node */
	    if (map->Line[line].type == AREA)
	    {
		if (area = check_next (map, line_from, RIGHT))
		    Del_area (map, area);
		if (area = check_next (map, line_from, LEFT))
		    Del_area (map, area);
	    }

	    display_line (map->Line[line].type, &Gpoints, line, map);

	    Vect__Rewrite_line (map, map->Line[line].offset, map->Line[line].type, &Gpoints);

         }

      Clear_info();
      } /* end of while */

    Changes_Made = 1;

    return (0);
}

#else

static int xx;

#endif /* SCS_MODS */
