/* plotCat - uses level 2 access to vector data for reads 
   this function plots dig line segments 
   which are common to both the RDBMS and the dig
   structure.
*/
#include "gis.h"
#include "Vect.h"
#include "display.h"

extern double D_get_d_north();
extern double D_get_d_south();
extern double D_get_d_west();
extern double D_get_d_east();
extern double D_d_to_u_row(), D_d_to_u_col() ;

extern int D_move_abs();
extern int D_cont_abs();



plotCat (name, mapset, Points, vect_cat, P_map, fillcolr)
    char *name, *mapset;
    struct line_pnts *Points;
    int vect_cat,
	fillcolr;
    struct Map_info *P_map;
{
    double *x, *y;
    int *list, count, idx, i;
    int ret, n,np, a_index;
    double N,S,E,W;
    struct Cell_head window;
    int *find_area(), *find_line();
    int x_screen[4096], y_screen[4096];
    

    fflush (stdout);

    G_get_set_window (&window);

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);


  /* list is the list of line cat numbers (ie. dig_att vals) */
  


   if (list = find_area (vect_cat, &count, P_map))
   {
	for (n = 0; n < count; n++)
	{
		idx = list[n];
        	if (V2_get_area_bbox (P_map, idx, &N, &S, &E, &W) < 0)
		{
	    		fprintf (stderr, "\nWARNING: vector file [%s]-read error\n",name) ;
			return -1;
		}
						 
        	if (!G_window_overlap (&window, N, S, E, W))
		    	continue;

               a_index = P_map->Att[P_map->Area[idx].att].index;
               Vect_get_area_points(P_map,a_index,Points);
               np = Points->n_points;
	       
		for(i=0; i < np; i++)
		{
		    	x_screen[i] = (int) (D_u_to_d_col( (*(Points->x+i))));
		    	y_screen[i] = (int) (D_u_to_d_row( (*(Points->y+i))));
		}
		
		R_polyline_abs(x_screen,y_screen,Points->n_points);
		
		if (fillcolr) {
			R_polygon_abs(x_screen,y_screen,Points->n_points);

		}
			
		
		
          }
/*
--------------
If we don't return now, we may draw lines which are not ours: A.Sh.
*/	  
	return 0;
	
      } /* end if nareas > 0  */

if (list = find_line (vect_cat, &count, P_map))
  {
	for (n = 0; n < count; n++)
	{
		idx = list[n];
		if (V2_get_line_bbox (P_map, idx, &N, &S, &E, &W) < 0)
		{
		    fprintf (stderr, "\nWARNING: vector file [%s]-read error\n",name) ;
			return -1;
		}
						 
		if (!G_window_overlap (&window, N, S, E, W))
		    continue;

		if (0 > (ret = V2_read_line (P_map, Points, idx)))
		{
			if (ret == -2) {
				G_warning ("Read error - EOF\n");
				return -1;
			}
			else {
				G_warning ("Read error\n");
				return -1;
			}
		}


	   	np = Points->n_points;
	   	x  = Points->x;
	   	y =  Points->y;
	   	for (i=1; i < np; i++)
	     	{
	       		G_plot_line (x[0], y[0], x[1], y[1]);
	       		x++;
	       		y++;
	     	}
	   }
    }    	             /* end for lines  */

    return 0;
}
