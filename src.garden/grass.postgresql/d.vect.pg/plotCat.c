/* plotCat - uses level 2 access to vector data for reads 
   this function plots dig line segments 
   which are common to both the RDBMS and the dig
   structure.
*/
/* using G_plot_area instead of R_poly and handle isles 03/2002 --alex
*/
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "raster.h"
#include "glocale.h"

extern double D_get_d_north();
extern double D_get_d_south();
extern double D_get_d_west();
extern double D_get_d_east();
extern double D_d_to_u_row(), D_d_to_u_col() ;

extern int D_move_abs();
extern int D_cont_abs();


int plotCat (name, mapset, points, vect_cat, Map, fillcolr)
    char *name, *mapset;
    struct line_pnts *points;
    int vect_cat,
	fillcolr;
    struct Map_info *Map;
{
    double *x, *y;
    int *list, count, idx, i, j, jk;
    int ret, n,np, a_index, k_index;
    double N,S,E,W;
    struct Cell_head window;
    int *find_area(), *find_line();
    P_AREA *pa;
    double **xs, **ys;
    int rings;
    int *rpnts;
    struct line_pnts *points_i;

    fflush (stdout);

    G_get_set_window (&window);

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);


  /* list is the list of line cat numbers (ie. dig_att vals) */
  


   if ((list = find_area (vect_cat, &count, Map)))

   {
	
	for (i = 0; i < count; i++) {
            
	    idx = list[i];

            V2_get_area_bbox (Map, idx, &N, &S, &E, &W);
            if (S > window.north || N < window.south ||
                    W > window.east  || E < window.west)
                continue;
	
	    k_index = Map->Area[idx].att;
	    if (!k_index) continue;
	    a_index = Map->Att[k_index].index;
		
            V2_get_area (Map, a_index, &pa);

            rings = 1 + pa->n_isles;
            xs = (double **) G_malloc (sizeof(double *) * rings);
            ys = (double **) G_malloc (sizeof(double *) * rings);
            rpnts = (int *) G_malloc (sizeof (int) * rings);	    
	    
            Vect_get_area_points (Map, a_index, points);
            rpnts[0] = points->n_points;
            xs[0] = (double *) G_malloc (sizeof(double) * rpnts[0]);
            ys[0] = (double *) G_malloc (sizeof(double) * rpnts[0]);
            Vect_copy_pnts_to_xy (points, xs[0], ys[0], &rpnts[0]);
            
	    points_i = Vect_new_line_struct();
	    
	    for (j = 0; j < pa->n_isles; j++) {
                Vect_get_isle_points (Map, pa->isles[j], points_i);
                rpnts[j+1] = points_i->n_points;
                xs[j+1] = (double *) G_malloc (sizeof(double) * rpnts[j+1]);
                ys[j+1] = (double *) G_malloc (sizeof(double) * rpnts[j+1]);
                Vect_copy_pnts_to_xy (points_i, xs[j+1], ys[j+1], &rpnts[j+1]);
		
			   
           	for (jk = 0; jk < points_i->n_points - 1; jk++)
                	G_plot_line (points_i->x[jk],   points_i->y[jk],
                        	points_i->x[jk+1], points_i->y[jk+1]);

            }

           if (fillcolr) G_plot_area (xs, ys, rpnts, rings);
			
	   for (j = 0; j < points->n_points - 1; j++)
                G_plot_line (points->x[j],   points->y[j],
                        points->x[j+1], points->y[j+1]);
	    
	    Vect_destroy_line_struct (points_i);
	    
            for (j = 0; j < rings; j++)
            {
                free (xs[j]);
                free (ys[j]);
            }
            free (xs);
            free (ys);
            free (rpnts);
        }

        Vect_rewind (Map);
	
	  
/*
--------------
If we don't return now, we may draw lines which are not ours: A.Sh.
*/	  
	return 0;
	
      } /* end if find_area > 0  */

if ((list = find_line (vect_cat, &count, Map)))
  {
	for (n = 0; n < count; n++)
	{
		idx = list[n];
		if (V2_get_line_bbox (Map, idx, &N, &S, &E, &W) < 0)
		{
		    fprintf (stderr, _("\nWARNING: vector file [%s]-read error\n"),name) ;
			return -1;
		}
						 
		if (!G_window_overlap (&window, N, S, E, W))
		    continue;

		if (0 > (ret = V2_read_line (Map, points, idx)))
		{
			if (ret == -2) {
				G_warning (_("Read error - EOF\n"));
				return -1;
			}
			else {
				G_warning (_("Read error\n"));
				return -1;
			}
		}


	   	np = points->n_points;
	   	x  = points->x;
	   	y =  points->y;
	   	for (i=1; i < np; i++)
	     	{
	       		G_plot_line (x[0], y[0], x[1], y[1]);
	       		x++;
	       		y++;
	     	}
	   }
    } 	             /* end for lines  */

    return 0;
}
