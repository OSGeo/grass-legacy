/* using G_plot_area instead of R_poly and handle isles 03/2002 --alex*/

#include <stdlib.h>
#include "what.h"
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "Vect.h"
#include "glocale.h"

int getCat (Map, Cats,colr,fillcolr,fill)

  struct Map_info *Map;
  struct Categories *Cats;
  int colr,fillcolr, fill;

{
    int i;
    int row, col;
    int nrows, ncols;
    struct Cell_head window;
    int screen_x, screen_y ;
    double east, north ;
    int button ;
    double D_get_d_north(), D_get_d_south() ;
    double D_get_d_east(), D_get_d_west() ;
    double D_d_to_u_row(), D_d_to_u_col() ;

    plus_t line, area;
    
    struct line_pnts *Points, *Points_i;
    int np,ret;
    double *x,*y;
    
    P_AREA *pa;
    double **xs, **ys;
    int rings;
    int *rpnts;
    int j;    
    	

	dbCat = -1;

	G_get_set_window (&window);
	nrows = window.rows;
	ncols = window.cols;
	
	G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);

	screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
	screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

        fprintf(stderr, _("\n\nButtons:\n")) ;
        fprintf(stderr, _("Left:   Choose object for query.\n")) ;
	fprintf(stderr, _("Middle: Toggle object color.\n")) ;
        fprintf(stderr, _("Right:  Finish. \n\n\n")) ;


        R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
        east  = D_d_to_u_col((double)screen_x) ;
        north = D_d_to_u_row((double)screen_y) ;
        row = (window.north - north) / window.ns_res ;
        col = (east - window.west) / window.ew_res ;
	
	if (button == 2) {
		fillcolr++;
		colr++;
	}
	
        if ( (row < 0 || row >= nrows) || (col < 0 || col >= ncols)) return(1);


	line = dig_point_to_line (Map, east, north, -1);
        area = dig_point_to_area (Map, east, north) ;


        if  ((line + area == 0) && (button != 3) )  {
		fprintf(stderr,_("Nothing found.\n")) ;
		dbCat=-1;
		return(button);
		
	}

        if ( (line > 0 ) && (area == 0) && (button != 3)) { 
	
	  if((dbCat = V2_line_att(Map,line))){		
	 	Points = Vect_new_line_struct();	
		if (0 > (ret = V2_read_line (Map, Points, line)))
		{
	    		if (ret == -2)
				G_warning (_("Read error\n"));
	    		return(-1);
		}
		np = Points->n_points;
	   	x  = Points->x;
	  	y =  Points->y;
		if (colr > 0) {
			R_standard_color(colr);
	   		for (i=1; i < np; i++){

	       			G_plot_line (x[0], y[0], x[1], y[1]);
	       			x++;
	       			y++;
	     		}
		}
		Vect_destroy_line_struct (Points);
	  }else 
	  fprintf(stderr,_("Line category not found\nArea not found\n"));	
	} 
	else if ( (area > 0) && (button != 3) ) {
       
	 if((dbCat= V2_area_att(Map, area))) {
		
		V2_get_area (Map, area, &pa);
		
		rings = 1 + pa->n_isles;
            	xs = (double **) G_malloc (sizeof(double *) * rings);
            	ys = (double **) G_malloc (sizeof(double *) * rings);
            	rpnts = (int *) G_malloc (sizeof (int) * rings);

		
		Points = Vect_new_line_struct();
		Points_i = Vect_new_line_struct();
		Vect_get_area_points(Map, area, Points);
		
		rpnts[0] = Points->n_points;
            	xs[0] = (double *) G_malloc (sizeof(double) * rpnts[0]);
            	ys[0] = (double *) G_malloc (sizeof(double) * rpnts[0]);
            	Vect_copy_pnts_to_xy (Points, xs[0], ys[0], &rpnts[0]);
            	for (j = 0; j < pa->n_isles; j++) {
                	Vect_get_isle_points (Map, pa->isles[j], Points_i);
                	rpnts[j+1] = Points_i->n_points;
                	xs[j+1] = (double *) G_malloc (sizeof(double) * rpnts[j+1]);
                	ys[j+1] = (double *) G_malloc (sizeof(double) * rpnts[j+1]);
                	Vect_copy_pnts_to_xy (Points_i, xs[j+1], ys[j+1], &rpnts[j+1]);
            	}
		
		if (fill) {
			R_standard_color(fillcolr);
			G_plot_area (xs, ys, rpnts, rings);
		}
		
		if (colr > 0) {
			
			R_standard_color(colr);
			
			for (j = 0; j < Points->n_points - 1; j++)
                		G_plot_line (Points->x[j],   Points->y[j],
                        	Points->x[j+1], Points->y[j+1]);
				
			for (j = 0; j < Points_i->n_points - 1; j++)
                		G_plot_line (Points_i->x[j],   Points_i->y[j],
                        	Points_i->x[j+1], Points_i->y[j+1]);
		}
		
		for (j = 0; j < rings; j++)
            	{	
                	free (xs[j]);
                	free (ys[j]);
            	}
            	
		free (xs);
            	free (ys);
            	free (rpnts);

		Vect_destroy_line_struct (Points);
		Vect_destroy_line_struct (Points_i);
	  }else{
	  fprintf(stderr,_("Area category not found\n"));
	  	
		if ( (line > 0) && (button != 3) ) {
			if ((dbCat = V2_line_att(Map,line))){		
	 			Points = Vect_new_line_struct();	
				if (0 > (ret = V2_read_line (Map, Points, line)))
				{
	    				if (ret == -2)
						G_warning (_("Read error\n"));
	    				return(-1);
				}
				np = Points->n_points;
	   			x  = Points->x;
	  			y =  Points->y;
				if (colr > 0) {
					R_standard_color(colr);
	   				for (i=1; i < np; i++){

	       				G_plot_line (x[0], y[0], x[1], y[1]);
	       				x++;
	       				y++;
	     				}
				}
				Vect_destroy_line_struct (Points);
	  		}else 
	  		fprintf(stderr,_("Line category not found\n"));	
		} 
	  
	  }
	}
R_flush(); 
return(button);
}
