#include "what.h"
#include "gis.h"
#include "display.h"
#include "Vect.h"

getCat (Map, Cats,colr,fillcolr,fill)

  struct Map_info *Map;
  struct Categories *Cats;
  int colr,fillcolr, fill;

{
    int width, mwidth;
    int i;
    int row, col;
    int nrows, ncols;
    CELL *buf;
    struct Cell_head window;
    int screen_x, screen_y ;
    double east, north ;
    int button ;
    double D_get_d_north(), D_get_d_south() ;
    double D_get_d_east(), D_get_d_west() ;
    double D_d_to_u_row(), D_d_to_u_col() ;

    P_LINE *Line;
    P_AREA *Area;
    plus_t line, area;
    
    struct line_pnts *Points;
    int np,ret;
    double *x,*y;
    
    int a_line;
    int nlines;
    int x_screen[4096], y_screen[4096];

    
    double N, S, E, W;
    
    	

	dbCat = -1;

	G_get_set_window (&window);
	nrows = window.rows;
	ncols = window.cols;
	
	G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);

	screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
	screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

        fprintf(stderr, "\n\nButtons:\n") ;
        fprintf(stderr, "Left:   Choose object for query.\n") ;
	fprintf(stderr, "Middle: Toggle object color.\n") ;
        fprintf(stderr, "Right:  Finish. \n\n\n") ;


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
		fprintf(stderr,"Nothing found.\n") ;
		dbCat=-1;
		return(button);
		
	}

        if ( (line > 0 ) && (area == 0) && (button != 3)) { 
	
	  if(dbCat = V2_line_att(Map,line)){		
	 	Points = Vect_new_line_struct();	
		if (0 > (ret = V2_read_line (Map, Points, line)))
		{
	    		if (ret == -2)
				G_warning ("Read error\n");
	    		return;
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
	  fprintf(stderr,"Line category not found\nArea not found\n");	
	} 
	else if ( (area > 0) && (button != 3) ) {
       
	 if(dbCat= V2_area_att(Map, area)) {
	
		Points = Vect_new_line_struct();
		Vect_get_area_points(Map, area, Points);		

		for(i=0; i < Points->n_points; i++)
		{
		    x_screen[i] = (int) (D_u_to_d_col( (*(Points->x+i))));
		    y_screen[i] = (int) (D_u_to_d_row( (*(Points->y+i))));
		}
	   if (fill) {
		R_standard_color(fillcolr);
		R_polygon_abs(x_screen,y_screen,Points->n_points);
	   }
		if (colr > 0)
		{
			R_standard_color(colr);
			R_polydots_abs(x_screen,y_screen,Points->n_points);
		}
		Vect_destroy_line_struct (Points);
	  }else{
	  fprintf(stderr,"Area category not found\n");
	  	
		if ( (line > 0) && (button != 3) ) {
			if(dbCat = V2_line_att(Map,line)){		
	 			Points = Vect_new_line_struct();	
				if (0 > (ret = V2_read_line (Map, Points, line)))
				{
	    				if (ret == -2)
						G_warning ("Read error\n");
	    				return;
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
	  		fprintf(stderr,"Line category not found\n");	
		} 
	  
	  }
	} 
return(button);
}
