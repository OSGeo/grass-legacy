#include "what.h"
#include "digit.h"

getCat (Map, Cats)

  struct Map_info *Map;
  struct Categories *Cats;

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

	dbCat = -1;

	G_get_set_window (&window);
	nrows = window.rows;
	ncols = window.cols;

	screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
	screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

        fprintf(stderr, "\n\nButtons:\n") ;
        fprintf(stderr, "Left:  Select query location.\n") ;
        fprintf(stderr, "Right: Quit this. \n\n\n") ;


        R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
        east  = D_d_to_u_col((double)screen_x) ;
        north = D_d_to_u_row((double)screen_y) ;
        row = (window.north - north) / window.ns_res ;
        col = (east - window.west) / window.ew_res ;

        if ( (row < 0 || row >= nrows) || (col < 0 || col >= ncols)) return(1);


	line = dig_point_to_line (Map, east, north, -1);
        area = dig_point_to_area (Map, east, north) ;


        if  ((line + area == 0) && (button != 3) )  {
		printf("Nothing Found.\n") ;
		return(button);
	}

        if ( (line > 0) && (button != 3))  
		dbCat = V2_line_att(Map,line);
        if ( (area > 0) && (button != 3) )  
		dbCat= V2_area_att(Map, area);
return(button);
}
