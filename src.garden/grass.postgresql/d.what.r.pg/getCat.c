#include "what.h"

getCat (mapname)
  char *mapname;

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

    G_get_set_window (&window);
    nrows = window.rows;
    ncols = window.cols;
    buf = G_allocate_cell_buf();
    
    dbCat = -1;

    screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
    screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

        fprintf(stderr, "\n\nButtons:\n") ;
        fprintf(stderr, "Left:  Choose location.\n") ;
        fprintf(stderr, "Right: Finish. \n\n\n") ;


        R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
        east  = D_d_to_u_col((double)screen_x) ;
        north = D_d_to_u_row((double)screen_y) ;
        row = (window.north - north) / window.ns_res ;
        col = (east - window.west) / window.ew_res ;

        if ( (row < 0 || row >= nrows) || (col < 0 || col >= ncols)) return(1);

        north = window.north - (row+.5) * window.ns_res ;
        east  = window.west  + (col+.5) * window.ew_res ;

        if (G_get_map_row (fd, buf, row) < 0)
	   fprintf(stderr, "Error read cell file %s.",mapname);


	if (button != 3)
		dbCat = buf[col];

return(button);
}
