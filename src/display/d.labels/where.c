#include "where.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

int where (char *easting, char *northing)
{
    int nrows, ncols;
    CELL *buf;
    struct Cell_head window;
    int screen_x, screen_y ;
    double east, north ;
    int button ;

    G_get_set_window (&window);
    nrows = window.rows;
    ncols = window.cols;
    buf = G_allocate_cell_buf();

    screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
    screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

    while (1)
    {
	R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	east  = D_d_to_u_col((double)screen_x) ;
	north = D_d_to_u_row((double)screen_y) ;
	if (button == 1)
           {
           sprintf(easting,"%.2f",east);
           sprintf(northing,"%.2f",north);
           return(0);                      /* return with coordinates */ 
           }
	if (button == 2)
           return(1);                      /* user wants to quit */ 
        if (button == 3)
           show_utm(north,east);           /* show coordinates */
    }
}

int get_button (int option)
{
    int nrows, ncols;
    CELL *buf;
    struct Cell_head window;
    int screen_x, screen_y ;
    int button ;

    G_get_set_window (&window);
    nrows = window.rows;
    ncols = window.cols;
    buf = G_allocate_cell_buf();

    screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
    screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

    while (1)
      {
      if (option == 1) show_menu1();
      else show_menu2();
      R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
      return(button);
      }

}
