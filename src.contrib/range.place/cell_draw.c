#include "gis.h"

cell_draw(name, mapset)
        char *name ;
        char *mapset ;
{
        char buff[128] ;
        int cellfile ;
        CELL *xarray ;
        int cur_A_row ;
	int t,b,l,r;

	D_get_screen_window(&t,&b,&l,&r);
/* Set up the screen, conversions, and graphics */
        if (D_cell_draw_setup(t,b,l,r))
        {
                sprintf(buff,"Cannot use current window") ;
                G_fatal_error(buff) ;
        }

/* Make sure map is available */
        if ((cellfile = G_open_cell_old(name, mapset)) == -1)
        {
                sprintf(buff,"Not able to open cellfile for [%s]", name);
                G_fatal_error(buff) ;
        }
 
/* Allocate space for cell buffer */
        xarray = G_allocate_cell_buf() ;
 
/* loop for array rows */
        for (cur_A_row = 0; cur_A_row != -1; )
        {
        /* Get window (array) row currently required */
                G_get_map_row(cellfile, xarray, cur_A_row) ;
         
        /* Draw the cell row, and get the next row number */
                cur_A_row = D_draw_cell_row(cur_A_row, xarray) ;
        }
 
/* Wrap up and return */
        G_close_cell(cellfile) ;
        return(0) ;
}

