#include "digit.h"
#include "gis.h"
#include "popup.h"

drawcell(window)
    struct Cell_head *window;
{
    int fd;
    int left, top;
    int ncols, nrows;
    int row;
    CELL *cell;
    struct Colors colr;
    struct Cell_head cellhd;
    int repeat, ret=0;
    char buf[100];


    sprintf(buf," Displaying Raster data."); 
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    sprintf(buf,"...Press < ESC > key to stop redraw .");
    message[1] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[1],"%s", buf);
    message[2] = '\0';

    Dchoose(MEN.name) ;
    popup_messg( "disp_rast", 1) ;

    G_get_set_window (&cellhd);  /* read window information from window_rout () */

    if(G_read_colors (N_backdrop, N_backdrop_mapset, &colr) < 0)
        {
	erase_popup("disp_rast");
	return 0;
	}

    D_set_colors (&colr);

    nrows = G_window_rows();
    ncols = G_window_cols();

    top =   1;  /* make it sit inside the outline box in digit */
    left = 1;

    R_standard_color (WHITE);

    fd = G_open_cell_old (N_backdrop, N_backdrop_mapset);
    if (fd < 0)
        {
	erase_popup("disp_rast");
	return 0;
	}
    cell = G_allocate_cell_buf();

    set_keyboard ();		/* setup for key_hit () */
    
    for (row = 0; row < nrows; row += repeat)
    {
	if (key_hit (buf))
	{
	   if (*buf == ESC)
	   {
	   /* ret = -1; */
	      break;
	   }
        }

	R_move_abs (left, top+row);
	if(G_get_map_row_nomask(fd, cell, row) < 0)
	    break;
	repeat = G_row_repeat_nomask (fd, row);
	D_raster (cell, ncols, repeat, &colr);
    }
    unset_keyboard (); 
    R_flush ();
    G_close_cell (fd);
    G_free_colors (&colr);
    free (cell);
    erase_popup("disp_rast");

    return ret;
}
/*
catch(sig)
int sig;
{
    signal(SIGINT, catch);  **  reset interrupt  **
    Interrupt = 1;
}
*/
