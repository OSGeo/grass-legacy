#include <stdlib.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "mask.h"
#include "local_proto.h"

static int cell_draw(char *,char *,struct Colors *,int,int,RASTER_MAP_TYPE);

int display(
    char *name ,
    char *mapset ,
    int overlay ,
    char *bg,
    RASTER_MAP_TYPE data_type, int invert)
{
    struct Colors colors ;
    int offset ;
    int r,g,b;

    if (G_read_colors(name, mapset, &colors) == -1)
    {
	fprintf(stderr,"Color file for [%s] not available", name) ;
	exit(1);
    }
    /***DEBUG ***
    if (G_write_colors(name, mapset, &colors) == -1)
    {
	fprintf(stderr,"can't write colors ");
	exit(1);
    }
    if (G_read_colors(name, mapset, &colors) == -1)
    {
	fprintf(stderr,"Color file for [%s] not available", name) ;
	exit(1);
    }
    *********/

    if (bg)
    {
	get_rgb(bg, &r, &g, &b);
	G_set_null_value_color (r, g, b, &colors);
    }

    D_setup(0);

    /* cell maps wipe out a picture, so we clear info on the window too */
    if (!overlay)
	D_clear_window();

    /* Get color offset value for current window and pass to driver */
    D_offset_is(&offset) ;
    R_color_offset(offset) ;

    /* Set the colors for the display */
    D_set_colors (&colors);

    /* Go draw the cell file */
    cell_draw(name, mapset, &colors, overlay, invert, data_type) ;

    /* release the colors now */
    G_free_colors (&colors);

    /* record the cell file */
    D_set_cell_name(G_fully_qualified_name(name, mapset));
    D_add_to_cell_list(G_fully_qualified_name(name, mapset));

    D_add_to_list(G_recreate_command());

    /* If overlay add it to the list instead of setting the cell name */
/*
    if (overlay) {
	sprintf(buf,"d.rast -o map=%s", G_fully_qualified_name(name,mapset));
	D_add_to_list(buf) ;
    }
*/

    return 0;
}

static int cell_draw(
    char *name ,
    char *mapset ,
    struct Colors *colors,
    int overlay ,
    int invert,
    RASTER_MAP_TYPE data_type)
{
    char buff[128] ;
    int cellfile;
    void *xarray ;
    int cur_A_row ;
    int t, b, l, r ;
    int ncols;

    ncols = G_window_cols();

    /* Set up the screen, conversions, and graphics */
    D_get_screen_window(&t, &b, &l, &r) ;
    if (D_cell_draw_setup(t, b, l, r))
    {
	sprintf(buff,"Cannot use current window") ;
	G_fatal_error(buff) ;
    }
    D_set_overlay_mode(overlay);

    /* Make sure map is available */
    if ((cellfile = G_open_cell_old(name, mapset)) == -1)
    {
	sprintf(buff,"Not able to open cellfile for [%s]", name);
	G_fatal_error(buff) ;
    }

    /* Allocate space for cell buffer */
    xarray = G_allocate_raster_buf(data_type) ;

    /* loop for array rows */
    for (cur_A_row = 0; cur_A_row != -1; )
    {
	/* Get window (array) row currently required */
	G_get_raster_row(cellfile, xarray, cur_A_row, data_type) ;
	mask_raster_array (xarray, ncols, invert, data_type);

	/* Draw the cell row, and get the next row number */
	cur_A_row = D_draw_raster(cur_A_row, xarray, colors, data_type) ;

    }
    R_flush() ;

    /* Wrap up and return */
    G_close_cell(cellfile) ;
    free (xarray);
    return(0) ;
}

int mask_raster_array (void *xarray,
   int ncols, int invert, RASTER_MAP_TYPE data_type)
{
   if(data_type == CELL_TYPE)
       mask_cell_array ((CELL *) xarray, ncols, &mask, invert);
   else if(data_type == DCELL_TYPE)
       mask_d_cell_array ((DCELL *) xarray, ncols, &d_mask, invert);

    return 0;
}
