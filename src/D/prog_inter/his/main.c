/* %W%   %G% */
#include <stdio.h>
#include "gis.h"

main(argc, argv) char **argv ;
{
    CELL *hue_array ;
    CELL *int_array ;
    CELL *out_array ;
    CELL *sat_array ;
    char *location ;
    char *mapset ;
    char *name[20] ;
    char buff[128] ;
    char error ;
    int intensity ;
    int saturation ;
    int atrow, atcol ;
    int next_row;
    int hue_file ;
    int int_file ;
    int int_used ;
    int new_percent ;
    int percent ;
    int sat_file ;
    int sat_used ;
    int out_file ;
    int out_used ;
    struct Cell_head window ;
    struct Colors hue_colors ;
    struct Colors int_colors ;
    struct Colors out_colors ;
    struct Colors sat_colors ;

    G_gisinit(argv[0]) ;

/* Print advertising */
    G_clear_screen() ;
    printf("\n\n  HIS (Hue/Intensity/Saturation) PICTURE GENERATION UTILITY\n");

/* read in current window */
    G_get_window(&window) ;

/* Do screen initializing stuff */
    if (init_his())
	exit(1) ;

/* Get name of layer to be used for hue */
    mapset = G_ask_cell_old ("Enter name of layer to be used for HUE.",
		name) ;
    if (mapset == NULL)
	exit(0) ;

/* Make sure map is available */
    if ((hue_file = G_open_cell_old(name, mapset)) == -1) 
    {
	sprintf(buff,"Not able to open cellfile for [%s]", name) ;
	G_fatal_error(buff) ;
    }

    hue_array = G_allocate_cell_buf () ;

/* Reading color lookup table */
    if (G_read_colors(name, mapset, &hue_colors) == -1)
    {
	sprintf(buff,"Color file for [%s] not available", name) ;
	G_fatal_error(buff) ;
    }

/* Get name of layer to be used for intensity */
    G_set_ask_return_msg ("if you don't want to apply INTENSITY");
    mapset = G_ask_cell_old("Enter name of layer to be used for INTENSITY.",
		name) ;
    if (mapset == NULL)
    {
	int_used = 0 ;
    }
    else
    {
	int_used = 1 ;

/* Make sure map is available */
	if ((int_file = G_open_cell_old(name, mapset)) == -1) 
	{
	    sprintf(buff,"Not able to open cellfile for [%s]", name) ;
	    G_fatal_error(buff) ;
	}

	int_array = G_allocate_cell_buf () ;

/* Reading color lookup table */
	if (G_read_colors(name, mapset, &int_colors) == -1)
	{
	    sprintf(buff,"Color file for [%s] not available", name) ;
	    G_fatal_error(buff) ;
	}
    }

/* Get name of layer to be used for saturation */
    G_set_ask_return_msg ("if you don't want to apply SATURATION");
    mapset = G_ask_cell_old("Enter name of layer to be used for SATURATION.",
	    name) ;
    if (mapset == NULL)
    {
	sat_used = 0 ;
    }
    else
    {
	sat_used = 1 ;

/* Make sure map is available */
	if ((sat_file = G_open_cell_old(name, mapset)) == -1) 
	{
	    sprintf(buff,"Not able to open cellfile for [%s]", name) ;
	    G_fatal_error(buff) ;
	}

	sat_array = G_allocate_cell_buf () ;

/* Reading color lookup table */
	if (G_read_colors(name, mapset, &sat_colors) == -1)
	{
	    sprintf(buff,"Color file for [%s] not available", name) ;
	    G_fatal_error(buff) ;
	}
    }

    G_set_ask_return_msg ("if you only want to display the results");
    mapset = G_ask_cell_new("Enter name of layer to contain results",
	    name) ;
    if (mapset == NULL)
    {
	out_used = 0;
    }
    else if ((out_file = G_open_cell_new (name)) < 0)
    {
	out_used = 0;
    }
    else
	out_used = 1;

    out_array = G_allocate_cell_buf () ;

/* Make color table */
    G_make_HIS_color(&out_colors) ;
    D_reset_colors (&out_colors);

/* Now do the work */
    intensity =  127 ;  /* default is to not change intensity */
    saturation = 255 ;  /* default is to not change saturation */

	alloc_pass_buff(window.cols) ;

    next_row = 0;
    for (atrow=0; atrow<window.rows; )
    {
	G_percent (atrow, window.rows, 5);
	if(G_get_map_row(hue_file, hue_array, atrow) < 0)
	    exit(1);
	if (int_used && (G_get_map_row(int_file, int_array, atrow) < 0))
	    exit(1); 
	if (sat_used && (G_get_map_row(sat_file, sat_array, atrow) < 0))
	    exit(1);
	
	for (atcol=0; atcol<window.cols; atcol++)
	{
	    int r, g, b ;

	    if (int_used) 
		G_get_color(int_array[atcol], &intensity, &g, &b, &int_colors) ;
	    if (sat_used) 
		G_get_color(sat_array[atcol], &saturation,&g, &b, &sat_colors) ;

	    G_get_color(hue_array[atcol], &r, &g, &b, &hue_colors) ;

	    out_array[atcol] = (CELL) G_HIS(atcol, r, g, b, intensity, saturation ) ;
	}

	if (atrow == next_row)
	    next_row = D_draw_cell_row(next_row, out_array);
	if (out_used)
	{
	    if(G_put_map_row (out_file, out_array) < 0)
		out_used = 0;
	}
	if (out_used)
	    atrow++;
	else if (next_row > 0)
	    atrow = next_row;
	else
	    break;
    }
    G_percent (window.rows, window.rows, 5);

/* Close down connection to window driver */
    R_close_driver() ;

/* Close the cell files */
    G_close_cell(hue_file) ;
    if (int_used)
	G_close_cell(int_file) ;
    if (sat_used)
	G_close_cell(sat_file) ;
    if (out_used)
    {
	printf ("CREATING SUPPORT FILES FOR %s\n", name);
	G_close_cell(out_file);
	G_write_colors (name, G_mapset(), &out_colors);
    }
    exit(0);
}
