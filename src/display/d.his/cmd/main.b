#include <stdio.h>
#include "gis.h"

main(argc, argv)
	int argc ;
	char **argv ;
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
	int hue_file ;
	int int_file ;
	int int_used ;
	int new_percent ;
	int percent ;
	int sat_file ;
	int sat_used ;
	struct Cell_head window ;
	struct Colors hue_colors ;
	struct Colors int_colors ;
	struct Colors out_colors ;
	struct Colors sat_colors ;

	G_gisinit(argv[0]) ;

/* Print advertising */
	G_clear_screen() ;
	printf("\n\n  HIS (Hue/Intensity/Saturation) PICTURE GENERATION UTILITY\n") ;

/* Do screen initializing stuff */
	if (init_his())
		exit(1) ;

/* Get name of layer to be used for hue */
	mapset = 
		G_ask_cell_old("Enter name of data layer to be used for HUE.", name) ;
	if (!name || (*name == NULL) )
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
	mapset = 
		G_ask_cell_old("Enter name of data layer to be used for INTENSITY.",
		name) ;
	if (!mapset || (*mapset == NULL) )
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
	mapset = 
		G_ask_cell_old("Enter name of data layer to be used for SATURATION.",
		name) ;
	if (!mapset || (*mapset == NULL) )
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

	out_array = G_allocate_cell_buf () ;

/* read in current window */
	G_get_window(&window) ;

/* Make color table */
	G_make_HIS_color(&out_colors) ;
	R_reset_colors(out_colors.min, out_colors.max, out_colors.red, out_colors.grn, out_colors.blu) ;
	R_reset_colors(0, 0, &out_colors.r0, &out_colors.g0, &out_colors.b0) ;

/* Now do the work */
	intensity =  127 ;  /* default is to not change intensity */
	saturation = 255 ;  /* default is to not change saturation */

	setbuf(stdout, 0) ;
	percent = 0 ;
	printf("\nPercent done: %4d ", percent) ;
	for (atrow=0; atrow<window.rows; )
	{
		if ( (new_percent = 100 * atrow / window.rows ) != percent)
			printf("%4d", percent = new_percent ) ;
		G_get_map_row(hue_file, hue_array, atrow) ; 
		if (int_used)
			G_get_map_row(int_file, int_array, atrow) ; 
		if (sat_used)
			G_get_map_row(sat_file, sat_array, atrow) ; 
		
		for (atcol=0; atcol<window.cols; atcol++)
		{
			int r, g, b ;

			if (int_used) 
				G_get_color(int_array[atcol], &intensity, &g, &b, &int_colors) ;
			if (sat_used) 
				G_get_color(sat_array[atcol], &saturation,&g, &b, &sat_colors) ;

			G_get_color(hue_array[atcol], &r, &g, &b, &hue_colors) ;

			out_array[atcol] = (CELL) G_HIS(r, g, b, intensity, saturation ) ;
		}

		if ((atrow = D_draw_cell_row(atrow, out_array)) < 0)
			break ;
	}
	printf("\n") ;

/* Close down connection to window driver */
	R_close_driver() ;

/* Close the cell files */
	G_close_cell(hue_file) ;
	if (int_used)
		G_close_cell(int_file) ;
	if (int_used)
		G_close_cell(sat_file) ;
}
