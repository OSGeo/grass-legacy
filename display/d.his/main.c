#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "his.h"

int 
main (int argc, char **argv)
{
	CELL *hue_array ;
	CELL *int_array = NULL;
	CELL *sat_array = NULL;
	CELL *r_array, *g_array, *b_array;
	char *mapset ;
	char *name_h, *name_i, *name_s ;
	int intensity ;
	int saturation ;
	int atrow, atcol ;
	int next_row;
	int hue_file ;
	int int_file = 0;
	int int_used ;
	int sat_file = 0;
	int sat_used ;
	struct Cell_head window ;
	struct Colors hue_colors ;
	struct Colors int_colors ;
	struct Colors sat_colors ;
	struct Colors gray_colors ;
	struct GModule *module;
	struct Option *opt_h, *opt_i, *opt_s;
	int offset ;
	char window_name[64] ;
	int t, b, l, r ;

	G_gisinit(argv[0]) ;

	module = G_define_module();
	module->description =
		"Displays the result obtained by combining "
		"hue, intensity, and saturation (his) values "
		"from user-specified input raster map layers.";

	opt_h = G_define_option() ;
	opt_h->key        = "h_map" ;
	opt_h->type       = TYPE_STRING ;
	opt_h->required   = YES ;
	opt_h->gisprompt  = "old,cell,raster" ;
	opt_h->description= "Name of layer to be used for HUE" ;

	opt_i = G_define_option() ;
	opt_i->key        = "i_map" ;
	opt_i->type       = TYPE_STRING ;
	opt_i->required   = NO ;
	opt_i->gisprompt  = "old,cell,raster" ;
	opt_i->description= "Name of layer to be used for INTENSITY" ;

	opt_s = G_define_option() ;
	opt_s->key        = "s_map" ;
	opt_s->type       = TYPE_STRING ;
	opt_s->required   = NO ;
	opt_s->gisprompt  = "old,cell,raster" ;
	opt_s->description= "Name of layer to be used for SATURATION" ;

	if (G_parser(argc, argv))
		exit(-1);

	/* read in current window */
	G_get_window(&window) ;

	/* Do screen initializing stuff */

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current graphics window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current graphics window not available") ;

	D_set_cell_name("his result") ;

	/* Get color offset value for current graphics window and pass to driver */
	D_offset_is(&offset) ;
	R_color_offset(offset) ;

	/* Prepare the raster cell drawing functions */
	D_get_screen_window(&t, &b, &l, &r) ;
	D_cell_draw_setup_RGB(t, b, l, r) ;

	/* Get name of layer to be used for hue */
	name_h = opt_h->answer;

	mapset = G_find_cell2(name_h, "");
	if (mapset == NULL)
		G_fatal_error("%s: <%s> cell file not found\n",
			      G_program_name(),
			      opt_h->answer);

	/* Make sure map is available */
	if ((hue_file = G_open_cell_old(name_h, mapset)) == -1)
		G_fatal_error("Not able to open cellfile for [%s]",
			      name_h) ;

	hue_array = G_allocate_cell_buf () ;

	/* Reading color lookup table */
	if (G_read_colors(name_h, mapset, &hue_colors) == -1)
		G_fatal_error("Color file for [%s] not available",
			      name_h) ;

	int_used = 0 ;

	if (opt_i->answer != NULL)
	{
		/* Get name of layer to be used for intensity */
		name_i = opt_i->answer;
		mapset = G_find_cell2(name_i, "");
		if (mapset != NULL)
		{
			int_used = 1 ;
			/* Make sure map is available */
			if ((int_file = G_open_cell_old(name_i, mapset)) == -1)
				G_fatal_error("Not able to open cellfile for [%s]",
					      name_i) ;

			int_array = G_allocate_cell_buf () ;

			/* Reading color lookup table */
			if (G_read_colors(name_i, mapset, &int_colors) == -1)
				G_fatal_error("Color file for [%s] not available",
					      name_i) ;
		}
		else
		{
		    G_fatal_error("Not able to find cellfile [%s]",
					      name_i) ;
		}
					      
	}

	sat_used = 0 ;

	if (opt_s->answer != NULL)
	{
		/* Get name of layer to be used for saturation */
		name_s = opt_s->answer;
		mapset = G_find_cell2 (name_s, "");
		if (mapset != NULL)
		{
			sat_used = 1 ;

			/* Make sure map is available */
			if ((sat_file = G_open_cell_old(name_s, mapset)) == -1)
				G_fatal_error("Not able to open cellfile for [%s]",
					      name_s) ;

			sat_array = G_allocate_cell_buf () ;

			/* Reading color lookup table */
			if (G_read_colors(name_s, mapset, &sat_colors) == -1)
				G_fatal_error("Color file for [%s] not available",
					      name_s) ;
		}
		else
		{
		    G_fatal_error("Not able to find cellfile [%s]",
					      name_s) ;
		}
	}

	r_array = G_allocate_cell_buf () ;
	g_array = G_allocate_cell_buf () ;
	b_array = G_allocate_cell_buf () ;

	/* Make color table */
	make_gray_scale(&gray_colors) ;
	D_set_colors_RGB();

	/* Now do the work */
	intensity =  255 ;  /* default is to not change intensity */
	saturation = 255 ;  /* default is to not change saturation */

	next_row = 0;
	for (atrow=0; atrow<window.rows; )
	{
		G_percent (atrow, window.rows, 2);
		if(G_get_c_raster_row(hue_file, hue_array, atrow) < 0)
			exit(1);
		if (int_used && (G_get_c_raster_row(int_file, int_array, atrow) < 0))
			exit(1);
		if (sat_used && (G_get_c_raster_row(sat_file, sat_array, atrow) < 0))
			exit(1);

		for (atcol=0; atcol<window.cols; atcol++)
		{
			int r, g, b ;

			if (int_used)
			{
				G_get_color(int_array[atcol], &r, &g, &b, &int_colors) ;
				intensity = r;
				/* intensity = (r + g + b) / 3; */
			}

			if (sat_used)
			{
				G_get_color(sat_array[atcol], &r, &g, &b, &sat_colors) ;
				saturation = r;
				/* saturation = (r + g + b) / 3; */
			}

			G_get_color(hue_array[atcol], &r, &g, &b, &hue_colors) ;

			HIS_to_RGB(r, g, b, intensity, saturation,
				   &r_array[atcol], &g_array[atcol], &b_array[atcol]) ;
		}

		if (atrow == next_row)
			next_row = D_draw_cell_RGB(next_row,
						   r_array, g_array, b_array,
						   &gray_colors, &gray_colors, &gray_colors);

		if (next_row > 0)
			atrow = next_row;
		else
			break;
	}
	G_percent (window.rows, window.rows, 5);

	/* Close down connection to display driver */
	D_add_to_list(G_recreate_command()) ;
	R_close_driver() ;

	/* Close the cell files */
	G_close_cell(hue_file) ;
	if (int_used)
		G_close_cell(int_file) ;
	if (sat_used)
		G_close_cell(sat_file) ;

	exit(0);
}
