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
	unsigned char *hue_n, *hue_r, *hue_g, *hue_b;
	unsigned char *int_n, *int_r;
	unsigned char *sat_n, *sat_r;
	unsigned char *dummy;
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
	struct Flag *nulldraw;
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
	
	nulldraw = G_define_flag();
	nulldraw->key = 'n';
	nulldraw->description = "Respect NULL values while drawing";


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

	hue_r = G_malloc(window.cols);
	hue_g = G_malloc(window.cols);
	hue_b = G_malloc(window.cols);
	hue_n = G_malloc(window.cols);

	dummy = G_malloc(window.cols);

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

			int_r = G_malloc(window.cols);
			int_n = G_malloc(window.cols);

			/* Reading color lookup table */
			if (G_read_colors(name_i, mapset, &int_colors) == -1)
				G_fatal_error("Color file for [%s] not available",
					      name_i) ;
		}
		else
			G_fatal_error("Not able to find cellfile [%s]", name_i) ;
					      
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

			sat_r = G_malloc(window.cols);
			sat_n = G_malloc(window.cols);

			/* Reading color lookup table */
			if (G_read_colors(name_s, mapset, &sat_colors) == -1)
				G_fatal_error("Color file for [%s] not available",
					      name_s) ;
		}
		else
			G_fatal_error("Not able to find cellfile [%s]", name_s) ;
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

		if (G_get_raster_row_colors(hue_file, atrow, &hue_colors, hue_r, hue_g, hue_b, hue_n) < 0)
			G_fatal_error("error reading hue data");
		if (int_used && (G_get_raster_row_colors(int_file, atrow, &int_colors, int_r, dummy, dummy, int_n) < 0))
			G_fatal_error("error reading intensity data");
		if (sat_used && (G_get_raster_row_colors(sat_file, atrow, &sat_colors, sat_r, dummy, dummy, sat_n) < 0))
			G_fatal_error("error reading saturation data");

		for (atcol=0; atcol<window.cols; atcol++)
		{
			if (nulldraw->answer)
			{
				if (hue_n[atcol]
				    || (int_used && int_n[atcol])
				    || (sat_used && sat_n[atcol]))
				{
					G_set_c_null_value(&r_array[atcol], 1);
					G_set_c_null_value(&g_array[atcol], 1);
					G_set_c_null_value(&b_array[atcol], 1);
					continue;
				}
			}

			if (int_used)
				intensity = int_r[atcol];

			if (sat_used)
				saturation = sat_r[atcol];

			HIS_to_RGB(hue_r[atcol], hue_g[atcol], hue_b[atcol],
				   intensity, saturation,
				   &r_array[atcol], &g_array[atcol], &b_array[atcol]);
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
