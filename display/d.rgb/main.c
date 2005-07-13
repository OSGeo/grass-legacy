#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "display.h"
#include "raster.h"

struct band {
	struct Option *opt;
	int file;
	int type;
	void *array;
	struct Colors colors;
};

static char * const color_names[3] = {"red", "green", "blue"};

int main(int argc, char **argv)
{
	struct band B[3];
	char *mapset ;
	int row ;
	int next_row;
	struct Cell_head window ;
	struct GModule *module;
	struct Flag *flag_o;
	struct Flag *flag_x;
	int t, b, l, r ;
	int i;

	G_gisinit(argv[0]) ;

	module = G_define_module();
	module->description =
		"Displays three user-specified raster map layers "
		"as red, green, and blue overlays in the active graphics frame.";

	flag_o = G_define_flag();
	flag_o->key = 'o';
	flag_o->description = "Overlay (non-null values only)";

	flag_x = G_define_flag();
	flag_x->key = 'x';
	flag_x->description = "Don't add to list of commands in monitor";

	for (i = 0; i < 3; i++)
	{
		char buff[80];
		sprintf(buff, "Name of raster map to be used for <%s>",
			color_names[i]);

		B[i].opt = G_define_option() ;
		B[i].opt->key        = G_store(color_names[i]) ;
		B[i].opt->type       = TYPE_STRING ;
		B[i].opt->answer     = NULL ;
		B[i].opt->required   = YES ;
		B[i].opt->gisprompt  = "old,cell,raster" ;
		B[i].opt->description= G_store(buff) ;
	}

	if (G_parser(argc, argv))
		exit(-1);

	/* Do screen initializing stuff */
	if (R_open_driver() != 0)
		G_fatal_error("No graphics device selected");

	D_get_screen_window(&t, &b, &l, &r) ;
	D_cell_draw_setup_RGB(t, b, l, r) ;

	for (i = 0; i < 3; i++)
	{
		/* Get name of layer to be used */
		char *name = B[i].opt->answer;

		mapset = G_find_cell2(name, "");
		if (mapset == NULL)
			G_fatal_error("Cell layer [%s] does not exist", name);

		/* Make sure map is available */
		if ((B[i].file = G_open_cell_old(name, mapset)) == -1)
			G_fatal_error("Unable to open cellfile for [%s]", name);

		B[i].type = G_raster_map_type(name, mapset);

		/* Reading color lookup table */
		if (G_read_colors(name, mapset, &B[i].colors) == -1)
			G_fatal_error("Color file for [%s] not available", name);

		B[i].array = G_allocate_raster_buf(B[i].type);
	}

	/* read in current window */
	G_get_window(&window) ;

	D_set_colors_RGB();
	D_set_overlay_mode(flag_o->answer);

	next_row = 0;
	for (row = 0; row < window.rows; )
	{
		G_percent(row, window.rows, 5);

		for (i = 0; i < 3; i++)
			if (G_get_raster_row(B[i].file, B[i].array, row, B[i].type) < 0)
				G_fatal_error("G_get_raster_row failed");

		if (row == next_row)
			next_row = D_draw_raster_RGB(
				next_row,
				 B[0].array,   B[1].array,   B[2].array,
				&B[0].colors, &B[1].colors, &B[2].colors,
				 B[0].type,    B[1].type,    B[2].type);
		else if (next_row > 0)
			row = next_row;
		else
			break;
	}
	G_percent(window.rows, window.rows, 5);

	/* Close down connection to window driver */
	if ( !flag_x->answer )
		D_add_to_list(G_recreate_command()) ;

	R_close_driver() ;

	/* Close the cell files */
	for (i = 0; i < 3; i++)
		G_close_cell(B[i].file) ;

	return 0;
}
