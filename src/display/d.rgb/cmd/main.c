#include <string.h>
#include <stdio.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

int main (int argc, char **argv)
{
	CELL *b1_array ;
	CELL *b2_array ;
	CELL *b3_array ;
	CELL *out_array ;
	char *mapset ;
	char name[20] ;
	char buff[128] ;
	int atrow, atcol ;
	int next_row;
	int b1_file ;
	int b2_file ;
	int b1_used ;
	int b2_used ;
	int b3_file ;
	int b3_used ;
	int out_file ;
	int out_used ;
	static int have_one = 1 ;
	struct Cell_head window ;
	struct Colors b1_colors ;
	struct Colors b2_colors ;
	struct Colors b3_colors ;
	struct Colors out_colors ;
	struct GModule *module;
	struct Option *opt1, *opt2, *opt3, *opt4 ;

	module = G_define_module();
	module->description =
		"Displays three user-specified raster map layers "
		"as red, green, and blue overlays in the active graphics frame.";

	opt1 = G_define_option() ;
	opt1->key        = "red" ;
	opt1->type       = TYPE_STRING ;
	opt1->answer     = NULL ;
	opt1->required   = NO ;
	opt1->gisprompt  = "old,cell,raster" ;
	opt1->description= "Name of raster map to be used for RED" ;

	opt2 = G_define_option() ;
	opt2->key        = "green" ;
	opt2->type       = TYPE_STRING ;
	opt2->answer     = NULL ;
	opt2->required   = NO ;
	opt2->gisprompt  = "old,cell,raster" ;
	opt2->description= "Name of raster map to be used for GREEN" ;

	opt3 = G_define_option() ;
	opt3->key        = "blue" ;
	opt3->type       = TYPE_STRING ;
	opt3->answer     = NULL ;
	opt3->required   = NO ;
	opt3->gisprompt  = "old,cell,raster" ;
	opt3->description= "Name of raster map to be used for BLUE" ;

	opt4 = G_define_option() ;
	opt4->key        = "out" ;
	opt4->type       = TYPE_STRING ;
	opt4->required   = NO ;
	opt4->gisprompt  = "new,cell,raster" ;
	opt4->description= "Name of raster map to contain results" ;

	G_gisinit(argv[0]) ;

	if (G_parser(argc, argv))
		exit(-1);

	/* Do screen initializing stuff */
	init_rgb();

	/* Get name of layer to be used for red */

	b1_used = 0 ;

	strcpy (name, opt1->answer);

	mapset = G_find_cell2(name, "");
	if (mapset == NULL)
	{
		b1_used = 0 ;
	}
	else
	{
		have_one++ ;
		b1_used = 1 ;

		/* Make sure map is available */
		if ((b1_file = G_open_cell_old(name, mapset)) == -1)
		{
			sprintf(buff,"Not able to open cellfile for [%s]", name) ;
			G_fatal_error(buff) ;
			exit(-1);
		}

		b1_array = G_allocate_cell_buf () ;

		/* Reading color lookup table */
		if (G_read_colors(name, mapset, &b1_colors) == -1)
		{
			sprintf(buff,"Color file for [%s] not available", name) ;
			G_fatal_error(buff) ;
		}
	}


	/* Get name of layer to be used for green */

	b2_used = 0 ;

	strcpy (name, opt2->answer);

	mapset = G_find_cell2(name, "");
	if (mapset == NULL)
	{
		b2_used = 0 ;
	}
	else
	{
		have_one++ ;
		b2_used = 1 ;

		/* Make sure map is available */
		if ((b2_file = G_open_cell_old(name, mapset)) == -1)
		{
			sprintf(buff,"Not able to open cellfile for [%s]", name) ;
			G_fatal_error(buff) ;
		}

		b2_array = G_allocate_cell_buf () ;

		/* Reading color lookup table */
		if (G_read_colors(name, mapset, &b2_colors) == -1)
		{
			sprintf(buff,"Color file for [%s] not available", name) ;
			G_fatal_error(buff) ;
		}
	}


	/* Get name of layer to be used for blue */

	b3_used = 0 ;

	strcpy (name, opt3->answer);
	mapset = G_find_cell2(name, "");
	if (mapset == NULL)
	{
		b3_used = 0 ;
	}
	else
	{
		have_one++ ;
		b3_used = 1 ;

		/* Make sure map is available */
		if ((b3_file = G_open_cell_old(name, mapset)) == -1)
		{
			sprintf(buff,"Not able to open cellfile for [%s]", name) ;
			G_fatal_error(buff) ;
		}

		b3_array = G_allocate_cell_buf () ;

		/* Reading color lookup table */
		if (G_read_colors(name, mapset, &b3_colors) == -1)
		{
			sprintf(buff,"Color file for [%s] not available", name) ;
			G_fatal_error(buff) ;
		}
	}

	if (! have_one)
	{
		fprintf (stdout,"ERROR: You must specify at least one input map\n") ;
		exit(-1) ;
	}

	out_used = 0;

	if (opt4->answer != NULL)
	{
		strcpy (name, opt4->answer);

		mapset = G_find_cell2(name, "");
		if (mapset == NULL)
		{
			out_used = 0;
		}
		else
		{
			sprintf(buff,"Color file for [%s] exists already", name);
			G_fatal_error(buff) ;
			exit(1);
		}
		if ((out_file = G_open_cell_new (name)) < 0)
		{
			out_used = 0;
		}
		else
			out_used = 1;
	}

	out_array = G_allocate_cell_buf () ;

	/* read in current window */
	G_get_window(&window) ;

	/* Make color table */
	G_make_RGB_color(&out_colors) ;
	D_set_colors (&out_colors);
	alloc_pass_buff(window.cols) ;

	next_row = 0;
	for (atrow=0; atrow<window.rows; )
	{
		int r, g, b, R, G, B ;
		R = G = B = 0 ;

		G_percent (atrow, window.rows, 5);
		if (b1_used && (G_get_map_row(b1_file, b1_array, atrow) < 0))
			exit(1);
		if (b2_used && (G_get_map_row(b2_file, b2_array, atrow) < 0))
			exit(1);
		if (b3_used && (G_get_map_row(b3_file, b3_array, atrow) < 0))
			exit(1);

		for (atcol=0; atcol<window.cols; atcol++)
		{
			if (b1_used)
			{
				G_get_color(b1_array[atcol], &r, &g, &b, &b1_colors) ;
				R = (r + g + b) / 3 ;
			}
			if (b2_used)
			{
				G_get_color(b2_array[atcol], &r, &g, &b, &b2_colors) ;
				G = (r + g + b) / 3 ;
			}
			if (b3_used)
			{
				G_get_color(b3_array[atcol], &r, &g, &b, &b3_colors) ;
				B = (r + g + b) / 3 ;
			}

			out_array[atcol] = (CELL) G_RGB(atcol, R, G, B) ;
		}

		if (atrow == next_row)
			next_row = D_draw_cell(next_row, out_array, &out_colors);
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
	D_add_to_list(G_recreate_command()) ;
	R_close_driver() ;

	/* Close the cell files */
	G_close_cell(b1_file) ;
	if (b2_used)
		G_close_cell(b2_file) ;
	if (b3_used)
		G_close_cell(b3_file) ;
	if (out_used)
	{
		fprintf (stdout,"CREATING SUPPORT FILES FOR %s\n", name);
		G_close_cell(out_file);
		G_write_colors (name, G_mapset(), &out_colors);
	}
	exit(0);
}
