/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       r.composite
 * AUTHOR(S):    Glynn Clements - glynn.clements@virgin.net
 * PURPOSE:      Combine red, green and blue layers into a single
 *               layer using a quantisation of the RGB color space.
 * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#include <stdio.h>
#include <string.h>

#include "gis.h"

struct band {
	struct Option *opt_name;
	struct Option *opt_levels;
	char *name;
	int levels;
	int maxlev;
	int file;
	int type;
	int size;
	void *array;
	struct Colors colors;
};

static char * const color_names[3] = {"red", "green", "blue"};

static void make_color_cube(struct Colors *colors, int nr, int ng, int nb);

int main(int argc, char **argv)
{
	struct GModule *module;
	struct Option *opt_out;
	struct Option *opt_lev;
	struct Flag *flg_o;
	struct band B[3];
	char *out_name;
	int out_file;
	CELL *out_array;
	struct Colors out_colors;
	int levels;
	char *mapset;
	int atrow, atcol;
	struct Cell_head window;
	int i;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Combines red, green and blue map layers into "
		"a single composite map layer.";

	for (i = 0; i < 3; i++)
	{
		struct Option *opt;
		char buff[80];

		B[i].opt_name = opt = G_define_option();

		sprintf(buff, "%c_map", color_names[i][0]);
		opt->key        = G_store(buff);

		opt->type       = TYPE_STRING;
		opt->answer     = NULL;
		opt->required   = YES;
		opt->gisprompt  = "old,cell,raster";

		sprintf(buff, "Name of raster map layer to be used for <%s>",
			color_names[i]);
		opt->description= G_store(buff);
	}

	opt_lev = G_define_option();
	opt_lev->key        = "levels";
	opt_lev->type       = TYPE_INTEGER;
	opt_lev->required   = NO;
	opt_lev->options    = "1-256";
	opt_lev->answer     = "32";
	opt_lev->description= "Number of levels to be used for each component";

	for (i = 0; i < 3; i++)
	{
		struct Option *opt;
		char buff[80];

		B[i].opt_levels = opt = G_define_option();

		sprintf(buff, "lev_%c", color_names[i][0]);
		opt->key        = G_store(buff);

		opt->type       = TYPE_INTEGER;
		opt->required   = NO;
		opt->options    = "1-256";

		sprintf(buff, "Number of levels to be used for <%s>",
			color_names[i]);
		opt->description= G_store(buff);
	}

	opt_out = G_define_option();
	opt_out->key        = "output";
	opt_out->type       = TYPE_STRING;
	opt_out->required   = YES;
	opt_out->gisprompt  = "new,cell,raster";
	opt_out->description= "Name of raster map to contain results";

	flg_o = G_define_flag();
	flg_o->key	    = 'o';
	flg_o->description  = "Overwrite output map";

	if (G_parser(argc, argv))
		exit(-1);

	levels = atoi(opt_lev->answer);

	for (i = 0; i < 3; i++)
	{
		struct band *b = &B[i];

		/* Get name of layer to be used */
		b->name = b->opt_name->answer;

		mapset = G_find_cell2(b->name, "");
		if (mapset == NULL)
			G_fatal_error("Cell layer [%s] does not exist", b->name);

		/* Make sure map is available */
		if ((b->file = G_open_cell_old(b->name, mapset)) == -1)
			G_fatal_error("Unable to open cellfile for [%s]", b->name);

		b->type = G_raster_map_type(b->name, mapset);

		b->size = G_raster_size(b->type);

		/* Reading color lookup table */
		if (G_read_colors(b->name, mapset, &b->colors) == -1)
			G_fatal_error("Color file for [%s] not available", b->name);

		b->array = G_allocate_raster_buf(b->type);

		b->levels = b->opt_levels->answer
			? atoi(b->opt_levels->answer)
			: levels;
		b->maxlev = b->levels - 1;
	}

	/* open output files */
	out_name = opt_out->answer;

	mapset = G_find_cell2(out_name, "");
	if (mapset != NULL)
	{
		if (flg_o->answer)
			G_remove("cell", out_name);
		else
			G_fatal_error("Cell file <%s> exists already", out_name);
	}

	if ((out_file = G_open_cell_new(out_name)) < 0)
		G_fatal_error("Unable to create output <%s>", out_name);

	out_array = G_allocate_cell_buf() ;

	/* read in current window */
	G_get_window(&window);

	/* Make color table */
	make_color_cube(&out_colors, B[0].levels, B[1].levels, B[2].levels);

	for (atrow = 0; atrow < window.rows; atrow++)
	{
		void *ptr[3];

		G_percent(atrow, window.rows, 2);

		for (i = 0; i < 3; i++)
		{
			if (G_get_raster_row(B[i].file, B[i].array, atrow, B[i].type) < 0)
				G_fatal_error("G_get_raster_row failed");
			ptr[i] = B[i].array;
		}

		for (atcol = 0; atcol < window.cols; atcol++)
		{
			int lev[3][3];
			int val[3];

			for (i = 0; i < 3; i++)
			{
				G_get_raster_color(
					ptr[i],
					&lev[i][0], &lev[i][1], &lev[i][2],
					&B[i].colors, B[i].type);

				ptr[i] = G_incr_void_ptr(ptr[i], B[i].size);
			}

			for (i = 0; i < 3; i++)
			{
				int v = lev[i][i];
			/*	int v = (lev[i][0] + lev[i][1] + lev[i][2]) / 3;	*/
				val[i] = v * B[i].maxlev / 255;
			}

			out_array[atcol] =
				(val[2] * B[1].levels + val[1]) * B[0].levels +
				val[0];
		}

		if(G_put_raster_row(out_file, out_array, CELL_TYPE) < 0)
			G_fatal_error("G_put_raster_row failed");
	}

	G_percent(window.rows, window.rows, 5);

	/* Close the input files */
	for (i = 0; i < 3; i++)
		G_close_cell(B[i].file);

	/* Close the output file */
	G_close_cell(out_file);
	G_write_colors(out_name, G_mapset(), &out_colors);

	return 0;
}

static void make_color_cube(struct Colors *colors, int nr, int ng, int nb)
{
	int mr = nr - 1;
	int mg = ng - 1;
	int mb = nb - 1;
	int r, g, b;
	int i = 0;

	G_init_colors(colors);

	for (b = 0; b < nb; b++)
	{
		G_percent(b, nb, 5);
		
		for (g = 0; g < ng; g++)
		{
			int blu = b * 255 / mb;
			int grn = g * 255 / mg;
			CELL i0 = i;
			CELL i1 = i + mr;

			G_add_c_raster_color_rule(&i0,   0, grn, blu,
						  &i1, 255, grn, blu,
						  colors);

			i += nr;
		}
	}

	G_percent(nb, nb, 5);
}
