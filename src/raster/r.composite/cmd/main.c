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
#include <stdlib.h>
#include <string.h>

#include "gis.h"

struct band {
	struct Option *opt_name;
	struct Option *opt_levels;
	char *name;
	int levels;
	int maxlev;
	int offset;
	int file;
	int type;
	int size;
	unsigned char *array[3];
	struct Colors colors;
};

static char * const color_names[3] = {"red", "green", "blue"};

static struct band B[3];
static int dither, closest;

static void make_color_cube(struct Colors *colors);
static int quantize(int c, int x);

int main(int argc, char **argv)
{
	struct GModule *module;
	struct Option *opt_out;
	struct Option *opt_lev;
	struct Flag *flg_o;
	struct Flag *flg_d;
	struct Flag *flg_c;
	char *out_name;
	int out_file;
	CELL *out_array;
	struct Colors out_colors;
	int levels;
	char *mapset;
	int atrow, atcol;
	struct Cell_head window;
	unsigned char *dummy, *nulls;
	int i, j;

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

	flg_d = G_define_flag();
	flg_d->key	    = 'd';
	flg_d->description  = "Dither";

	flg_c = G_define_flag();
	flg_c->key	    = 'c';
	flg_c->description  = "Use closest color";

	if (G_parser(argc, argv))
		exit(-1);

	levels = atoi(opt_lev->answer);

	dither = flg_d->answer;
	closest = flg_c->answer;

	/* read in current window */
	G_get_window(&window);

	dummy = (unsigned char *) G_malloc(window.cols);

	nulls = (unsigned char *) G_malloc(window.cols);

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

		for (j = 0; j < 3; j++)
			b->array[j] = (i == j)
				? (unsigned char *) G_malloc(window.cols)
				: dummy;

		b->levels = b->opt_levels->answer
			? atoi(b->opt_levels->answer)
			: levels;
		b->maxlev = b->levels - 1;
		b->offset = 128 / b->maxlev;
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

	/* Make color table */
	make_color_cube(&out_colors);

	for (atrow = 0; atrow < window.rows; atrow++)
	{
		int diff[3];

		G_percent(atrow, window.rows, 2);

		for (i = 0; i < 3; i++)
		{
			if (G_get_raster_row_colors(
				    B[i].file, atrow, &B[i].colors,
				    B[i].array[0],
				    B[i].array[1],
				    B[i].array[2],
				    nulls) < 0)
				G_fatal_error("Error reading '%s' map", color_names[i]);

			diff[i] = 0;
		}

		for (atcol = 0; atcol < window.cols; atcol++)
		{
			int val[3];

			if (nulls[atcol])
			{
				G_set_c_null_value(&out_array[atcol], 1);
				continue;
			}

			for (i = 0; i < 3; i++)
			{
				int v = B[i].array[i][atcol];

				if (dither)
				{
					int r, w;

					v += diff[i];
					r = quantize(i, v);
					r = 	(r<0) ? 0 :
						(r>256) ? 256 :
						r;
					w = r * 255 / B[i].maxlev;
					diff[i] = v - w;
					val[i] = r;
				}
				else
					val[i] = quantize(i, v);
			}

			out_array[atcol] = (CELL)
				(val[2] * B[1].levels + val[1]) * B[0].levels +
				val[0];
		}

		if(G_put_raster_row(out_file, out_array, CELL_TYPE) < 0)
			G_fatal_error("G_put_raster_row failed (file system full?)");
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

static int quantize(int c, int x)
{
	return	closest
		? (x + B[c].offset) * B[c].maxlev / 256
		: x * B[c].levels / 256;
}

static void make_color_cube(struct Colors *colors)
{
	int nr = B[0].levels;
	int ng = B[1].levels;
	int nb = B[2].levels;
	int mr = B[0].maxlev;
	int mg = B[1].maxlev;
	int mb = B[2].maxlev;
	int g, b;
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
