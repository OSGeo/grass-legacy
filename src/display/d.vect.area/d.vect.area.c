/*
 * 
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       d.vect.area
 * AUTHOR(S):    Eric G. Miller - egm2@jps.net 2002/01/27
 * PURPOSE:      Draw filled areas to the current display.
 * COPYRIGHT:    (C) 2002 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 ****************************************************************************
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "raster.h"
#include "colors.h"
#include "avl.h"



/* Category => Color Mapping */
struct cat_color {
	int cat;
	unsigned char R, G, B;
};

/* GLOBALS */
static struct avl_table *cat_tbl = NULL;
static struct avl_traverser cat_trav;
static struct cat_color *color_cache = NULL;
static void (*set_color) (void) = 0;

static struct cat_color * new_cat_color (void)
{
	struct cat_color *self = G_malloc (sizeof (struct cat_color));
	return self;
}

static void destroy_cat_color (void *item, void *data)
{
	free (item);
}

static int cat_comp (const void *a, const void *b, void *data)
{
	struct cat_color *A, *B;
	A = (struct cat_color *)a;
	B = (struct cat_color *)b;
	if (A->cat < B->cat) return -1;
	if (A->cat > B->cat) return 1;
	return 0;
}

static int is_cat_drawn (const int cat)
{
	int status = 0;
	struct cat_color *it = NULL;
	int def_color = -1;

	if (cat_tbl) {
		it = avl_t_find (&cat_trav, cat_tbl, &cat);
                /* maybe a default (-1) */
		if (!it) it = avl_t_find (&cat_trav, cat_tbl, &def_color);
		if (it) {
			color_cache = it;
			status = 1;
		}
		else { 
			color_cache = NULL;
			status = 0;
		}
	}
	else {
		status = 1;
	}

	return status;
}
	
static void random_color_func (void)
{
	static int bgcolr = -1;
	static int color   = -1;
	
	if (bgcolr < 0) {
		/* first call */
		char colorname[30];
		if (D_get_erase_color(colorname) != 0) {
			strcpy (colorname, "black");
		}
		bgcolr = D_translate_color (colorname);
		color = bgcolr;
	}
	color++;
	if (color == bgcolr) color++;
	if (color > MAXCOLORS) color = 1;
	if (color == bgcolr) color++;
	R_standard_color (color);
}


static void const_color_func (void)
{
	/* does nothing */
	return;
}

static void legend_color_func (void)
{
	struct cat_color *it = color_cache;
	const int colornum = MAXCOLORS + 1;
	
	if (it) {
		R_reset_color (it->R, it->G, it->B, colornum);
		R_color (colornum);
	}
}

static void add_cat_to_table (const int cat, 
			      unsigned char R,
			      unsigned char G,
			      unsigned char B)
{
	struct cat_color *it;

	if (!cat_tbl) {
		/* first time */
		cat_tbl = avl_create (cat_comp, NULL, NULL);
		if (!cat_tbl)
			G_fatal_error ("Couldn't build category color table! "\
					"Out of memory?");
	}
	it = new_cat_color();
	it->cat = cat;
	it->R   = R;
	it->G   = G;
	it->B   = B;
	if (avl_insert (cat_tbl, it) != NULL) {
		G_warning ("Attempt to insert category [%d] more than "\
				"once into color table!", cat);
		destroy_cat_color (it, NULL);
	}
}

static void destroy_cat_tbl (void)
{
	if (cat_tbl) {
		avl_destroy (cat_tbl, destroy_cat_color);
		cat_tbl = NULL;
		color_cache = NULL;
	}
}

static int use_legend_file (const char *filename)
{
	const char *delims = " :,";
	char  *cptr, *end;
	int status = 0;
	int cat;
	long red, green, blue;
	FILE *ifp;
	char buff[80];
	
	if ((ifp = fopen (filename, "r"))) {
		while (fgets (buff, 80, ifp)) {
			cptr = strtok (buff, delims);
			if (!cptr) continue;
			if (!(cat = atoi (cptr))) continue;
			cptr = strtok (NULL, delims);
			if (!cptr) continue;
			red = strtol (cptr, &end, 10);
			if (cptr == end || red < 0 || red > 255) continue;
			cptr = strtok (NULL, delims);
			if (!cptr) continue;
			green = strtol (cptr, &end, 10);
			if (cptr == end || green < 0 || green > 255) continue;
			cptr = strtok (NULL, delims);
			if (!cptr) continue;
			blue = strtol (cptr, &end, 10);
			if (cptr == end || blue < 0 || blue > 255) continue;
			add_cat_to_table (cat, (unsigned char) red,
				       	       (unsigned char) green,
				               (unsigned char) blue);
			status++;
		}
		fclose (ifp);
	}		
			
	if (status) {
		avl_t_init (&cat_trav, cat_tbl);
		set_color = legend_color_func;
	}
	
	return status;
}

static int use_const_color (const char *clr_spec)
{
	int std_color, status = 0;
	int red, green, blue;
	
	std_color = D_translate_color (clr_spec);
	if (std_color) {
		R_standard_color (std_color);
		set_color = const_color_func;
		status = 1;
	}
	else {
		if (sscanf (clr_spec, "%d:%d:%d", &red, &green, &blue) == 3)
		{
			if (red   >= 0 && red   < 256 && 
			    green >= 0 && green < 256 &&
			    blue  >= 0 && blue  < 256) {
				std_color = MAXCOLORS + 1;
				R_reset_color ((unsigned char)red,
					       (unsigned char)green,
					       (unsigned char)blue,
					       std_color);
				R_color (std_color);
				status = 1;
			}
			else {
				G_warning ("RGB colors out of range 0..255!");
			}
		}
		else
		{
			G_warning ("parsing RGB colors %s", clr_spec);
		}
	}
		
	return status;
}

static int use_random_color (void)
{
	set_color = random_color_func;
	return 1;
}
/* end of cat_color management */

int main (int argc, char **argv)
{
	char wind_name[80];
	int  color_number = 0, backgroundcolor = 0;
	char colorname[30];
	double N, S, E, W;
	struct line_pnts *points;
	char *mapset;
	double **xs, **ys;
	int rings;
	int *rpnts;
	struct Cell_head window;
	struct Map_info Map;
	struct Option *map;
	struct Option *color_const;
	struct Option *leg_opt;
	struct Flag *rand_colr;
	struct GModule *mod;
	int    dig_att;
	int nareas, i, j;
	P_AREA *pa;
	
	G_gisinit (argv[0]);

	mod 			= G_define_module();
	mod->description        = "Draws filled vector areas to the current "\
				  "display device";
	
	rand_colr		= G_define_flag();
	rand_colr->key		= 'r';
	rand_colr->description  = "Use \"random\" colors";
	
	map		    	= G_define_option();
	map->description	= "Name of existing vector map to draw";
	map->key		= "map";
	map->type		= TYPE_STRING;
	map->required		= YES;
	map->gisprompt		= "old,dig,vector";

	color_const		= G_define_option();
	color_const->description = "Draw using either a GRASS standard color or "
				   "R:G:B triplet (separated by colons)";
	color_const->key	= "color";
	color_const->required   = NO;
	color_const->type	= TYPE_STRING;
	color_const->answer     = "white";
	
	leg_opt			= G_define_option();
	leg_opt->description    = "Draw areas according to a legend file "\
		                  "(<cat> <R> <G> <B>)";
	leg_opt->key		= "legend";
	leg_opt->type		= TYPE_STRING;

	if (G_parser (argc, argv))
		exit (EXIT_FAILURE);

	mapset = G_find_file2 ("dig", map->answer, "");
	if (!mapset) {
		G_fatal_error ("Couldn't find vector map [%s]!", map->answer);
	}

	if (R_open_driver() != 0)
		G_fatal_error ("Couldn't open display! Please select a monitor.");

	D_setup(0);

	if (leg_opt->answer) {
		if (!use_legend_file (leg_opt->answer)) {
			G_fatal_error ("Failed to parse legend file [%s]!",
					leg_opt->answer);
		}
	}
	else if (rand_colr->answer) {
		if (!use_random_color()) {
			G_fatal_error ("setting up random color selection");
		}
	}
	else
	{
		if (!use_const_color (color_const->answer)) {
			G_fatal_error ("setting constant color [%s]!",
					color_const->answer);
		}
	}
	
	if (D_get_cur_wind (wind_name))
		G_fatal_error ("Couldn't get window name!");
	if (D_set_cur_wind (wind_name))
		G_fatal_error ("Couldn't select current window!");
	G_get_set_window (&window);
	G_setup_plot (D_get_d_north(),
		      D_get_d_south(),
		      D_get_d_west(),
		      D_get_d_east(),
		      D_move_abs, 
		      D_cont_abs);

	points = Vect_new_line_struct();

	if (2 > Vect_open_old (&Map, map->answer, mapset))
		G_fatal_error ("opening vector file. Please run v.support!");

	nareas = V2_num_areas (&Map);
	G_percent (0, nareas, 5);
	for (i = 1; i <= nareas; i++) {
		
		if ((dig_att = V2_area_att(&Map, i)) == 0)
			continue;
		
		V2_get_area_bbox (&Map, i, &N, &S, &E, &W);
		if (S > window.north || N < window.south ||
		    W > window.east  || E < window.west)
			continue;
	
		if (!is_cat_drawn (dig_att))
			continue;

		set_color();
		
		V2_get_area (&Map, i, &pa);

		rings = 1 + pa->n_isles;
		xs = (double **) G_malloc (sizeof(double *) * rings);
		ys = (double **) G_malloc (sizeof(double *) * rings);
		rpnts = (int *) G_malloc (sizeof (int) * rings);
		Vect_get_area_points (&Map, i, points);
		rpnts[0] = points->n_points;
		xs[0] = (double *) G_malloc (sizeof(double) * rpnts[0]);
		ys[0] = (double *) G_malloc (sizeof(double) * rpnts[0]);
		Vect_copy_pnts_to_xy (points, xs[0], ys[0], &rpnts[0]);
		for (j = 0; j < pa->n_isles; j++) {
			Vect_get_isle_points (&Map, pa->isles[j], points);
			rpnts[j+1] = points->n_points;
			xs[j+1] = (double *) G_malloc (sizeof(double) * rpnts[j+1]);
			ys[j+1] = (double *) G_malloc (sizeof(double) * rpnts[j+1]);
			Vect_copy_pnts_to_xy (points, xs[j+1], ys[j+1], &rpnts[j+1]);
		}

		G_plot_area (xs, ys, rpnts, rings);
		for (j = 0; j < rings; j++)
		{
			free (xs[j]);
			free (ys[j]);
		}
		free (xs);
		free (ys);
		free (rpnts);
		G_percent (i, nareas, 5);
	}

	D_add_to_list(G_recreate_command()) ;

        D_set_dig_name(G_fully_qualified_name(map->answer, mapset));
        D_add_to_dig_list(G_fully_qualified_name(map->answer, mapset));

	R_close_driver();

	return 0;
}

