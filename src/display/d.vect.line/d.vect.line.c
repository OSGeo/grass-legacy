/*
 * 
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       d.vect.line
 * AUTHOR(S):    Eric G. Miller - egm2@jps.net 2002/02/04
 * PURPOSE:      Draw labelled lines with a specified color.
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
struct rgb_color {
	unsigned char R, G, B;
};

struct cat_color {
	int cat;
	struct rgb_color *line;
};

/* GLOBALS */
static struct Cell_head window;
static struct avl_table *cat_tbl = NULL;
static struct avl_table *clr_tbl = NULL;
static struct rgb_color *def_line_color = NULL;
static int  (*select_color) (const int) = 0;
static int std_clr_line = 0;

/* Local PROTOS */
static struct cat_color *new_cat_color(void);
static struct rgb_color *new_rgb_color(void);
static void destroy_cat_color(void *item, void *data);
static void destroy_rgb_color(void *item, void *data);
static int cat_comp(const void *a, const void *b, void *data);
static int rgb_comp(const void *a, const void *b, void *data);
static int legend_color_func(const int cat);
static int const_color_func(const int cat);
static struct rgb_color *
add_color_to_table(unsigned char R, unsigned char G, unsigned char B);
static void 
add_cat_to_table(int cat, struct rgb_color *line);
static void destroy_cat_tbl(void);
static void destroy_rgb_tbl(void);
static int rgb_ok(int red, int green, int blue);
static int use_legend_file(const char *filename);
static int rgb_parse(const char *clr_spec, 
		unsigned char *red, unsigned char *green, unsigned char *blue);
static int use_const_color(const char *line_clr, char **catlist);
static int write_legend(const char *filename);
static void plot_line(struct line_pnts *points);



static struct cat_color * new_cat_color (void)
{
	struct cat_color *self = G_malloc (sizeof (struct cat_color));
	return self;
}

static struct rgb_color * new_rgb_color (void)
{
	struct rgb_color *self = G_malloc (sizeof (struct rgb_color));
	return self;
}

static void destroy_cat_color (void *item, void *data)
{
	free (item);
}


static void destroy_rgb_color (void *item, void *data)
{
	free (item);
}

static int cat_comp (const void *a, const void *b, void *data)
{
	const struct cat_color *A, *B;
	A = (const struct cat_color *)a;
	B = (const struct cat_color *)b;
	if (A->cat < B->cat) return -1;
	if (A->cat > B->cat) return 1;
	return 0;
}

static int rgb_comp (const void *a, const void *b, void *data)
{
	const struct rgb_color *A, *B;
	A = (struct rgb_color *)a;
	B = (struct rgb_color *)b;
	/* Order by Red, Green, Blue */
	if (A->R < B->R) return -1;
	if (A->R > B->R) return 1;
	if (A->G < B->G) return -1;
	if (A->G > B->G) return 1;
	if (A->B < B->B) return -1;
	if (A->B > B->B) return 1;
	return 0;
}


static int legend_color_func (const int cat)
{
	int status = 0;
	const int colornum = MAXCOLORS + 1;
	struct cat_color *self = NULL;
	struct rgb_color *clr = NULL;
	
	if (cat_tbl) {
		self = avl_find (cat_tbl, &cat);
		if (self && self->line) {
			clr = self->line;
			status = 1;
		}
		else if (def_line_color) {
			clr = def_line_color;
			status = 1;
		}
		else {
			status = 0;
		}
	}
	
	if (status) {
		R_reset_color (clr->R, clr->G, clr->B, colornum);
		R_color (colornum);
	}			

	return status;
}


static int const_color_func (const int cat)
{
	int colornum = MAXCOLORS + 1;
	int status = 0;
	
	if (cat_tbl && avl_find (cat_tbl, &cat) == NULL)
		return status;

	if (std_clr_line) {
		R_standard_color (std_clr_line);
		status = 1;
	}
	else if (def_line_color) {
		R_reset_color (def_line_color->R,
				def_line_color->G,
				def_line_color->B,
				colornum);
		R_color (colornum);
		status = 1;
	}

	return status;
}

static struct rgb_color *
add_color_to_table (unsigned char R, unsigned char G, unsigned char B)
{
	struct rgb_color *self;
	struct rgb_color *other;
	
	if (!clr_tbl) {
		/* first time */
		clr_tbl = avl_create (rgb_comp, NULL, NULL);
		if (!clr_tbl)
			G_fatal_error ("Couldn't create color table! Out of memory?");
	}
	self = new_rgb_color();
	self->R = R;
	self->G = G;
	self->B = B;
	if ((other = avl_insert (clr_tbl, self)) != NULL) {
		/* duplicate color */
		destroy_rgb_color (self, NULL);
		self = other;
	}
	return self;
}
	
static void add_cat_to_table (int cat, struct rgb_color *line)
{
	struct cat_color *self;

	if (!cat_tbl) {
		/* first time */
		cat_tbl = avl_create (cat_comp, NULL, NULL);
		if (!cat_tbl)
			G_fatal_error ("Couldn't build category color table! "\
					"Out of memory?");
	}
	self = new_cat_color();
	self->cat = cat;
	self->line = line;

	if (avl_insert (cat_tbl, self) != NULL) {
		G_warning ("Attempt to insert category [%d] more than "\
				"once into color table!", cat);
		destroy_cat_color (self, NULL);
	}
}

static void destroy_cat_tbl (void)
{
	if (cat_tbl) {
		avl_destroy (cat_tbl, destroy_cat_color);
		cat_tbl = NULL;
	}
}

static void destroy_rgb_tbl (void)
{
	if (clr_tbl) {
		avl_destroy (clr_tbl, destroy_rgb_color);
		clr_tbl = NULL;
	}
}

static int rgb_ok (int red, int green, int blue)
{
	if (red >= 0 && red < 256 &&
	    green >= 0 && green < 256 &&
	    blue >= 0 && blue < 256)
		return 1;
	else
		return 0;
}

static int use_legend_file (const char *filename)
{
	const char *format = "%d %d %d %d";
	int status = 0;
	int conversions = 0;
	int cat;
	int line_cnt = 0;
	long red, green, blue;
	struct rgb_color *line;
	FILE *ifp = NULL;
	char buff[512];
	
	if (!strcmp (filename, "-")) {
		ifp = stdin;
	}
	else {
		ifp = fopen (filename, "r");
	}
	if (ifp) {
		while (fgets (buff, 80, ifp)) {
			line_cnt++;
			conversions = sscanf (buff, format, &cat,
					&red, &green, &blue);
			if (conversions < 1)
				continue;
			else if (conversions != 4) {
				G_warning ("ignoring line [%d] in legend file!", line_cnt);
				continue;
			}

			if (rgb_ok (red, green, blue)) {
				line = add_color_to_table ( (unsigned char) red,
						            (unsigned char) green,
							    (unsigned char) blue );
			}
			else {
				G_warning ("bad color spec on line [%d], skipping",
						line_cnt);
				continue;
			}
			
			if (cat < 1) {
				/* defaults? */
				if (def_line_color == NULL) {
					def_line_color = line;
				}
				else {
					G_warning ("duplicate default color on line [%d]",
							line_cnt);
					destroy_rgb_color (line, NULL);
					continue;
				}
			}
			else {
				add_cat_to_table (cat, line);
			}
			status++;
		}
		fclose (ifp);
	}		
			
	if (status) {
		select_color = legend_color_func;
	}
	
	return status;
}

static int rgb_parse (const char *clr_spec, 
		      unsigned char *red,
		      unsigned char *green,
		      unsigned char *blue)
{
	int status = 0;
	int R, G, B;

	if (sscanf (clr_spec, "%d:%d:%d", &R, &G, &B) == 3) {
		if (rgb_ok (R, G, B)) {
			*red   = (unsigned char) R;
			*green = (unsigned char) G;
			*blue  = (unsigned char) B;
			status = 1;
		}
	}
	return status;
}

static int use_const_color (const char *line_clr, char **catlist)
{
	int std_color, status = 0;
	unsigned char red, green, blue;
	int catnum;
	struct cat_color *cat;
	
	if (catlist) {
		cat_tbl = avl_create (cat_comp, NULL, NULL);
		if (!cat_tbl)
			G_fatal_error ("creating avl tree. Out of memory?");
		while (*catlist) {
			catnum = atoi (*catlist);
			if (catnum) {
				cat = new_cat_color();
				cat->cat = catnum;
				cat->line = NULL;
				avl_insert (cat_tbl, cat);
			}
			catlist++;
		}
		if (avl_count(cat_tbl) == 0) {
			avl_destroy (cat_tbl, NULL);
			cat_tbl = NULL;
			G_warning ("Failed to convert category numbers!");
		}
	}

	std_color = D_translate_color (line_clr);
	if (std_color) {
		std_clr_line = std_color;
		status = 1;
	}
	else if (rgb_parse (line_clr, &red, &green, &blue)) {
		def_line_color = new_rgb_color();
		def_line_color->R = red;
		def_line_color->G = green;
		def_line_color->B = blue;
		status = 1;
	}
	else
	{
		G_warning ("parsing RGB colors %s", line_clr);
	}

	if (status)
		select_color = const_color_func;

	return status;
}


static int write_legend (const char *filename)
{
	FILE *ofp;
	const char *fmt = "%d %d %d %d\n";
	struct avl_traverser trav;
	struct rgb_color *line;
	struct cat_color *cat;
	int status = 0;

	if ((ofp = fopen (filename, "w"))) {
		/* write default first, if any */
		if (def_line_color) {
			line = def_line_color;
			fprintf (ofp, fmt, -1,
				 line->R, line->G, line->B);
		}
					
		/* traverse cat tree */
		avl_t_init (&trav, cat_tbl);
		avl_t_first (&trav, cat_tbl);
		while ((cat = avl_t_cur (&trav))) {
			line = cat->line;
			if (line) {
				fprintf (ofp, fmt, cat->cat,
					 line->R, line->G, line->B);
			}
			avl_t_next (&trav);
		}
		fclose (ofp);
		status = 1;
	}

	return status;
}

/* end of cat_color management */


static void plot_line (struct line_pnts *points)
{
	int i;
	
	for (i = 0; i < points->n_points - 1; i++) 
        {
            G_plot_line (points->x[i],   points->y[i],
                         points->x[i+1], points->y[i+1]);
	}
}


int main (int argc, char **argv)
{
	char wind_name[80];
	char *tmpfile;
	char *cmd_buff;
	int  len;
	double N, S, E, W;
	struct line_pnts *points;
	char *mapset;
	struct Map_info Map;
	struct Option *map;
	struct Option *line_color;
	struct Option *catnum;
	struct Option *leg_opt;
	struct GModule *mod;
	int    dig_att;
	int nlines, i;
	
	G_gisinit (argv[0]);

	mod 			= G_define_module();
	mod->description        = "Draw labelled vector lines to the current "\
				  "display device";
	
	map		    	= G_define_option();
	map->description	= "Name of existing vector map to draw";
	map->key		= "map";
	map->type		= TYPE_STRING;
	map->required		= YES;
	map->gisprompt		= "old,dig,vector";

	line_color		= G_define_option();
	line_color->key		= "color";
	line_color->description = "Draw all lines using either a GRASS standard "\
				  "color or R:G:B triplet (separated by colons)";
	line_color->type	= TYPE_STRING;
	line_color->required	= NO;
	line_color->answer	= "white";
	
	catnum			= G_define_option();
	catnum->key		= "catnum";
	catnum->description     = "Only draw areas with a matching category number "\
				  "(for constant color option only)";
	catnum->required	= NO;
	catnum->multiple	= YES;
	catnum->type		= TYPE_INTEGER;
	catnum->answers		= NULL;
	
	leg_opt			= G_define_option();
	leg_opt->description    = "Draw lines according to a legend file "\
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
	else
	{
		if (!use_const_color (line_color->answer, catnum->answers)) {
			G_fatal_error ("setting constant color [%s]!",
					line_color->answer);
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

        Vect_set_constraint_region (&Map, window.north, window.south,
                                    window.east, window.west);

	nlines = V2_num_lines (&Map);

	/* Do fill */
	fprintf (stderr, "drawing lines ... ");
	fflush (stderr);
	G_percent (0, nlines, 5);
	for (i = 1; i <= nlines; i++) {
		
		G_percent (i-1, nlines, 5);
		if ((dig_att = V2_line_att(&Map, i)) == 0)
			continue;
		
		V2_get_line_bbox (&Map, i, &N, &S, &E, &W);
		if (S > window.north || N < window.south ||
		    W > window.east  || E < window.west)
			continue;
	
		if (!select_color (dig_att))
			continue;
		
		V2_read_line (&Map, points, i);

		plot_line (points);
	
	}
        G_percent (nlines, nlines, 5);

	if (leg_opt->answer && !strcmp(leg_opt->answer, "-")) {
		/* use temporary legend file for future redraws */
		tmpfile = G_tempfile();
		if (write_legend (tmpfile)) {
			len = strlen(argv[0]) + strlen(tmpfile)
			      + strlen(mapset) + strlen(map->answer)
			      + strlen(" map=@ legend= ");
			cmd_buff = G_malloc (len);
			sprintf (cmd_buff, "%s map=%s@%s legend=%s",
					argv[0], map->answer, mapset,
					tmpfile);
			D_add_to_list (cmd_buff);
			D_set_dig_name (G_fully_qualified_name (
						map->answer, mapset));
			D_add_to_dig_list (G_fully_qualified_name (
						map->answer, mapset));
		}
	}
	else {
		D_add_to_list(G_recreate_command()) ;

		D_set_dig_name(G_fully_qualified_name(
				map->answer, mapset));
		D_add_to_dig_list(G_fully_qualified_name(
				map->answer, mapset));
	}
	
	R_close_driver();

	return 0;
}

