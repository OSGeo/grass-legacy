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
#include <time.h>
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
	struct rgb_color *area, *line;
};

enum line_or_area {DRAW_LINE, DRAW_AREA}; 

/* GLOBALS */
static struct Cell_head window;
static struct avl_table *cat_tbl = NULL;
static struct avl_table *clr_tbl = NULL;
static struct rgb_color *def_area_color = NULL;
static struct rgb_color *def_line_color = NULL;
static int  (*select_color) (const int, enum line_or_area) = 0;
static int std_clr_area = 0;
static int std_clr_line = 0;
static const int palette_ncolors = 16;
static struct rgb_color palette[16] =  {
	{198, 198, 198}, /*  1: light gray */
	{127, 127, 127}, /*  2: medium/dark gray */
	{255,   0,   0}, /*  3: bright red */
	{139,   0,   0}, /*  4: dark red */
	{  0, 255,   0}, /*  5: bright green */
	{  0, 139,   0}, /*  6: dark green */
	{  0,   0, 255}, /*  7: bright blue */
	{  0,   0, 139}, /*  8: dark blue   */
	{255, 255,   0}, /*  9: yellow */
	{139, 126,  10}, /* 10: olivey brown */
	{255, 165,   0}, /* 11: orange */
	{255, 192, 203}, /* 12: pink   */
	{255,   0, 255}, /* 13: magenta */
	{139,   0, 139}, /* 14: dark magenta */
	{  0, 255, 255}, /* 15: cyan */
	{  0, 139, 139}  /* 16: dark cyan */
};

/* Local PROTOS */
static struct cat_color *new_cat_color(void);
static struct rgb_color *new_rgb_color(void);
static void destroy_cat_color(void *item, void *data);
static void destroy_rgb_color(void *item, void *data);
static int cat_comp(const void *a, const void *b, void *data);
static int rgb_comp(const void *a, const void *b, void *data);
static int legend_color_func(const int cat, enum line_or_area which);
static int random_color_func(const int cat, enum line_or_area la);
static int const_color_func(const int cat, enum line_or_area which);
static struct rgb_color *
add_color_to_table(unsigned char R, unsigned char G, unsigned char B);
static void 
add_cat_to_table(int cat, struct rgb_color *area, struct rgb_color *line);
static void destroy_cat_tbl(void);
static void destroy_rgb_tbl(void);
static int rgb_ok(int red, int green, int blue);
static int use_legend_file(const char *filename);
static int rgb_parse(const char *clr_spec, 
		unsigned char *red, unsigned char *green, unsigned char *blue);
static int use_const_color(const char *area_clr, const char *line_clr);
static int use_random_color(void);
static int write_legend(const char *filename);
static void adjust_point(double *x, double *y, 
		double dx, double dy, unsigned char code);
static int clip_segment(double *x0, double *y0, double *x1, double *y1);
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


static int legend_color_func (const int cat, enum line_or_area which)
{
	int status = 0;
	const int colornum = MAXCOLORS + 1;
	struct cat_color *self = NULL;
	struct rgb_color *clr = NULL;
	
	if (cat_tbl) {
		self = avl_find (cat_tbl, &cat);
		switch (which) {
			case DRAW_AREA:
				if (self && self->area) {
					clr = self->area;
					status = 1;
				}
				else if (def_area_color) {
					clr = def_area_color;
					status = 1;
				}
				else { 
					status = 0;
				}
				break;
			case DRAW_LINE:
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
				break;
			default:
				break; /* never reached */
		}
	}
	
	if (status) {
		R_reset_color (clr->R, clr->G, clr->B, colornum);
		R_color (colornum);
	}			

	return status;
}

static int random_color_func (const int cat, enum line_or_area la)
{
	unsigned char which; 
	const int colornum = MAXCOLORS + 1;

	switch (la) {
		case DRAW_AREA:
			which = (rand() % palette_ncolors);
			R_reset_color (palette[which].R, 
					palette[which].G, 
					palette[which].B, 
					colornum);
			break;
		case DRAW_LINE:
			R_reset_color (0, 0, 0, colornum);
			break;
		default:
			break; /* never reached */
	}
	R_color (colornum);

	return 1;
}


static int const_color_func (const int cat, enum line_or_area which)
{
	int colornum = MAXCOLORS + 1;
	int status = 0;
	
	switch (which) {
		case DRAW_AREA:
			if (std_clr_area) {
				R_standard_color (std_clr_area);
				status = 1;
			}
			else if (def_area_color) {
				R_reset_color (def_area_color->R,
						def_area_color->G,
						def_area_color->B,
						colornum);
				R_color (colornum);
				status = 1;
			}
			break;
		case DRAW_LINE:
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
			break;
		default:
			break; /* never reached */
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
	
static void add_cat_to_table (int cat, 
			      struct rgb_color *area,
			      struct rgb_color *line)
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
	self->area = area;
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
	const char *format = "%d %d %d %d %d %d %d";
	int status = 0;
	int conversions = 0;
	int cat;
	int line_cnt = 0;
	long red1, green1,blue1, red2, green2, blue2;
	struct rgb_color *area;
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
					&red1, &green1, &blue1,
					&red2, &green2, &blue2);
			if (conversions < 1)
				continue;
			else if (!(conversions == 4 || conversions == 7)) {
				G_warning ("ignoring line [%d] in legend file!", line_cnt);
				continue;
			}

			if (rgb_ok (red1, green1, blue1)) {
				area = add_color_to_table ( (unsigned char) red1,
						            (unsigned char) green1,
							    (unsigned char) blue1 );
			}
			else {
				G_warning ("bad color spec on line [%d], skipping",
						line_cnt);
				continue;
			}

			if (conversions == 7) {
				if (rgb_ok (red2, green2, blue2)) {
					line = add_color_to_table (
							(unsigned char) red2,
							(unsigned char) green2,
							(unsigned char) blue2);
				}
				else {
					G_warning ("bad color spec on line [%d], skipping");
					continue;
				}
			}
			else
			{
				line = NULL;
			}
			
			if (cat < 1) {
				/* defaults? */
				if (def_area_color == NULL) {
					def_area_color = area;
				}
				else {
					G_warning ("duplicate default color on line [%d]",
							line_cnt);
					destroy_rgb_color (area, NULL);
					destroy_rgb_color (line, NULL);
					continue;
				}
				if (def_line_color == NULL) {
					def_line_color = line;
				}
				else {
					destroy_rgb_color (line, NULL);
				}
			}
			else {
				add_cat_to_table (cat, area, line);
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

static int use_const_color (const char *area_clr, const char *line_clr)
{
	int std_color, status = 0;
	unsigned char red, green, blue;
	
	std_color = D_translate_color (area_clr);
	if (std_color) {
		std_clr_area = std_color;
		status = 1;
	}
	else if (rgb_parse (area_clr, &red, &green, &blue)) {
		def_area_color = new_rgb_color();
		def_area_color->R = red;
		def_area_color->G = green;
		def_area_color->B = blue;
		status = 1;
	}
	else
	{
		G_warning ("parsing RGB colors %s", area_clr);
	}
	if (status && line_clr) {
		if ((std_color = D_translate_color (line_clr))) {
			std_clr_line = std_color;
		}
		else if (rgb_parse (line_clr, &red, &green, &blue)) {
			def_line_color = new_rgb_color();
			def_line_color->R = red;
			def_line_color->G = green;
			def_line_color->B = blue;
		}
		else {
			G_warning ("parsing RGB colors %s", line_clr);
		}
	}
	
	if (status)
		select_color = const_color_func;

	return status;
}

static int use_random_color (void)
{
	srand (time (0));
	select_color = random_color_func;
	return 1;
}

static int write_legend (const char *filename)
{
	FILE *ofp;
	const char *fmt4 = "%d %d %d %d\n";
	const char *fmt7 = "%d %d %d %d %d %d %d\n";
	struct avl_traverser trav;
	struct rgb_color *area, *line;
	struct cat_color *cat;
	int status = 0;

	if ((ofp = fopen (filename, "w"))) {
		/* write default first, if any */
		if (def_area_color) {
			area = def_area_color;
			if (def_line_color) {
				line = def_line_color;
				fprintf (ofp, fmt7, -1,
					 area->R, area->G, area->B,
					 line->R, line->G, line->B);
			}
			else {
				fprintf (ofp, fmt4, -1,
					 area->R, area->G, area->B);
			}
		}
					
		/* traverse cat tree */
		avl_t_init (&trav, cat_tbl);
		avl_t_first (&trav, cat_tbl);
		while ((cat = avl_t_cur (&trav))) {
			area = cat->area;
			line = cat->line;

			if (area && line) {
				fprintf (ofp, fmt7, cat->cat,
					 area->R, area->G, area->B,
					 line->R, line->G, line->B);
			}
			else {
				fprintf (ofp, fmt4, cat->cat,
					 area->R, area->G, area->B);
			}
			avl_t_next (&trav);
		}
		fclose (ofp);
		status = 1;
	}

	return status;
}

/* end of cat_color management */


static void
adjust_point (double *x, double *y, double dx, double dy, unsigned char code)
{
    /* supports the clip_segment() routine below */
    if (code & 8) /* to the left */
    {
        *y += (window.west - *x) * dy / dx;
        *x  = window.west;
    }
    else if (code & 2) /* to the right */
    {
        *y += (window.east - *x) * dy / dx;
        *x  = window.east;
    }
    else if (code & 1) /* to the south */
    {
        *x += (window.south - *y) * dx / dy;
        *y  = window.south;
    }
    else if (code & 4) /* to the north */
    {
        *x += (window.north - *y) * dx / dy;
        *y  = window.north;
    }
    else /* shouldn't happen, but makes compiler happy */
        return;
}


static int
clip_segment (double *x0, double *y0, double *x1, double *y1)
{
    /* Using Cohen-Sutherland'ish algorithm */
    unsigned char code1, code2;
    double dx, dy;
    
    /* loop will iterate at most four times */
    do {
        
        code1 = code2 = 0;  /* always initialize code word */
        
        if (*x0 < window.west)  code1 |= 8;  /* left */
        if (*y0 > window.north) code1 |= 4;  /* above */
        if (*x0 > window.east)  code1 |= 2;  /* right */
        if (*y0 < window.south) code1 |= 1;  /* below */

        if (*x1 < window.west)  code2 |= 8;
        if (*y1 > window.north) code2 |= 4;
        if (*x1 > window.east)  code2 |= 2;
        if (*y1 < window.south) code2 |= 1;

        dx = *x1 - *x0;     /* always recompute deltas */
        dy = *y1 - *y0;

        /* Eventually one of these two conditions will be true */
        if ((code1 | code2) == 0) /* trivial accept */
            return 1;
        if ((code1 & code2) > 0)  /* trivial reject */
            return 0;

        if (code1) /* point one is outside */
            adjust_point (x0, y0, dx, dy, code1);
        else /* point two is outside */
            adjust_point (x1, y1, dx, dy, code2);
    
    } while (1);
}

static void plot_line (struct line_pnts *points)
{
	int i;
	double x[2], y[2];
	
	for (i = 0; i < points->n_points - 1; i++) {
		x[0] = points->x[i];
		y[0] = points->y[i];
		x[1] = points->x[i+1];
		y[1] = points->y[i+1];

		if (clip_segment (&x[0], &y[0], &x[1], &y[1])) {
			G_plot_line (x[0], y[0], x[1], y[1]);
		}
	}
}


int main (int argc, char **argv)
{
	char wind_name[80];
	int  color_number = 0, backgroundcolor = 0;
	char colorname[30];
	char *tmpfile;
	char *cmd_buff;
	int  len;
	double N, S, E, W;
	struct line_pnts *points;
	char *mapset;
	double **xs, **ys;
	int rings;
	int *rpnts;
	struct Map_info Map;
	struct Option *map;
	struct Option *color_const;
	struct Option *line_color;
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
	rand_colr->description  = "Use random colors";
	
	map		    	= G_define_option();
	map->description	= "Name of existing vector map to draw";
	map->key		= "map";
	map->type		= TYPE_STRING;
	map->required		= YES;
	map->gisprompt		= "old,dig,vector";

	color_const		= G_define_option();
	color_const->description = "Draw areas using either a GRASS standard color or "\
				   "R:G:B triplet (separated by colons)";
	color_const->key	= "color";
	color_const->required   = NO;
	color_const->type	= TYPE_STRING;
	color_const->answer     = "white";

	line_color		= G_define_option();
	line_color->key		= "linecolor";
	line_color->description = "Draw area boundaries using either a GRASS standard "\
				  "color or R:G:B triplet (separated by colons)";
	line_color->type	= TYPE_STRING;
	line_color->required	= NO;
	line_color->answer	= "black";
	
	leg_opt			= G_define_option();
	leg_opt->description    = "Draw areas according to a legend file "\
		                  "(<cat> <R> <G> <B> [<R> <G> <B>])";
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
		if (!use_const_color (color_const->answer, line_color->answer)) {
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

	/* Do fill */
	fprintf (stderr, "filling areas ... ");
	fflush (stderr);
	G_percent (0, nareas, 5);
	for (i = 1; i <= nareas; i++) {
		
		if ((dig_att = V2_area_att(&Map, i)) == 0)
			continue;
		
		V2_get_area_bbox (&Map, i, &N, &S, &E, &W);
		if (S > window.north || N < window.south ||
		    W > window.east  || E < window.west)
			continue;
	
		if (!select_color (dig_att, DRAW_AREA))
			continue;
		
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
	
	Vect_rewind (&Map);
	/* do boundary lines */
	fprintf (stderr, "drawing boundaries ... ");
	fflush (stderr);
	G_percent (0, nareas, 5);
	for (i = 1; i <= nareas ; i++) {

		if ((dig_att = V2_area_att(&Map, i)) == 0)
			continue;
		
		V2_get_area_bbox (&Map, i, &N, &S, &E, &W);
		if (S > window.north || N < window.south ||
		    W > window.east  || E < window.west)
			continue;
	
		if (!select_color (dig_att, DRAW_LINE))
			continue;
		
		V2_get_area (&Map, i, &pa);

		Vect_get_area_points (&Map, i, points);
		
		/* exterior */
		plot_line (points);
	
		/* interiors */
		for (j = 0; j < pa->n_isles; j++) {
			Vect_get_isle_points (&Map, pa->isles[j], points);
			plot_line (points);
		}
		
		G_percent (i, nareas, 5);
	}

	putchar('\n');

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

