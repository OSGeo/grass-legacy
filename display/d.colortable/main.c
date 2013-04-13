
/****************************************************************************
 *
 * MODULE:       d.colortable
 * AUTHOR(S):    James Westervelt, CERL (original contributor)
 *               Markus Neteler <neteler itc.it>,
 *               Bernhard Reiter <bernhard intevation.de>, 
 *               Eric G. Miller <egm2 jps.net>, 
 *               Glynn Clements <glynn gclements.plus.com>, 
 *               Hamish Bowman <hamish_b yahoo.com>, 
 *               Jan-Oliver Wagner <jan intevation.de>
 * PURPOSE:      display the color table associated with a raster map layer in
 *               the active frame on the graphics monitor
 * COPYRIGHT:    (C) 1999-2009 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/


#include <stdlib.h>
#include <math.h>
#include <grass/display.h>
#include <grass/gis.h>
#include <grass/raster.h>
#include <grass/glocale.h>

int main(int argc, char **argv)
{
    char *map_name;
    int color;
    int lines;
    int cols;

    char window_name[64];
    char *mapset;
    struct FPRange fp_range;
    struct Colors colors;
    double ratio;
    DCELL dmin, dmax, dval;
    int cats_num;
    int cur_dot_row, cur_dot_col;
    int dots_per_line, dots_per_col;
    int atcat;
    int white, black;
    int atcol, atline;
    int count, offset;
    int t, b, l, r;
    int fp, new_colr;
    int x_box[5], y_box[5];

    struct GModule *module;
    struct Option *opt1, *opt2, *opt3, *opt4;
    struct Flag *skip_null;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("display, raster");
    module->description =
	_("Displays the color table associated with a raster map layer.");

    opt1 = G_define_standard_option(G_OPT_R_MAP);
    opt1->description =
	_("Name of raster map whose color table is to be displayed");

    opt2 = G_define_option();
    opt2->key = "color";
    opt2->type = TYPE_STRING;
    opt2->answer = DEFAULT_BG_COLOR;
    opt2->options = D_color_list();
    opt2->description =
	_("Color of lines separating the colors of the color table");

    opt3 = G_define_option();
    opt3->key = "lines";
    opt3->type = TYPE_INTEGER;
    opt3->options = "1-1000";
    opt3->description = _("Number of lines to appear in the color table");

    opt4 = G_define_option();
    opt4->key = "cols";
    opt4->type = TYPE_INTEGER;
    opt4->options = "1-1000";
    opt4->description = _("Number of columns to appear in the color table");

    skip_null = G_define_flag();
    skip_null->key = 'n';
    skip_null->description =
	_("Don't draw a collar showing the NULL color in FP maps");

    /* Check command line */
    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    map_name = opt1->answer;
    mapset = G_find_cell2(map_name, "");
    if (mapset == NULL)
	G_fatal_error(_("Raster map <%s> not found"), map_name);

    fp = G_raster_map_is_fp(map_name, mapset);

    if (opt2->answer != NULL) {
	new_colr = D_translate_color(opt2->answer);
	color = new_colr;
    }

    if (fp)
	lines = 1;
    else
	lines = 0;

    if (opt3->answer != NULL) {
	if (fp)
	    G_warning(_("<%s> is a floating point map. "
			"Ignoring [lines] and drawing continuous color ramp"),
		      map_name);
	else
	    sscanf(opt3->answer, "%d", &lines);
    }

    if (fp)
	cols = 1;
    else
	cols = 0;

    if (opt4->answer != NULL) {
	if (fp)
	    G_warning(_("<%s> is a floating point map. "
			"Ignoring [cols] and drawing continuous color ramp"),
		      map_name);
	else
	    sscanf(opt4->answer, "%d", &cols);
    }

    /* Make sure map is available */
    if (G_read_colors(map_name, mapset, &colors) == -1)
	G_fatal_error(_("R_color file for [%s] not available"), map_name);
    if (G_read_fp_range(map_name, mapset, &fp_range) == -1)
	G_fatal_error(_("Range file for [%s] not available"), map_name);
    if (R_open_driver() != 0)
	G_fatal_error(_("No graphics device selected"));
    if (D_get_cur_wind(window_name))
	G_fatal_error(_("No current frame"));
    if (D_set_cur_wind(window_name))
	G_fatal_error(_("Current frame not available"));

    /* Figure out where to put boxes */
    D_get_screen_window(&t, &b, &l, &r);

    G_get_fp_range_min_max(&fp_range, &dmin, &dmax);
    if (G_is_d_null_value(&dmin) || G_is_d_null_value(&dmax))
	G_fatal_error(_("Data range is empty"));

    cats_num = (int)dmax - (int)dmin + 1;

    if (lines <= 0 && cols <= 0) {
	double dx, dy;

	dy = (double)(b - t);
	dx = (double)(r - l);
	ratio = dy / dx;
	cols = 1 + sqrt((dmax - dmin + 1.) / ratio);
	lines = 1 + cats_num / cols;
    }
    else if (lines > 0 && cols <= 0) {
	cols = 1 + cats_num / lines;
    }
    else if (cols > 0 && lines <= 0) {
	lines = 1 + cats_num / cols;
    }
    /* otherwise, accept without complaint what the user requests
     * It is possible that the number of lines and cols is not
     * sufficient for the number of categories.
     */

    dots_per_line = (b - t) / lines;
    dots_per_col = (r - l) / cols;

    x_box[0] = 0;
    y_box[0] = 0;
    x_box[1] = 0;
    y_box[1] = (5 - dots_per_line);
    x_box[2] = (dots_per_col - 5);
    y_box[2] = 0;
    x_box[3] = 0;
    y_box[3] = (dots_per_line - 5);
    x_box[4] = (5 - dots_per_col);
    y_box[4] = 0;

    white = D_translate_color("white");
    black = D_translate_color("black");
    G_set_c_null_value(&atcat, 1);

    if (!fp) {
	for (atcol = 0; atcol < cols; atcol++) {
	    cur_dot_row = t;
	    cur_dot_col = l + atcol * dots_per_col;
	    count = 0;
	    for (atline = 0; atline < lines; atline++) {
		cur_dot_row += dots_per_line;
		/* Draw outer border box */
		R_standard_color(color);
		R_move_abs(cur_dot_col + 2, (cur_dot_row - 1));
		R_cont_rel(0, (2 - dots_per_line));
		R_cont_rel((dots_per_col - 2), 0);
		R_cont_rel(0, (dots_per_line - 2));
		R_cont_rel((2 - dots_per_col), 0);
		/* Draw black box */
		R_standard_color(black);
		R_move_abs(cur_dot_col + 3, (cur_dot_row - 2));
		R_cont_rel(0, (4 - dots_per_line));
		R_cont_rel((dots_per_col - 4), 0);
		R_cont_rel(0, (dots_per_line - 4));
		R_cont_rel((4 - dots_per_col), 0);
		/* Color box */
		D_color((CELL) atcat, &colors);
		R_move_abs(cur_dot_col + 4, (cur_dot_row - 2));
		R_polygon_rel(x_box, y_box, 5);

		count++;
		/* first cat number is null value */
		if (count == 1)
		    atcat = (int)dmin;
		else if (++atcat > (int)dmax)
		    break;
	    }
	    if (atcat > (int)dmax)
		break;
	} /* col loop */
    } /* int map */

    else {
	/*** draw continuous color ramp for fp map ***/

	cur_dot_row = t + dots_per_line;
	cur_dot_col = l;

	/* Draw outer border box */
	R_standard_color(color);
	R_move_abs(cur_dot_col + 1, (cur_dot_row - 3));
	R_cont_rel(0, (4 - dots_per_line));
	R_cont_rel((dots_per_col - 3), 0);
	R_cont_rel(0, (dots_per_line - 3));
	R_cont_rel((3 - dots_per_col), 0);
	/* Draw black box */
	R_standard_color(black);
	R_move_abs(cur_dot_col + 2, (cur_dot_row - 3));
	R_cont_rel(0, (5 - dots_per_line));
	R_cont_rel((dots_per_col - 5), 0);
	R_cont_rel(0, (dots_per_line - 5));
	R_cont_rel((5 - dots_per_col), 0);

	/* Color ramp box */
	  /* get separate color for each pixel */
	  /* fisrt 5 pixels draw null color */
	y_box[1] = -1;
	y_box[3] = 1;
	x_box[2] = (dots_per_col - 6);
	x_box[4] = (6 - dots_per_col);

	G_debug(1, "dots_per_line: %d  dmin=%.2f dmax=%.2f",
		dots_per_line, dmin, dmax);

	if (skip_null->answer)
	    offset = 1;
	else
	    offset = 4;

	for (r = 0; r < dots_per_line - 6; r++) {
	    if ((r <= 4) && !skip_null->answer)
		G_set_d_null_value(&dval, 1);
	    else
		dval =
		    dmin +  r*(dmax - dmin) / (dots_per_line - 6 - offset);

	    D_d_color(dval, &colors);
	    R_move_abs(cur_dot_col + 3, (cur_dot_row - 3) - r);
	    R_polygon_rel(x_box, y_box, 5);
	}
    }

    D_add_to_list(G_recreate_command());

    R_close_driver();

    exit(EXIT_SUCCESS);
}
