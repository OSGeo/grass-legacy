
/****************************************************************************
 *
 * MODULE:       r.surf.contour
 * AUTHOR(S):    Chuck Ehlschlaeger (original contributor)
 *               Markus Neteler <neteler itc.it>,
 *               Bernhard Reiter <bernhard intevation.de>, 
 *               Brad Douglas <rez touchofmadness.com>, 
 *               Huidae Cho <grass4u gmail.com>, 
 *               Glynn Clements <glynn gclements.plus.com>, 
 *               Hamish Bowman <hamish_b yahoo.com>, 
 *               Jan-Oliver Wagner <jan intevation.de>,
 *               Markus Metz
 * PURPOSE:      interpolates a raster elevation map from a rasterized
 *               contour map
 * COPYRIGHT:    (C) 1999-2006 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#define MAIN
#include "contour.h"
#include <unistd.h>
#undef MAIN
#include <grass/gis.h>
#include <grass/glocale.h>

int main(int argc, char *argv[])
{
    int r, c;
    DCELL con1, con2;
    double d1, d2;
    DCELL *alt_row;
    char *con_name, *alt_name, *con_mapset;
    int file_fd;
    int fast_mode, n_segments, s_size;
    DCELL value;
    char mask_value;
    struct History history;
    struct GModule *module;
    struct Flag *flag1, *flag_slow;
    struct Option *opt1, *opt2;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("raster, interpolation");
    module->description =
	_("Surface generation program from rasterized contours.");

    opt1 = G_define_option();
    opt1->key = "input";
    opt1->type = TYPE_STRING;
    opt1->required = YES;
    opt1->gisprompt = "old,cell,raster";
    opt1->description = _("Name of existing raster map containing contours");

    opt2 = G_define_option();
    opt2->key = "output";
    opt2->type = TYPE_STRING;
    opt2->required = YES;
    opt2->gisprompt = "new,cell,raster";
    opt2->description = _("Output elevation raster map");

    flag1 = G_define_flag();
    flag1->key = 'f';
    flag1->description = _("Unused; retained for compatibility purposes, "
			   "will be removed in future");

    flag_slow = G_define_flag();
    flag_slow->key = 's';
    flag_slow->description = _("Invoke slow, but memory frugal operation "
			       "(generally not needed, will be removed in future)");

    on = 1;
    off = 0;

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    con_name = opt1->answer;
    alt_name = opt2->answer;

    if (flag_slow->answer)
	fast_mode = 0;
    else
	fast_mode = 1;


    con_mapset = G_find_cell2(con_name, "");
    if (!con_mapset)
	G_fatal_error("Contour raster map [%s] not found", con_name);

    nrows = G_window_rows();
    ncols = G_window_cols();
    i_val_l_f = nrows + ncols;
    s_size = 128;
    n_segments = nrows / s_size + (nrows % s_size > 0) * s_size + 
                 ncols / s_size + (ncols % s_size > 0) * s_size;
    /* limit memory consumption to about 100 MB */
    if (n_segments > 800)
	n_segments = 800;
    G_debug(1, "n_segments: %d", n_segments);
    dseg_open(&con, s_size, s_size, n_segments);
    dseg_read_cell(&con, con_name, con_mapset);
    alt_row = (DCELL *) G_malloc(ncols * sizeof(DCELL));
    if (fast_mode) {
	seen = flag_create(nrows, ncols);
	mask = flag_create(nrows, ncols);
    }
    else {
	bseg_open(&bseen, s_size, s_size, n_segments);
	bseg_open(&bmask, s_size, s_size, n_segments);
    }
    if (NULL != G_find_file("cell", "MASK", G_mapset())) {
	if ((file_fd = G_open_cell_old("MASK", G_mapset())) < 0)
	    G_fatal_error("Unable to open MASK");
	if (fast_mode) {
	    for (r = 0; r < nrows; r++) {
		G_get_d_raster_row_nomask(file_fd, alt_row, r);
		for (c = 0; c < ncols; c++)
		    if (G_is_d_null_value(&(alt_row[c])) || alt_row[c] == 0)
			FLAG_SET(mask, r, c);
		    else
			FLAG_UNSET(mask, r, c);
	    }
	}
	else {
	    for (r = 0; r < nrows; r++) {
		G_get_d_raster_row_nomask(file_fd, alt_row, r);
		for (c = 0; c < ncols; c++)
		    if (G_is_d_null_value(&(alt_row[c])) || alt_row[c] == 0)
			bseg_put(&bmask, &off, r, c);
	    }
	}
	G_close_cell(file_fd);
    }
    zero = (NODE *) G_malloc(INIT_AR * sizeof(NODE));
    minc = minr = 0;
    maxc = ncols - 1;
    maxr = nrows - 1;
    array_size = INIT_AR;
    file_fd = G_open_raster_new(alt_name, DCELL_TYPE);
    if (!file_fd)
	G_fatal_error("Unable to open output map");
    for (r = 0; r < nrows; r++) {
	G_percent(r, nrows, 1);
	G_set_d_null_value(alt_row, ncols);
	for (c = 0; c < ncols; c++) {
	    if (fast_mode) {
		if (FLAG_GET(mask, r, c))
		    continue;
	    }
	    else {
		bseg_get(&bmask, &mask_value, r, c);
		if (mask_value)
		    continue;
	    }
	    dseg_get(&con, r, c, &value);
	    if (!G_is_d_null_value(&value)) {
		alt_row[c] = value;
		continue;
	    }
	    if (fast_mode)
		find_con(r, c, &d1, &d2, &con1, &con2);
	    else
		find_con_slow(r, c, &d1, &d2, &con1, &con2);
	    if (!G_is_d_null_value(&con2))
		alt_row[c] = d2 * con1 / (d1 + d2) +
			     d1 * con2 / (d1 + d2);
	    else
		alt_row[c] = con1;
	}
	G_put_raster_row(file_fd, alt_row, DCELL_TYPE);
    }
    G_percent(r, nrows, 1);
    dseg_close(&con);
    if (fast_mode) {
	flag_destroy(seen);
	flag_destroy(mask);
    }
    else {
	bseg_close(&bseen);
	bseg_close(&bmask);
    }
    G_close_cell(file_fd);

    G_short_history(alt_name, "raster", &history);
    G_command_history(&history);
    G_write_history(alt_name, &history);

    exit(EXIT_SUCCESS);
}
