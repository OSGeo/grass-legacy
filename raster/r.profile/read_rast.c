/*
 * $Id$
 *
 * Copyright (C) 2000 by the GRASS Development Team
 * Author: Bob Covill <bcovill@tekmap.ns.ca>
 * 
 * This Program is free software under the GPL (>=v2)
 * Read the file COPYING coming with GRASS for details
 *
 */

#include <grass/gis.h>
#include <grass/glocale.h>
#include "local_proto.h"

int read_rast(double east, double north, double dist, int fd, int coords, 
		RASTER_MAP_TYPE data_type, FILE * fp, char *null_string)
{
    int row, col, nrows, ncols;
    struct Cell_head window;
    CELL *cell;
    char buf[1024] = "";
    char cbuf[80];
    int red, green, blue;
    FCELL *fcell;
    DCELL *dcell;

    G_get_window(&window);
    nrows = window.rows;
    ncols = window.cols;

    row = (window.north - north) / window.ns_res;
    col = (east - window.west) / window.ew_res;
    G_debug(4, "row=%d:%d  col=%d:%d", row, nrows, col, ncols);

    if ( (row < 0) || (row >= nrows) || (col < 0) || (col >= ncols) )
	G_fatal_error(_("Coordinate request outsite current region settings"));

    if (data_type == CELL_TYPE) {
	cell = G_allocate_c_raster_buf();
	if (G_get_c_raster_row(fd, cell, row) < 0)
	    G_fatal_error(_("Reading raster map row %d"), row);

	if (G_is_c_null_value(&cell[col]))
	    sprintf(buf, null_string);
	else
	    sprintf(buf, "%d", cell[col]);

	if (clr) {
		G_get_c_raster_color(&cell[col], &red, &green, &blue, &colors);
		sprintf(cbuf, " %03d:%03d:%03d", red, green, blue);
		strcat(buf, cbuf);
	}

	if (coords == 1)
	    fprintf(fp, "%f %f %f %s\n", east, north, dist, buf);
	else
	    fprintf(fp, "%f %s\n", dist, buf);
    }

    if (data_type == FCELL_TYPE) {
	fcell = G_allocate_f_raster_buf();
	if (G_get_f_raster_row(fd, fcell, row) < 0)
	    G_fatal_error(_("Reading raster map row %d"), row);

	if (G_is_f_null_value(&fcell[col]))
	    sprintf(buf, null_string);
	else
	    sprintf(buf, "%f", fcell[col]);

	if (clr) {
		G_get_f_raster_color(&fcell[col], &red, &green, &blue, &colors);
		sprintf(cbuf, " %03d:%03d:%03d", red, green, blue);
		strcat(buf, cbuf);
	}

	if (coords == 1)
	    fprintf(fp, "%f %f %f %s\n", east, north, dist, buf);
	else
	    fprintf(fp, "%f %s\n", dist, buf);
    }

    if (data_type == DCELL_TYPE) {
	dcell = G_allocate_d_raster_buf();
	if (G_get_d_raster_row(fd, dcell, row) < 0)
	    G_fatal_error(_("Reading raster map row %d"), row);
    
	if (G_is_d_null_value(&dcell[col]))
	    sprintf(buf, null_string);
	else
	    sprintf(buf, "%f", dcell[col]);

	if (clr) {
		G_get_d_raster_color(&dcell[col], &red, &green, &blue, &colors);
		sprintf(cbuf, " %03d:%03d:%03d", red, green, blue);
		strcat(buf, cbuf);
	}

	if (coords == 1)
	    fprintf(fp, "%f %f %f %s\n", east, north, dist, buf);
	else
	    fprintf(fp, "%f %s\n", dist, buf);
    }

    return 0;
}
