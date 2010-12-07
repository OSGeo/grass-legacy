/* calculate camera angle to local surface
 * 90 degrees: orthogonal to local surface
 * 0 degress: parallel to local surface
 * < 0 degrees: not visible by camera
 * 
 * Earth curvature is not considered, assuming that the extends of the 
 * imagery to be orthorectified are rather small
 * Shadowing effects by ridges and peaks are not considered */

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include "global.h"

int camera_angle(struct cache *ebuffer, char *name)
{
    int row, col, nrows, ncols;
    double XC = group.XC;
    double YC = group.YC;
    double ZC = group.ZC;
    double c_angle, c_angle_min, c_alt, c_az, slope, aspect;
    double radians_to_degrees = 180.0 / M_PI;
    /* double degrees_to_radians = M_PI / 180.0; */
    DCELL *ep;
    DCELL e1, e2, e3, e4, e5, e6, e7, e8, e9;
    double factor, V, H, dx, dy, dz, key;
    double north, south, east, west, ns_med;
    FCELL *fbuf;
    int outfd;
    struct Colors colr;
    FCELL clr_min, clr_max;
    struct History hist;
    char *type;

    G_message(_("Calculating camera angle to local surface..."));
    
    select_target_env();
    
    nrows = target_window.rows;
    ncols = target_window.cols;
    
    outfd = G_open_raster_new(name, FCELL_TYPE);
    fbuf = G_allocate_raster_buf(FCELL_TYPE);
    
    /* give warning if location units are different from meters and zfactor=1 */
    factor = G_database_units_to_meters_factor();
    if (factor != 1.0)
	G_warning(_("Converting units to meters, factor=%.6f"), factor);

    G_begin_distance_calculations();
    north = G_row_to_northing(0.5, &target_window);
    ns_med = G_row_to_northing(1.5, &target_window);
    south = G_row_to_northing(2.5, &target_window);
    east = G_col_to_easting(2.5, &target_window);
    west = G_col_to_easting(0.5, &target_window);
    V = G_distance(east, north, east, south) * 4;
    H = G_distance(east, ns_med, west, ns_med) * 4;
    
    c_angle_min = 90;
    for (row = 0; row < nrows; row++) {
	G_percent(row, nrows, 2);
	
	G_set_null_value(fbuf, ncols, FCELL_TYPE);

	if (row == 0 || row == nrows - 1) {
	    G_put_raster_row(outfd, fbuf, FCELL_TYPE);
	    continue;
	}

	north = G_row_to_northing(row + 0.5, &target_window);

	for (col = 1; col < ncols - 1; col++) {
	    
	    ep = CPTR(ebuffer, row - 1, col - 1);
	    if (G_is_d_null_value(ep))
		continue;
	    e1 = *ep;
	    ep = CPTR(ebuffer, row - 1, col);
	    if (G_is_d_null_value(ep))
		continue;
	    e2 = *ep;
	    ep = CPTR(ebuffer, row - 1, col + 1);
	    if (G_is_d_null_value(ep))
		continue;
	    e3 = *ep;
	    ep = CPTR(ebuffer, row, col - 1);
	    if (G_is_d_null_value(ep))
		continue;
	    e4 = *ep;
	    ep = CPTR(ebuffer, row, col);
	    if (G_is_d_null_value(ep))
		continue;
	    e5 = *ep;
	    ep = CPTR(ebuffer, row, col + 1);
	    if (G_is_d_null_value(ep))
		continue;
	    e6 = *ep;
	    ep = CPTR(ebuffer, row + 1, col - 1);
	    if (G_is_d_null_value(ep))
		continue;
	    e7 = *ep;
	    ep = CPTR(ebuffer, row + 1, col);
	    if (G_is_d_null_value(ep))
		continue;
	    e8 = *ep;
	    ep = CPTR(ebuffer, row + 1, col + 1);
	    if (G_is_d_null_value(ep))
		continue;
	    e9 = *ep;
	    
	    dx = ((e1 + e4 + e4 + e7) - (e3 + e6 + e6 + e9)) / H;
	    dy = ((e7 + e8 + e8 + e9) - (e1 + e2 + e2 + e3)) / V;
	    
	    /* compute topographic parameters */
	    key = dx * dx + dy * dy;
	    /* slope in radians */
	    slope = atan(sqrt(key));

	    /* aspect in radians */
	    if (key == 0.)
		aspect = 0.;
	    else if (dx == 0) {
		if (dy > 0)
		    aspect = M_PI / 2;
		else
		    aspect = 1.5 * M_PI;
	    }
	    else {
		aspect = atan2(dy, dx);
		if (aspect <= 0.)
		    aspect = 2 * M_PI + aspect;
	    }
	    
	    /* camera altitude angle in radians */
	    east = G_col_to_easting(col + 0.5, &target_window);
	    dx = east - XC;
	    dy = north - YC;
	    dz = ZC - e5;
	    c_alt = atan(sqrt(dx * dx + dy * dy) / dz);

	    /* camera azimuth angle in radians */
	    c_az = atan(dy / dx);
	    if (east < XC && north != YC)
		c_az += M_PI;
	    else if (north < YC && east > XC)
		c_az += 2 * M_PI;
		
	    /* camera angle to real ground */
	    /* orthogonal to ground: 90 degrees */
	    /* parallel to ground: 0 degrees */
	    c_angle = asin(cos(c_alt) * cos(slope) - sin(c_alt) * sin(slope) * cos(c_az - aspect));
	    
	    fbuf[col] = c_angle * radians_to_degrees;
	    if (c_angle_min > fbuf[col])
		c_angle_min = fbuf[col];
	}
	G_put_raster_row(outfd, fbuf, FCELL_TYPE);
    }
    G_percent(row, nrows, 2);

    G_close_cell(outfd);
    G_free(fbuf);

    type = "raster";
    G_short_history(name, type, &hist);
    G_write_history(name, &hist);
    
    G_init_colors(&colr);
    if (c_angle_min < 0) {
	clr_min = (FCELL)((int)(c_angle_min / 10 - 1)) * 10;
	clr_max = 0;
	G_add_f_raster_color_rule(&clr_min, 0, 0, 0, &clr_max, 0,
				  0, 0, &colr);
    }
    clr_min = 0;
    clr_max = 10;
    G_add_f_raster_color_rule(&clr_min, 0, 0, 0, &clr_max, 255,
			      0, 0, &colr);
    clr_min = 10;
    clr_max = 40;
    G_add_f_raster_color_rule(&clr_min, 255, 0, 0, &clr_max, 255,
			      255, 0, &colr);
    clr_min = 40;
    clr_max = 90;
    G_add_f_raster_color_rule(&clr_min, 255, 255, 0, &clr_max, 0,
			      255, 0, &colr);

    G_write_colors(name, G_mapset(), &colr);


    select_current_env();

    return 1;
}
