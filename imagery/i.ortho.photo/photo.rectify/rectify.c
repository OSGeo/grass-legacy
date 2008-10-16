/* rectification code */

/* 1/2002: updated to GRASS 5 write routines and 
   CELL/FP elevation - Markus Neteler
 */

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <grass/glocale.h>
#include "global.h"
#include "local_proto.h"

int rectify(char *name, char *mapset, char *result)
{
    struct Cell_head cellhd, win;
    int ncols, nrows;
    int row, col;
    int infd;
    void *rast;
    int x_ties, y_ties;
    int tie_row, tie_col;
    int i;
    double n2, e2, z2;
    double nx, ex, zx;
    int r2, c2;
    double row2, col2;
    double aver_z;

    G_debug(1, "Open temp elevation file: ");

    /*  open temporary elevation cell layer */
    select_target_env();

    /**G_set_window (&elevhd);**/
    G_debug(1, "target window: rs=%d cs=%d n=%f s=%f w=%f e=%f\n",
	    target_window.rows, target_window.cols, target_window.north,
	    target_window.south, target_window.west, target_window.east);

    G_set_window(&target_window);
    elevfd = G_open_cell_old(elev_layer, mapset_elev);

    /**G_get_cellhd (elev_layer, mapset_elev, &elevhd);**/
    elevbuf = G_allocate_d_raster_buf();	/* enforce DCELL */

    /* get an average elevation of the control points */
    /* this is used only if TIE points are outside of the elev_layer boundary */
    get_aver_elev(&group.control_points, &aver_z);


    if (elevfd < 0) {
	G_debug(1, "CANT OPEN ELEV");
	G_debug(1, "elev layer = %s  mapset elev = %s elevfd = %d",
		elev_layer, mapset_elev, elevfd);
	return 0;
    }

    G_debug(1, "elev layer = %s  mapset elev = %s elevfd = %d",
	    elev_layer, mapset_elev, elevfd);

    /* alloc Tie_Points  */
    y_ties = (int)(target_window.rows / TIE_ROW_DIST) + 2;
    x_ties = (int)(target_window.cols / TIE_COL_DIST) + 2;

    G_debug(1, "Number Tie_Points: y_ties %d \tx_ties %d", y_ties,
	    x_ties);

    T_Point = (Tie_Point **) G_malloc(y_ties * sizeof(Tie_Point *));
    for (i = 0; i < y_ties; i++)
	T_Point[i] = (Tie_Point *) G_malloc(x_ties * sizeof(Tie_Point));

    /* build Tie_Points  */
    nrows = 0;
    for (tie_row = 0; tie_row < y_ties; tie_row++) {
	n2 = target_window.north -
	    (tie_row * TIE_ROW_DIST * target_window.ns_res) - 1;
	if (n2 <= target_window.south)
	    n2 = target_window.south + 1;
	row2 = northing_to_row(&target_window, n2);
	r2 = (int)row2;

	if ((G_get_d_raster_row(elevfd, elevbuf, r2)) < 0) {
	    G_debug(1, "ERROR reading elevation layer %s fd = %d : row %d",
		    elev_layer, elevfd, r2);
	    exit(0);
	}
	ncols = 0;
	for (tie_col = 0; tie_col < x_ties; tie_col++) {
	    e2 = target_window.west +
		(tie_col * TIE_COL_DIST * target_window.ew_res) + 1;
	    if (e2 >= target_window.east)
		e2 = target_window.east - 1;

	    G_debug(5, "Tie_Point \t row %d \tcol %d", tie_row, tie_col);
	    G_debug(5, "\t east %f\t north %f", e2, n2);

	    col2 = easting_to_col(&target_window, e2);
	    c2 = (int)col2;

	    G_debug(5, "\t\t row2 = %f \t col2 =  %f", row2, col2);
	    G_debug(5, "\t\t   r2 = %d \t   c2 =  %d", r2, c2);
	    G_debug(5, "\t\t elevbuf[c2] = %f", (DCELL) elevbuf[c2]);

	    /* if target TIE point has no elevation, set to aver_z */
	    if (G_is_d_null_value(&elevbuf[c2]))
		z2 = aver_z;
	    else
		z2 = (double)elevbuf[c2];

	    G_debug(5, "\t\t e2 = %f \t n2 =  %f \t z2 = %f", e2, n2, z2);
	    G_debug(5, "\t\t XC = %f \t YC =  %f \t ZC = %f", group.XC,
		    group.YC, group.ZC);
	    G_debug(5, "\t\t omega = %f \t phi =  %f \t kappa = %f",
		    group.omega, group.phi, group.kappa);

	    /* ex, nx: photo coordinates */
	    I_ortho_ref(e2, n2, z2, &ex, &nx, &zx, &group.camera_ref,
			group.XC, group.YC, group.ZC, group.omega, group.phi,
			group.kappa);

	    G_debug(5, "\t\tAfter ortho ref (photo cords): ex = %f \t nx =  %f",
		    ex, nx);

	    /* ex, nx: relative to (row,col) = 0 */
	    I_georef(ex, nx, &ex, &nx, group.E21, group.N21);

	    G_debug(5, "\t\tAfter geo ref: ex = %f \t nx =  %f", ex, nx);

	    T_Point[tie_row][tie_col].XT = e2;
	    T_Point[tie_row][tie_col].YT = n2;
	    T_Point[tie_row][tie_col].ZT = z2;
	    T_Point[tie_row][tie_col].xt = ex;
	    T_Point[tie_row][tie_col].yt = nx;
	}

    }				/* end  build */

    /* close elev layer so we can open the file to be rectified */
    select_target_env();
    if (!G_close_cell(elevfd)) {
	G_debug(1, "Can't close the elev file %s [%s in %s]",
		elev_layer, mapset_elev, G_location());
    }

    /* open the result file into target window
     * this open must be first since we change the window later
     * raster maps open for writing are not affected by window changes
     * but those open for reading are
     *
     * also tell open that raster map will have the same format
     * (ie number of bytes per cell) as the file being rectified
     */
    select_current_env();
    if (G_get_cellhd(name, mapset, &cellhd) < 0)
	return 0;

    select_target_env();
    G_set_window(&target_window);
    G_set_cell_format(cellhd.format);


    select_current_env();

    G_copy(&win, &target_window, sizeof(win));

    win.west += win.ew_res / 2;
    ncols = target_window.cols;
    col = 0;

    for (tie_col = 0; tie_col < (x_ties - 1); tie_col++) {
	G_debug(3, "Patching column %d:", ncols);

	if ((win.cols = ncols) > TIE_COL_DIST)
	    win.cols = TIE_COL_DIST;
	win.north = target_window.north - win.ns_res / 2;
	nrows = target_window.rows;
	row = 0;

	for (tie_row = 0; tie_row < (y_ties - 1); tie_row++) {
	    G_debug(5, "Patching %d row:", nrows);

	    if ((win.rows = nrows) > TIE_ROW_DIST)
		win.rows = TIE_ROW_DIST;

	    get_psuedo_control_pt(tie_row, tie_col);

	    G_debug(5, "\t got psuedo pts: row %d \t col %d", tie_row, tie_col);

	    compute_georef_matrix(&cellhd, &win);

	    G_debug(5, "\t\tcompute geo matrix");

	    /* open the source imagery file to be rectified */
	    /* set window to cellhd first to be able to read file exactly */
	    select_current_env();
	    G_set_window(&cellhd);
	    infd = G_open_cell_old(name, mapset);
	    if (infd < 0) {
		close(infd);
		return 0;
	    }
	    map_type = G_get_raster_map_type(infd);
	    rast =
		(void *)G_calloc(G_window_cols() + 1,
				 G_raster_size(map_type));
	    G_set_null_value(rast, G_window_cols() + 1, map_type);

	    /* perform the actual data rectification */
	    perform_georef(infd, rast);

	    G_debug(5, "\t\tperform georef");
	    G_debug(5, "\t\twrite matrix");

	    /* close the source imagery file and free the buffer */
	    select_current_env();
	    G_close_cell(infd);
	    G_free(rast);
	    /*   select_current_env(); */
	    select_target_env();


	    /* write of the data rectified into the result file */
	    write_matrix(row, col);

	    nrows -= win.rows;
	    row += win.rows;
	    win.north -= (win.ns_res * win.rows);
	}

	ncols -= win.cols;
	col += win.cols;
	win.west += (win.ew_res * win.cols);
	G_percent(col, col + ncols, 1);
    }

    select_target_env();

    if (cellhd.proj == 0) {	/* x,y imagery */
	cellhd.proj = target_window.proj;
	cellhd.zone = target_window.zone;
    }

    if (target_window.proj != cellhd.proj) {
	cellhd.proj = target_window.proj;
	G_warning(_("%s@%s: projection doesn't match current settings"),
		name, mapset);
    }

    if (target_window.zone != cellhd.zone) {
	cellhd.zone = target_window.zone;
	 G_warning(_("%s@%s: zone doesn't match current settings"),
		name, mapset);
    }

    target_window.compressed = cellhd.compressed;
    G_close_cell(infd);
    write_map(result);
    select_current_env();

    return 1;
}
