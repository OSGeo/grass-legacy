#include "global.h"
#define  NO_ELEV_DATA_VALUE  1000

rectify (name, mapset, result)
    char *name;
    char *mapset;
    char *result;
{
    struct Cell_head cellhd, win;
    int ncols, nrows;
    int row, col;
    int infd, outfd;
    CELL *cell;
    int x_ties, y_ties;
    int tie_row, tie_col;
    int i;
    double get_z_from_cell();
    double easting_to_col();
    double northing_to_row();
    int nrow1, nrow2;
    int ncol1, ncol2;
    double n1,w1,ns_res1,ew_res1;
    double n2,e2,z2,ns_res2,ew_res2;
    double nx,ex,zx;
    double NX, EX;
    int r2, c2, zz2;
    double row2, col2;
    double aver_z;

#ifdef DEBUG3
    fprintf (Bugsr,"Open temp elevation file: \n");
#endif

    /*  open temporary elevation cell layer */
    select_target_env();
    /**G_set_window (&elevhd);**/
    G_set_window (&target_window);  
    elev = G_open_cell_old (elev_layer, mapset_elev);
    /**G_get_cellhd (elev_layer, mapset_elev, &elevhd);**/ 
    elevbuf = G_allocate_cell_buf();  

    /* get an average elevation of the control points */
    /* this is used only if TIE points are outside of the elev_layer boundary */
    get_aver_elev (&group.control_points, &aver_z);


    if (elev < 0) {   
#ifdef DEBUG3
        fprintf (Bugsr,"CANT OPEN ELEV\n");
        fprintf (Bugsr,"elev layer = %s  mapset elev = %s elevfd = %d \n",
            elev_layer,mapset_elev,elev);
        fflush (Bugsr);
#endif
        return 0;
    }

#ifdef DEBUG3
    fprintf (Bugsr,"elev layer = %s  mapset elev = %s elevfd = %d \n",
            elev_layer,mapset_elev,elev);
    fflush (Bugsr);
#endif

   /* alloc Tie_Points  */
   y_ties = (int) (target_window.rows / TIE_ROW_DIST) + 2;
   x_ties = (int) (target_window.cols / TIE_COL_DIST) + 2;

#ifdef DEBUG3
   fprintf (Bugsr,"Number Tie_Points: y_ties %d \tx_ties %d \n",y_ties,x_ties);
#endif

   T_Point = (Tie_Point **) G_malloc (y_ties * sizeof(Tie_Point *));  
   for (i = 0; i < y_ties; i++)
       T_Point[i] = (Tie_Point *) G_malloc (x_ties * sizeof (Tie_Point));  

    /* build Tie_Points  */
    nrows = 0;
    for (tie_row = 0; tie_row < y_ties; tie_row++)
    {   n2 = target_window.north - (tie_row * TIE_ROW_DIST * target_window.ns_res) - 1;
        if (n2 <= target_window.south) n2 = target_window.south + 1;
        row2 = northing_to_row(&target_window, n2);
        r2 = (int) row2;

        if ( (G_get_map_row (elev, elevbuf, r2)) < 0)  
        {
#ifdef DEBUG3
           fprintf (Bugsr, "ERROR reading elevation layer %s fd = %d : row %d, col %d \n", elev_layer, elev, row, col);
#endif
           exit (0);
        }
        ncols = 0;
	for (tie_col = 0; tie_col < x_ties; tie_col++)
	{
           e2 = target_window.west + (tie_col * TIE_COL_DIST * target_window.ew_res)  + 1;
           if (e2 >= target_window.east) e2 = target_window.east - 1;
#ifdef DEBUG3
           fprintf (Bugsr,"Tie_Point \t row %d \tcol %d \n", tie_row, tie_col);
           fprintf (Bugsr,"\t east %f\t north %f \n", e2,n2);
#endif

           col2 = easting_to_col (&target_window, e2);
           c2 = (int) col2;
 
#ifdef DEBUG3
           fprintf (Bugsr,"\t\t row2 = %f \t col2 =  %f \n",row2,col2);
#endif
 
           zz2 = (elevbuf[c2]);
           /* if target TIE point has no elevation, set to aver_z */
           /* if (zz2 == 0) zz2 = NO_ELEV_DATA_VALUE; */
           if (zz2 == 0) zz2 =  aver_z; 
           z2 = (double) zz2;

#ifdef DEBUG3
           fprintf (Bugsr,"\t\t e2 = %f \t n2 =  %f \t z2 = %f \n",e2,n2,z2);
           fprintf (Bugsr,"\t\t XC = %f \t YC =  %f \t ZC = %f \n",group.XC, group.YC, group.ZC);
           fprintf (Bugsr,"\t\t omega = %f \t phi =  %f \t kappa = %f \n",group.omega, group.phi, group.kappa);
#endif
 
           /* ex, nx: photo coordinates */
           I_ortho_ref (e2, n2, z2, &ex, &nx, &zx, &group.camera_ref, group.XC, group.YC, group.ZC, group.omega, group.phi, group.kappa);

#ifdef DEBUG3
           fprintf (Bugsr,"\t\tAfter ortho ref (photo cords): ex = %f \t nx =  %f \n",ex,nx);
           fflush (Bugsr);
#endif

           /* ex, nx: relative to (row,col) = 0 */
           I_georef (ex, nx, &ex, &nx, group.E21, group.N21);
 
#ifdef DEBUG3
           fprintf (Bugsr,"\t\tAfter geo ref: ex = %f \t nx =  %f \n",ex,nx);
           fflush (Bugsr);
#endif

           T_Point[tie_row][tie_col].XT = e2;
           T_Point[tie_row][tie_col].YT = n2;
           T_Point[tie_row][tie_col].ZT = z2;
           T_Point[tie_row][tie_col].xt = ex;
           T_Point[tie_row][tie_col].yt = nx;
	}

    } /* end  build */

    /* close elev layer so we can open the file to be rectified */
    select_target_env();
    if (!G_close_cell (elev)) {  
#ifdef DEBUG3
       fprintf (Bugsr,"Can't close the elev file %s [%s in%s]",
       elev_layer, mapset_elev, G_location());
#endif
    }

/* open the result file into target window
 * this open must be first since we change the window later
 * cell files open for writing are not affected by window changes
 * but those open for reading are
 *
 * also tell open that cell file will have the same format
 * (ie number of bytes per cell) as the file being rectified
 */
    select_current_env();
    if (G_get_cellhd (name, mapset, &cellhd) < 0)
	return 0;

    select_target_env();
    G_set_window (&target_window);
    G_set_cell_format (cellhd.format);
    outfd = G_open_cell_new_random (result); 

    select_current_env();
    if (outfd < 0)
	return 0;

/* open the file to be rectified
 * set window to cellhd first to be able to read file exactly
 */
    select_current_env();
    G_suppress_warnings(1);
    G_set_window (&cellhd); 
    infd = G_open_cell_old (name, mapset);
    if (infd < 0) {
#ifdef DEBUG3
        fprintf (Bugsr, "Cant open %s in %s infd = %d \n", name, mapset, infd);
#endif
	close (infd);
        return 0;
    }
    cell = (CELL *) G_calloc (G_window_cols()+1, sizeof(CELL));
    *cell = 0;

    G_copy (&win, &target_window, sizeof(win));

    win.west += win.ew_res/2;
    ncols = target_window.cols;
    col = 0;

    for (tie_col = 0; tie_col < (x_ties -1); tie_col++) {
#ifdef DEBUG3
        fprintf (Bugsr,"Patching column %d: \n", ncols);
        fflush (Bugsr);
#endif

	if ((win.cols = ncols) > TIE_COL_DIST)
	    win.cols = TIE_COL_DIST;
	win.north = target_window.north - win.ns_res/2;
	nrows = target_window.rows;
	row = 0;

	for (tie_row = 0; tie_row < (y_ties -1); tie_row++) {
#ifdef DEBUG3

            fprintf (Bugsr,"Patching %d row: \n", nrows);
            fflush  (Bugsr);
#endif
	    if ((win.rows = nrows) > TIE_ROW_DIST)
		win.rows = TIE_ROW_DIST;

            get_psuedo_control_pt (tie_row,tie_col);
#ifdef DEBUG3
            fprintf (Bugsr,"\t got psuedo pts: row %d \t col %d \n",tie_row,tie_col);
            fflush  (Bugsr);
#endif
 
            compute_georef_matrix (&cellhd, &win);
#ifdef DEBUG3
            fprintf (Bugsr,"\t\tcompute geo matrix\n");
            fflush  (Bugsr);
#endif
	
            perform_georef (infd, cell);
#ifdef DEBUG3
            fprintf (Bugsr,"\t\tperform georef \n");
            fflush  (Bugsr);

            fprintf (Bugsr,"\t\twrite matrix \n");
            fflush  (Bugsr);
#endif
	    write_matrix (outfd, row, col);

	    nrows -= win.rows;
	    row += win.rows;
	    win.north -= (win.ns_res * win.rows);
	}

	ncols -= win.cols;
	col += win.cols;
	win.west += (win.ew_res * win.cols);
    }

/* Close cell files */
    select_target_env();
    if (!G_close_cell (outfd)) {
#ifdef DEBUG3
       fprintf (Bugsr,"Can't close the elev file %s [%s in%s]",
               elev_layer, mapset_elev, G_location());
#endif
    }

    select_current_env();

    G_close_cell (infd);
    free (cell);

    return 1;
}

