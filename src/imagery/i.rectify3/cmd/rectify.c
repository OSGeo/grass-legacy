/*======================================================================
                             i.rectify.c

  rectify.c --

            Perform the individual file rectification.

	    (1) set the target window
	    (2) Partion target window into tiles
	    (3) Work with a single tile at a time.
	    (4) For each cell in the tile compute the inverse
	        transformation.  That is, determine from which pixel in the
                original (source) image, the target pixel should be filled in.
                See (matrix.c) for details
	    (5) After all pixel location for a tile are computed,
	        do the actural reading and writing (perform.c).

======================================================================*/

#include <unistd.h>
#include "global.h"
#include "protodefs.h"

int rectify (char *name, char *mapset, char *result)
{
    Auxillary_Photo  *auxil;

    struct Cell_head cellhd, win;
    int ncols, nrows;
    int row, col;
    int infd, elevfd;
    void *rast;
    char msg[100];      /* message buffer */
    int  num_blocks, block_count;
    char buf[64]="";

    /* make auxilary visiable */
    auxil = (Auxillary_Photo *) group.auxil;

    select_current_env();
    if (G_get_cellhd (name, mapset, &cellhd) < 0)
	return 0;

/* open the result file into target window
 * this open must be first since we change the window later
 * cell files open for writing are not affected by window changes
 * but those open for reading are
 *
 * also tell open that cell file will have the same format
 * (ie number of bytes per cell) as the file being rectified
 */

    select_target_env();
    G_set_window (&target_window);
    G_set_cell_format (cellhd.format);
    select_current_env();

/* open the file to be rectified
 * set window to cellhd first to be able to read file exactly
 */

/* source image and elevation can NOT be opened for reading */
/* simultaneously because different projections */
/*******************************
**    G_set_window (&cellhd);
**    infd = G_open_cell_old (name, mapset);
**    if (infd < 0)
**    {
**	close (infd);
**	return 0;
**    }
**    cell = (CELL *) G_calloc (G_window_cols()+1, sizeof(CELL));
**    *cell = 0;
*******************************/


    G_copy (&win, &target_window, sizeof(win));

    win.west += win.ew_res/2;
    ncols = target_window.cols;
    col = 0;

    /** get a rough idea of the number of blocks we will need **/
    /** so we can print stats about the progress **/
    num_blocks = (((win.cols / NCOLS)+1) * ((win.rows / NROWS)+1));
    block_count = 0;

    temp_fd=0;
    while (ncols > 0)
    {
	if ((win.cols = ncols) > NCOLS)
	    win.cols = NCOLS;
	win.north = target_window.north - win.ns_res/2;
	nrows = target_window.rows;
	row = 0;

	while (nrows > 0)
	{
            block_count++ ;  /* for progress reports */

	    if ((win.rows = nrows) > NROWS)
		win.rows = NROWS;

	    /* tell user about progress */
	    /* TODO verbose flag */
            /***
	    ** sprintf (msg, "Rectifying Block <col = %d> <row = %d> \n",
	    **     ncols, nrows);
	    ** fprintf  (stderr, msg);
            ***/
	    G_percent (block_count, num_blocks, 1);


	    /** This is only for photo trans or trans that need elevation data **/
	    /** For now we just assume if group.auxil is not NULL then we have **/
	    /** elevation data.  TODO either explict switches for the differnt **/
	    /** transformations or a better checking of the elevation data     **/


	    /* open the elevation data in the target window
	     * this open must be opened prior to the source imagery ??
	     * The elevatation data is opened for reading and is affected by
	     * all subsequent window changes in the target location
	     */

	    select_target_env();
	    G_set_window (&target_window);  

	    if (auxil != NULL)  {
		elevfd = G_open_cell_old (auxil->elev.elev_map,
					  auxil->elev.elev_mapset);

		select_current_env();
		if (elevfd < 0) {
		  /** TODO -- remove group.elev. */
		  /** G_fatal_error ("Can't open elevation file <%s>\n", 
		   ** group.elev.elev_map); 
		  **/ 
		  return 0;
		}
	    } /* end auxil != NULL */

	    select_current_env();

	    /* compute the georef equations */
	    /* ortho photo and TM require elevation */
	    compute_georef_matrix (elevfd, &cellhd, &win);


	    /** TODO -- this is done in matrix.c **/
	
	    /* close the elevation data */
	    if (auxil != NULL )  {
	      select_target_env();
	      G_close_cell (elevfd);
	      select_current_env();
	    } /* end auxil != NULL */

	    /* open the source imagery file to be rectified */
	    /* set window to cellhd first to be able to read file exactly */
	    select_current_env();
	    G_set_window (&cellhd);
	    infd = G_open_cell_old (name, mapset);
	    if (infd < 0)
	      {
		close (infd);
		return 0;
	      }
	    map_type = G_raster_map_type(name, mapset);
	    rast = (void *)  G_calloc (G_window_cols()+1, G_raster_size(map_type));
	    G_set_null_value(rast, G_window_cols()+1, map_type);

	    /* perform the actual data rectification */
	    perform_georef (infd, rast);

	    /* close the source imagery file and free the buffer */
	    select_current_env();
	    G_close_cell (infd);
	    G_free (rast);
	 /*   select_current_env();*/
	 select_target_env();


	    /* write of the data rectified into the result file */
	    write_matrix (row, col);

	    nrows -= win.rows;
	    row += win.rows;
	    win.north -= (win.ns_res * win.rows);
	}

	ncols -= win.cols;
	col += win.cols;
	win.west += (win.ew_res * win.cols);
    }

    select_target_env();
    G_suppress_warnings(0);
    if (cellhd.proj == 0) { /* x,y imagery */
			cellhd.proj = target_window.proj;
			cellhd.zone = target_window.zone;
	}

    if (target_window.proj != cellhd.proj) {
			cellhd.proj = target_window.proj;
			sprintf(buf,"WARNING %s@%s: projection don't match current settings.\n",name,mapset);
			G_warning(buf);
	}  

    if (target_window.zone != cellhd.zone) {
			cellhd.zone = target_window.zone;
			sprintf(buf,"WARNING %s@%s: zone don't match current settings .\n",name,mapset);
			G_warning(buf);
	}  

    G_suppress_warnings(1);
    target_window.compressed=cellhd.compressed;
    G_close_cell (infd); /* (pmx) 17 april 2000 */
    write_map(result);
    select_current_env();

    G_suppress_warnings(0);

    return 1;
}
