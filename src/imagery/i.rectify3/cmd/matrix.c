/*======================================================================
                             i.rectify.c

  rectify.c --

            Computes and stores the rectification data for
	    a single tile.

	    (1) Set window
	    (2) Compute the transformation paratmers 
	    (3) For each cell in the tile compute the inverse
	        transformation.  That is, determine from which pixel in the
                original (source) image, the target pixel should be filled in.
            (4) *rmap holds the rows and *cmap holds the columns that 
                are determine in (3) for each pixel in the single tile.
	    (5) sort the rmap and cmap so that when we do the actual 
	        reading and writing of data only one pass is required
		through the original imagery.

======================================================================*/


/**************************************************************
 * compute_georef_matrix (elevation_file, win1, win2)
 *
 */
#include <stdlib.h>
#include "global.h"
#include "crs.h"     /* CRS HEADER FILE */

static int cmp (IDX *,IDX *);
double northing_to_row();
double easting_to_col();


int compute_georef_matrix (int elevation_file, struct Cell_head *win1,
    struct Cell_head *win2)
{
    ROWCOL *rmap, *cmap,rr,cc;
    int nrow1, nrow2;
    int ncol1, ncol2;
    double n1,w1,ns_res1,ew_res1;
    double n2,e2,ns_res2,ew_res2;
    double nx,ex;
    double z2 = 0.0, zx;

/*    double NX, EX; DELETED WITH CRS MODIFICATIONS */
    int row, col=0;
    int min, max;

/* stuff for elevation data used in ortho photo and TM */
    int  order = group.trans_type;
    struct Cell_head temp_win;
    CELL  *elevation_data;
    int  elev_row, elev_col;
    char msg[80];


    ns_res1 = win1->ns_res;
    ew_res1 = win1->ew_res;
    nrow1 = win1->rows;
    ncol1 = win1->cols;
    n1 = win1->north;
    w1 = win1->west;

    ns_res2 = win2->ns_res;
    ew_res2 = win2->ew_res;
    nrow2 = win2->rows;
    ncol2 = win2->cols;
    n2 = win2->north;
    e2 = win2->west;
    matrix_rows = nrow2;
    matrix_cols = ncol2;

/*  allocate temporary elevation data buffer */
    select_target_env();
    G_copy (&temp_win, win2, sizeof (temp_win));
    G_set_window (&temp_win);  
    elevation_data  = G_allocate_cell_buf();  
    if (elevation_data == NULL) {
      /** TODO - message and exit ? **/
    }
    select_current_env();

    /* get an average elevation of the control points */
    /* this is used only if TIE points are outside of the elev_layer boundary */
    /* get_aver_elev (&group.control_points, &aver_z); */



/* georef equation is
 * ex = E21a + E21b * e2 + E21c * n2
 * nx = N21a + N21b * e2 + N21c * n2
 *
 * compute common code (for northing) outside east loop
 */
    for (n2 = win2->north, row = 0; row < nrow2; row++, n2 -= ns_res2)
    {
	rmap = row_map[row];
	cmap = col_map[row];
	min = max = -1;

	/* read this row of elevation data */
        elev_row = (int) northing_to_row(win2, n2);

	/* NOTE: no elevation for poly trans */
	/** TODO - better check of when we have elev data */

	if (group.auxil != NULL)
	  if ( (G_get_map_row (elevation_file, elevation_data, elev_row)) < 0)  
	    {
	      sprintf (msg, "ERROR reading elevation layer at row %d, col %d \n", row, col);
	      G_warning (msg);
	    }


	for (e2 = win2->west, col = 0; col < ncol2; col++, e2 += ew_res2)
	{
	  /* BACKWARDS TRANSFORMATION */
	  /* georef e2,n2 */

	  /** z2 = zero for poly trans **/
	  if (group.auxil !=NULL ) {
	    elev_col = (int) easting_to_col (win2, e2);
	    z2 = (double) (elevation_data[elev_col]);
	  }

	  /** CRS_georef(e2,n2,&ex,&nx,E21,N21,order); **/
	  group.inverse_trans (&group, &ex, &nx, &zx, e2, n2, z2);

	  rr = (n1-nx) / ns_res1;
	  if (rr < 0 || rr >= nrow1)
	    rr = -1;
	  else if (min < 0)
	    min = max = rr;
	  else if (rr < min)
	    min = rr;
	  else if (rr > max)
	    max = rr;
	  *rmap++ = rr;
	  
	  cc = (ex-w1) / ew_res1;
	  if (cc < 0 || cc >= ncol1)
	    cc = -1;
	  *cmap++ = cc;
	}
	row_min[row] = min;
	row_max[row] = max;
	row_left[row] = 0;
	row_right[row] = matrix_cols-1;
	row_idx[row] = row;
    }
    qsort (row_idx, nrow2, sizeof (IDX), cmp);

    /* close elev layer so we can open the file to be rectified */
    select_target_env();
    if (!G_close_cell (elevation_file)) {  
#ifdef DEBUG3
       fprintf (Bugsr,"Can't close the elev file %s [%s in%s]",
       elev_layer, mapset_elev, G_location());
#endif
       select_current_env();
     }

     return 0;
}

static int cmp (IDX *a,IDX *b)
{
    return (int) (row_min[*a] - row_min[*b]);
}


/* in these next 2 routines, location determines if we are
 * converting from center of the cell (location == .5)
 * top or left edge (location == 0.0)
 * bottom or right edge (location == 1.0)
 */

double 
row_to_northing (struct Cell_head *cellhd, int row, double location)
{
    return cellhd->north - (row + location) * cellhd->ns_res;
}

double 
col_to_easting (struct Cell_head *cellhd, int col, double location)
{
    return cellhd->west + (col + location) * cellhd->ew_res;
}

double 
northing_to_row (struct Cell_head *cellhd, double north)
{
    return  (cellhd->north - north) / cellhd->ns_res;
}

double 
easting_to_col (struct Cell_head *cellhd, double east)
{
    return  (east - cellhd->west) / cellhd->ew_res;
}

