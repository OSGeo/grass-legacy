#include "gis.h"
#include "options.h"
#include <stdio.h>

static int elev = -1 ;
static CELL *elev_array[3] ;
static CELL row_array[3] ;
static CELL age_array[3] ;
static CELL *elev_array1 ;
static CELL *elev_array2 ;
static CELL *scrx1, *scry1, *scrx2, *scry2 ;
static CELL *last_scr_x1, *last_scr_y1, *last_scr_x2, *last_scr_y2 ;
static CELL *avg_array ;
static int last_row ;


initialize_arrays()
{
	age_array[0] = age_array[1] = age_array[2] = 0 ;
	row_array[0] = row_array[1] = row_array[2] = -99 ;
	avg_array  = G_allocate_cell_buf() ;
	scrx1  = G_allocate_cell_buf() ;
	scrx2  = G_allocate_cell_buf() ;
	scry1  = G_allocate_cell_buf() ;
	scry2  = G_allocate_cell_buf() ;
	elev_array[0]  = G_allocate_cell_buf() ;
	elev_array[1]  = G_allocate_cell_buf() ;
	elev_array[2]  = G_allocate_cell_buf() ;

	last_scr_x1 = scrx1 ;
	last_scr_x2 = scrx2 ;
	last_scr_y1 = scry1 ;
	last_scr_y2 = scry2 ;
	last_row = -99 ;

	if ((elev = G_open_cell_old(elevfile, elevfile_mapset)) == -1) 
	{
		char buffer[256] ;
		sprintf(buffer, "Raster file [%s] in [%s] not available",
			file, file_mapset) ;
		G_fatal_error(buffer) ;
	}
}
de_initialize_arrays()
{
	free(scrx1) ;
	free(scrx2) ;
	free(scry1) ;
	free(scry2) ;
	free(elev_array[0]) ;
	free(elev_array[1]) ;
	free(elev_array[2]) ;
	if (elev >= 0)
	{
	    G_close_cell (elev);
	    elev = -1;
	}
}


get_corners(row, scr_x1, scr_y1, scr_x2, scr_y2)
	CELL **scr_x1, **scr_x2, **scr_y1, **scr_y2 ;
{
	CELL *get_row() ;
	/* For raw elevation arrays */

	if (row == last_row)
	{
		return ;
	}

	if (row-1 == last_row)
	{
		elev_array1 = get_row(row) ;
		elev_array2 = get_row(row+1) ;
		if(do_average)
			avg_rows(avg_array, elev_array1, elev_array2) ;
		else
			no_avg_rows(avg_array, elev_array1, elev_array2) ;
		Screen_calc(avg_array, exag, last_scr_x1, last_scr_y1,
			(double)row+1., &window, do_zero) ;
		*scr_x1 = last_scr_x2 ;
		*scr_y1 = last_scr_y2 ;
		*scr_x2 = last_scr_x1 ;
		*scr_y2 = last_scr_y1 ;
	}

	else if (row+1 == last_row)
	{
		elev_array1 = get_row(row) ;
		elev_array2 = get_row(row-1) ;
		if(do_average)
			avg_rows(avg_array, elev_array1, elev_array2) ;
		else
			no_avg_rows(avg_array, elev_array1, elev_array2) ;
		Screen_calc(avg_array, exag, last_scr_x2, last_scr_y2,
			(double)row, &window, do_zero) ;
		*scr_x1 = last_scr_x2 ;
		*scr_y1 = last_scr_y2 ;
		*scr_x2 = last_scr_x1 ;
		*scr_y2 = last_scr_y1 ;
	}

	else 
	{
		elev_array1 = get_row(row-1) ;
		elev_array2 = get_row(row) ;
		if(do_average)
			avg_rows(avg_array, elev_array1, elev_array2) ;
		else
			no_avg_rows(avg_array, elev_array1, elev_array2) ;
		Screen_calc(avg_array, exag, last_scr_x1, last_scr_y1,
			(double)row, &window, do_zero) ;
		elev_array1 = get_row(row+1) ;
		avg_rows(avg_array, elev_array1, elev_array2) ;
		Screen_calc(avg_array, exag, last_scr_x2, last_scr_y2,
			(double)row+1., &window, do_zero) ;
		*scr_x1 = last_scr_x1 ;
		*scr_y1 = last_scr_y1 ;
		*scr_x2 = last_scr_x2 ;
		*scr_y2 = last_scr_y2 ;
	}

	last_scr_x1 = *scr_x1 ;
	last_scr_y1 = *scr_y1 ;
	last_scr_x2 = *scr_x2 ;
	last_scr_y2 = *scr_y2 ;
	last_row = row ;
}

static CELL *
get_row(row)
	int row ;
{
	int i ;
	int high ;
	int highest ;


	for(i=0; i<3; i++)
		age_array[i] += 1 ;

	for(i=0; i<3; i++)
	{
		if (row == row_array[i])
		{
			age_array[i] = 0 ;
			return(elev_array[i]) ;
		}
	}

	high = -1 ;
	for(i=0; i<3; i++)
	{
		if (age_array[i] > high)
		{
			highest = i ;
			high = age_array[i] ;
		}
	}

	G_get_map_row_nomask (elev, elev_array[highest], row) ;
	age_array[highest] = 0 ;
	row_array[highest] = row ;
	return(elev_array[highest]) ;
}

avg_rows(avg, a1, a2)
	CELL *avg, *a1, *a2 ;
{
	register CELL *p1, *p2, *p3 ;
	register int cols ;
	for(p1=a1, p2=a2, p3=avg, cols=G_window_cols()-1;
		cols; cols--, p1++, p2++, p3++)
	{
		if (! do_zero && (
			 *p1 == 0.  ||
			 *(p1+1) == 0. ||
			 *p2 == 0.  ||
			 *(p2+1) == 0.))
			 *p3 = 0.0 ;
		else
		*p3 = (*p1 + *(p1+1) + *p2 + *(p2+1)) / 4 ;
	}
}

no_avg_rows(avg, a1)
	CELL *avg, *a1 ;
{
	register CELL *p1, *p2 ;
	register int cols ;
	for(p1=a1, p2=avg, cols=G_window_cols()-1;
		cols; cols--, p1++, p2++)
	{
		if (! do_zero && *p1 == 0. )
			 *p2 = 0.0 ;
		else
		*p2 = *p1 ;
	}
}
