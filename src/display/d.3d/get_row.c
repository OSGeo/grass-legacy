#include <stdlib.h>
#include <stdio.h>
#include "gis.h"
#include "options.h"
#include "l_proto.h"

static int elev = -1 ;
static DCELL *elev_array[3] ;
static int row_array[3] ;
static int age_array[3] ;
static DCELL *elev_array1 ;
static DCELL *elev_array2 ;
static int *scrx1, *scry1, *scrx2, *scry2 ;
static int *last_scr_x1, *last_scr_y1, *last_scr_x2, *last_scr_y2 ;
static DCELL *avg_array ;
static int last_row ;
static DCELL *get_row(int);


int initialize_arrays (void)
{
	age_array[0] = age_array[1] = age_array[2] = 0 ;
	row_array[0] = row_array[1] = row_array[2] = -99 ;
	avg_array  = G_allocate_d_raster_buf() ;
	scrx1  = G_allocate_cell_buf() ;
	scrx2  = G_allocate_cell_buf() ;
	scry1  = G_allocate_cell_buf() ;
	scry2  = G_allocate_cell_buf() ;
	elev_array[0]  = G_allocate_d_raster_buf() ;
	elev_array[1]  = G_allocate_d_raster_buf() ;
	elev_array[2]  = G_allocate_d_raster_buf() ;

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

	return 0;
}

int de_initialize_arrays (void)
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

	return 0;
}


int get_corners(
	int row, int **scr_x1, int **scr_y1, int **scr_x2, int **scr_y2)
{
	DCELL *get_row() ;
	/* For raw elevation arrays */

	if (row == last_row)
	{
		return 0;
	}

	if (row-1 == last_row)
	{
		elev_array1 = get_row(row) ;
		elev_array2 = get_row(row+1) ;
		if(do_average)
			avg_rows(avg_array, elev_array1, elev_array2) ;
		else
			no_avg_rows(avg_array, elev_array1) ;
		Screen_calc(avg_array, exag, last_scr_x1, last_scr_y1,
			(double)row+1., &window, do_null) ;
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
			no_avg_rows(avg_array, elev_array1) ;
		Screen_calc(avg_array, exag, last_scr_x2, last_scr_y2,
			(double)row, &window, do_null) ;
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
			no_avg_rows(avg_array, elev_array1) ;
		Screen_calc(avg_array, exag, last_scr_x1, last_scr_y1,
			(double)row, &window, do_null) ;
		elev_array1 = get_row(row+1) ;
		avg_rows(avg_array, elev_array1, elev_array2) ;
		Screen_calc(avg_array, exag, last_scr_x2, last_scr_y2,
			(double)row+1., &window, do_null) ;
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

	return 0;
}

static DCELL *get_row(int row)
{
	int i ;
	int high ;
	int highest = 0;


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

	G_get_d_raster_row_nomask (elev, elev_array[highest], row) ;
	age_array[highest] = 0 ;
	row_array[highest] = row ;
	return(elev_array[highest]) ;
}

int avg_rows(DCELL *avg,DCELL *a1,DCELL *a2)
{
	register DCELL *p1, *p2, *p3 ;
	DCELL t1, t2, t3, t4;
	register int cols ;
	for(p1=a1, p2=a2, p3=avg, cols=G_window_cols()-1;
		cols; cols--, p1++, p2++, p3++)
	{
		t1 = *p1; t2 = *p2; t3= *(p1+1); t4 = *(p2+1);
		if(G_is_d_null_value(&t1))
		{
		    if(!do_null)
		    {
		        G_set_d_null_value(p3, 1);
			continue;
                    }
                    else
			t1 = 0.;
                }
		if(G_is_d_null_value(&t2))
		{
		    if(!do_null)
		    {
		        G_set_d_null_value(p3, 1);
			continue;
                    }
                    else
			t2 = 0.;
                }
		if(G_is_d_null_value(&t3))
		{
		    if(!do_null)
		    {
		        G_set_d_null_value(p3, 1);
			continue;
                    }
                    else
			t3 = 0.;
                }
		if(G_is_d_null_value(&t4))
		{
		    if(!do_null)
		    {
		        G_set_d_null_value(p3, 1);
			continue;
                    }
                    else
			t4 = 0.;
                }
		*p3 = (t1 + t2 + t3 + t4) / 4. ;
	}

	return 0;
}

int no_avg_rows(DCELL *avg,DCELL *a1 )
{
	register DCELL *p1, *p2 ;
	DCELL t;
	register int cols ;
	for(p1=a1, p2=avg, cols=G_window_cols()-1;
		cols; cols--, p1++, p2++)
	{
		t = *p1;
		if(G_is_d_null_value(p1))
		{
		    if(!do_null)
		        G_set_d_null_value(p2, 1);
                    else
			*p2 = 0.;
                }
		else
		    *p2 = *p1 ;
	}

	return 0;
}
