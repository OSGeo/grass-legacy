/*  ./src/libes/dlg/read_area.c
 *******************************************************************
 *  #include "dlg.h"
 *
 *  read_area (fd,dlg,area,xarr,yarr,n_points,n_alloc)
 *      FILE *fd              file containing dlg header info
 *      struct dlg *dlg       structures containing dlg information
 *      int area              are for which polygon coors. desired
 *      double **xarr, **yarr pointers to x and y arrays for returning coors.
 *      int *n_points         number of points returned in xarr & yarr
 *      int *n_alloc          amount of space allocated in arrays
 *
 * returns:  -1 on error
 *            0 on completion
 *            1 if there is no area for that number
 */

#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "dlg.h"

#define  MEMORY_SIZE  1024

int dlg_read_whole_area (
    FILE *fd,
    struct dlg *dlg,
    int area,
    double **xarray,double **yarray ,
    int *n_points,
    int *n_alloc)
{
	double *x_area = NULL, *y_area = NULL;
	double *x_line, *y_line ;
	int line ;
	int at_line ;
	int line_abs ;
	int n_coors=0;
	int stat ;

	if (! *n_alloc)
	{
		*n_alloc  = MEMORY_SIZE ;
		*xarray = (double *) G_calloc(*n_alloc, sizeof(double)) ;
		*yarray = (double *) G_calloc(*n_alloc, sizeof(double)) ;
	}
	
	*n_points = 0 ;

/* Read the dlg information for the area  */

	if (dlg->area_off[area]  ==  '\0')
		return(1) ;
	fseek(fd, dlg->area_off[area], 0) ;
	_dlg_read_area(&dlg->area, fd) ;

	for (at_line=0; at_line<dlg->area.n_lines; at_line++)
	{
		line = dlg->area.lines[at_line] ;
		if (line == 0)    /* Leave a island coordinate marker to identify island */
		{
			++(*n_points) ;
			if (*n_points + 1 >= *n_alloc)
			{
				*n_alloc = *n_points + n_coors + MEMORY_SIZE ;
				*xarray = (double *)G_realloc((char *)*xarray, *n_alloc * sizeof(double));
				*yarray = (double *)G_realloc((char *)*yarray, *n_alloc * sizeof(double));
			}

			*x_area++ = ISLAND_MARKER ;  /*  these were 0.0  */
			*y_area++ = ISLAND_MARKER ;
			continue ;
		}
		else
		{
			line_abs = abs(line) ;
			/*  bad read  */
			if (0 > (stat = dlg_read_line (fd,dlg,line_abs)) )
				return(-1) ;
			/*  no line  */
			if (stat == 1)
				continue ;
			n_coors = dlg->line.n_coors ;
		}

	/* Save these points in growing perimeter list */

	/* Make sure there is enough space to add this line */
		if (*n_points + n_coors >= *n_alloc)
		{
			*n_alloc = *n_points + n_coors + MEMORY_SIZE ;
			*xarray = (double *)G_realloc((char *)*xarray, *n_alloc * sizeof(double));
			*yarray = (double *)G_realloc((char *)*yarray, *n_alloc * sizeof(double));
		}

		x_area = *xarray + *n_points ;
		y_area = *yarray + *n_points ;

		*n_points += n_coors ;

		if (line == line_abs)
		{
			x_line = dlg->line.coors ;
			y_line = dlg->line.coors + 1 ;
			while(n_coors--)
			{
				*x_area++ = *x_line ;
				*y_area++ = *y_line ;
				x_line += 2 ;
				y_line += 2 ;
			}
		}
		else
		{
			x_line = dlg->line.coors + 2 * n_coors - 2 ;
			y_line = dlg->line.coors + 2 * n_coors - 1 ;

			while(n_coors--)
			{
				*x_area++ = *x_line ;
				*y_area++ = *y_line ;
				x_line -= 2 ;
				y_line -= 2 ;
			}
		}
	}
	return(0) ;
}
