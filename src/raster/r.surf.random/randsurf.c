/*****************/
/** randsurf()	**/
/*****************/

#include "gis.h"
#include <math.h>

randsurf(out,min,max)

char	*out;				/* Name of cell files to be opened.	*/
int	min,max;			/* Minimum and maximum cell values.	*/

{

	int	nrows,ncols;		/* Number of cell rows and columns	*/

	

	CELL	*row_out;		/* Buffer just large enough to hold one	*/
    					/* row of the raster map layer.		*/

	int	fd_out;			/* File descriptor - used to identify 	*/
					/* open cell files.			*/
	

	int	row_count,col_count;

	float	rand1();
		
	/****** INITIALISE RANDOM NUMBER GENERATOR ******/

	rand1(-1* getpid());

	/****** OPEN CELL FILES AND GET CELL DETAILS ******/

	fd_out = G_open_cell_new(out);
	
	nrows = G_window_rows();
	ncols = G_window_cols();

	row_out = G_allocate_cell_buf();


	/****** PASS THROUGH EACH CELL ASSIGNING RANDOM VALUE ******/

	for (row_count=0;row_count<nrows;row_count++)
	{
	    for (col_count=0;col_count<ncols;col_count++)
		*(row_out+col_count) = rint(rand1(2742)*(max-min)+min);	
	   
	    /* Write contents row by row */
	    G_put_map_row(fd_out,row_out);
	}


	/****** CLOSE THE CELL FILE ******/

	G_close_cell(fd_out);


}



