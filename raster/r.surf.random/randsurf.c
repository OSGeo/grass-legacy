/*****************/
/** randsurf()	**/
/*****************/

#include <unistd.h>
#include <math.h>
#include <grass/gis.h>

int randsurf (
    char *out,				/* Name of cell files to be opened.	*/
    int min,
    int max			/* Minimum and maximum cell values.	*/
)

{

	int	nrows,ncols;		/* Number of cell rows and columns	*/

	

	DCELL	*row_out;		/* Buffer just large enough to hold one	*/
    					/* row of the raster map layer.		*/

	int	fd_out;			/* File descriptor - used to identify 	*/
					/* open cell files.			*/
	

	int	row_count,col_count;

	float	rand1();
		
	/****** INITIALISE RANDOM NUMBER GENERATOR ******/

	rand1(-1* getpid());

	/****** OPEN CELL FILES AND GET CELL DETAILS ******/

        if ( (fd_out=G_open_raster_new(out , DCELL_TYPE)) <0)
        {
                char err[256];
                sprintf(err,"ERROR: Problem opening output file.");
                G_fatal_error(err);
        }

	nrows = G_window_rows();
	ncols = G_window_cols();

	row_out = G_allocate_d_raster_buf();


	/****** PASS THROUGH EACH CELL ASSIGNING RANDOM VALUE ******/

	for (row_count=0;row_count<nrows;row_count++)
	{
	    for (col_count=0;col_count<ncols;col_count++)
		*(row_out+col_count) = (DCELL) (rand1(2742)*(max-min)+min);	
	   
	    /* Write contents row by row */
	    G_put_d_raster_row(fd_out, (DCELL *)row_out); 
	}


	/****** CLOSE THE CELL FILE ******/

	G_close_cell(fd_out);

	return 0;
}
