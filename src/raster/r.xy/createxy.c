/*****************/
/** createxy()	**/
/*****************/

#include "gis.h"

createxy(xname,yname)

char	*xname,*yname;			/* Name of cell files to be opened.	*/

{

	int	nrows,ncols;		/* Number of cell rows and columns	*/

	

	CELL	*x_out;			/* Buffer just large enough to hold one	*/
    	CELL	*y_out;			/* row of the raster map layer.		*/

	int	fd_xout;		/* File descriptor - used to identify 	*/
	int	fd_yout;		/* open cell files.			*/
	

	int	row_count,col_count;

	struct Cell_head	*window;

	struct Categories	xcats;
	struct Categories	ycats;


	int	northing;

	char	label[128];

	/****** OPEN CELL FILES AND GET CELL DETAILS ******/


	fd_xout = G_open_cell_new(xname);
	fd_yout = G_open_cell_new(yname);

	nrows = G_window_rows();
	ncols = G_window_cols();

	x_out = G_allocate_cell_buf();
	y_out = G_allocate_cell_buf();

	G_init_cats( (CELL) ncols, "Column Positions", &xcats);
	G_init_cats( (CELL) nrows, "Row Positions", &ycats);



	/****** PASS THROUGH EACH CELL ASSIGNING COORDINATE VALUE ******/

	
	/* Fill up the buffer of x values */

	for (col_count=0;col_count<ncols;col_count++)
	{
	    *(x_out+col_count) =col_count;
				/* (int) G_col_to_easting((double)col_count+0.5,window) */	
	    sprintf(label,"%f",(float)col_count); 

	    G_set_cat (col_count,label,&xcats);
	}


	for (row_count=0;row_count<nrows;row_count++)
	{

	    /* Find out what the y coordinate will be for each new row */

	    northing = row_count;
	 		/* (int) G_row_to_northing((double)row_count+0.5,window) */
	    sprintf(label,"%f",(float)row_count);

	    G_set_cat(row_count,label,&ycats);


	    /* Fill up the buffer of y values */

	    for (col_count=0;col_count<ncols;col_count++)
   		*(y_out+col_count) = northing;

	    /* Write contents row by row */
	    G_put_map_row(fd_xout,x_out);
	    G_put_map_row(fd_yout,y_out);

	}


	/****** CLOSE THE CELL FILES ******/ 

	G_close_cell(fd_xout);
	G_close_cell(fd_yout);


	/****** WRITE CATEGORY SUPPORT FILES ******/

	G_write_cats(xname,&xcats);
	G_write_cats(yname,&ycats);

	G_free_cats(&xcats);
	G_free_cats(&ycats);




}



