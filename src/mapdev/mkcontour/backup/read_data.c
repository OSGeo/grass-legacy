#include "contour.h"

cntr_read_data(cfd,dfp,afp,interval,window)
    int cfd;	/* file identifier */
    FILE *dfp, *afp;  /* file descriptor */
    int	interval; /* interval between contours */
    struct Cell_head	*window; /* info for active window */
{
    CELL	*temp;/*maintains memory location when reassigning arrays */
    CELL 	*data[2];/*data will point to 2 rows of data from cell file*/
    register int 	nrows, ncols;
    int		y;/* looping variable */
    int		r; /* looping variable */ 
    int		init_flag = 1;
    int		percent;/*reported in G_percent function*/

    /*inside read_data.c */
    /* get the cell array sizes for cell file currently open */
    nrows = G_window_rows();
    ncols = G_window_cols();


    /* DEBUG */
    /*fprintf(stderr,"%d rows   %d cols \n",nrows,ncols);*/

    /* initialize the map location to be used in coordinate calculations */
    NS_RES = window->ns_res;
    SOUTH = window->north - NS_RES/2;

    /* choose an appropriate reporting rate based on data size */
    if (nrows > 150)
	percent = 1;
    else if (nrows > 75)
	percent = 5;
    else
	percent = 10;
    
    /* initialize variables inside G_percent function */
    
    fprintf(stderr,"CONTOURING COMPLETED:");
    G_percent(0,nrows,percent);

    /* reading in data and building contours will be done a row at a time*/
    for (y=0; y < nrows-1; y++)
    {
	G_percent(y,nrows,percent);/*reporting status of job */
	NORTH = SOUTH; /* decremented as it loops */
	SOUTH = NORTH - NS_RES;
	if (init_flag) /* first read in two rows of data from cell_file */
	{
	    for(r=0; r < 2; r++)
	    {
		data[r] = G_allocate_cell_buf();
		if (G_get_map_row(cfd,data[r],r) == -1)
		{
		    fprintf(stderr,"ERROR: reading in data \n");
		    exit(1);
		}
	    }
	    init_flag = 0;
	}
	else /* read in one row at a time and reassign data array*/
	{
	    temp = data[0]; /* so don't lose allocated memory location */
	    data[0] = data[1];
	    data[1] = temp;
	    if (G_get_map_row(cfd,data[1],y+1) == -1)
	    {
		fprintf(stderr,"ERROR: reading in data from \n");
		exit(1);
	    }
	}

	/* call subroutine to build contours */
	cntr_do_contours(data,ncols,interval,dfp,afp,window);
    }
    G_percent(nrows,nrows,percent);/* to insure 100% at end of job*/
}
