/**************************************************************************/
/*** 	                   interpol_layer()				***/
/*** Function to read in and interpolate between rasterized contours	***/
/*** Jo Wood, V1.0, December, 1991, V1.2 March 7th, 1992		***/
/*** V2.0 Modified to conform to GRASS module strcture, 23rd July, 1995 ***/
/*** V2.1 Modified to allow any profile angle, 28th July, 1995.		***/
/***									***/
/**************************************************************************/

#include "spline.h"

interpol_layer(dem_name,rcont_name,profile)

char	*dem_name,		/* Name of DEM and rasterised contours.	*/
	*rcont_name;
int	profile;		/* Profile direction code.		*/
{
    /*------------------------------------------------------------------*/
    /*				INITIALISE				*/
    /*------------------------------------------------------------------*/
 
    char 	profile_name[127],	/* Names of files storing DEM	*/
		weight_name[127];	/* profiles and weights.	*/
			
    int 	row,col,		/* Counts through raster cells.	*/
		fd_rcont,		/* Rasterised contour file desc.*/
		fd_dem,			/* DEM and weights profile file	*/
		fd_weight,		/* desciptor.			*/
		ncells;			/* Number of cells in profile.	*/

    CELL	*dem_prof,		/* Stores oblique profiles.	*/
		*weight_prof,
		*dem_ptr,		/* Stores entire rasters.	*/
		*weight_ptr;

    /*------------------------------------------------------------------*/
    /*			OPEN RASTER FILE CONTAING CONTOURS		*/
    /*------------------------------------------------------------------*/

    if ((mapset_in=G_find_cell(rcont_name,"")) == NULL)
    {
	char err[256];
	sprintf(err,"Rasterized contours could not be opened!\n");
	G_fatal_error(err);
    }

    fd_rcont  = G_open_cell_old(rcont_name,mapset_in);


    /*------------------------------------------------------------------*/
    /*     ALLOCATE MEMORY FOR ENTIRE DEM AND READ IN CONTOURS		*/
    /*------------------------------------------------------------------*/


						/* Oblique profiles.	*/
    weight_prof	= (int *) G_malloc((ncols+nrows)*sizeof(int));
    dem_prof	= (CELL *) G_malloc((ncols+nrows)*sizeof(CELL));

						/* Entire rasters.	*/
    dem_ptr	= (CELL *) G_malloc(ncols*nrows*sizeof(CELL));
    weight_ptr	= (int *) G_malloc(ncols*nrows*sizeof(int));

			/* Read in contours and initialise weights.	*/
    for (row=0; row<nrows; row++)
    {
	G_get_map_row(fd_rcont,dem_ptr + ncols*row,row);

	for (col=0; col<ncols; col++)
	    *(weight_ptr + row*ncols + col) = -1;
    }


    /*------------------------------------------------------------------*/
    /*	    	      INTERPOLATE SPARSE PROFILE			*/
    /*------------------------------------------------------------------*/

    printf("\nSTEP 3 - Calculating splines for profile number %d\t",profile);

    /* Open files to store profile interpolation */

    sprintf(profile_name,"%s.%d",dem_name,profile);
    sprintf(weight_name,"%s.%d.weights",dem_name,profile);

    if ((fd_dem = G_open_cell_new(profile_name,mapset_out))==NULL)
    {
	char err[256];
	sprintf(err,"Trouble opening [%s].", profile_name);
	G_fatal_error(err);
    }

    if ((fd_weight = G_open_cell_new(weight_name,mapset_out))==NULL)
    {
	char err[256];
	sprintf(err,"Trouble opening [%s].", weight_name);
	G_fatal_error(err);
    }
					/* Gather profiles for raster.	*/
    for (row=0;row<nrows;row++)
    {
	for (col=0; col<ncols; col++)
	    if (*(weight_ptr + row*ncols + col) == -1)
	    {
		gather_profile(dem_ptr,dem_prof,row,col,&ncells,profile);

		if (ncells >2)
 		{
  		    inter_spline(ncells,dem_prof,weight_prof,profile);
	   
		    G_percent(row,nrows,5);

		    fill_profile(dem_ptr,weight_ptr,dem_prof,weight_prof,
		  	         row,col,profile);
	    	}

	    }

	/* Save interpolated row to disk */
	G_put_map_row(fd_dem,dem_ptr + ncols*row);
	G_put_map_row(fd_weight,weight_ptr + ncols*row);
    }
    G_percent(nrows,nrows,1);


    /*------------------------------------------------------------------*/
    /*	    	      		CLOSE DOWN				*/
    /*------------------------------------------------------------------*/

    G_close_cell(fd_dem);
    G_close_cell(fd_weight);
    G_unopen_cell(fd_rcont);

    free(dem_ptr);
    free(weight_ptr);
    free(dem_prof);
    free(weight_prof);
}
