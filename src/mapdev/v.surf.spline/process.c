/**************************************************************************/
/***									***/
/***                               process()				***/
/*** Function to process vector for spline interpolation.		***/
/*** Jo Wood, Department of Geography, V1.0 December 1991		***/
/*** Modified for rooks-case rasterisation, V1.2, 7th March, 1992	***/
/*** Modified for standard GRASS module structure, V2.0, 23rd July 1995	***/
/*** 									***/
/**************************************************************************/

#include "spline.h"

process()
{
    /*------------------------------------------------------------------*/
    /*				INITIALISE				*/
    /*------------------------------------------------------------------*/

    char 	cmd[127],		/* System command buffer.	*/
		rcont_name[127],	/* Rasterized contour file.	*/
		temp[127],		/* Temporary file name.		*/
		*rmapset;		/* Raster mapset.		*/

    int 	new_ncols,
		profile,
		nprofiles=4,
		fd_new;

    nrows = G_window_rows(); 
    ncols = G_window_cols(); 

    /*------------------------------------------------------------------*/
    /*		   RASTERIZE CONTOURS USING GRASS FUNCTION		*/
    /*------------------------------------------------------------------*/

    /* Create temporary rasterized contour layer */
    strcpy(rcont_name,rast_out_name);
    strcat(rcont_name,".contours");   

    printf("\n\nSTEP 1 - Rasterizing vectors\n         ");
    sprintf(cmd,"v.to.rast input=%s output=%s",vect_in_name,rcont_name);
    system(cmd);

    if (rooks)			/* rasterise using rooks case adjacency	*/
    {
    	sprintf(cmd,"rooks.sh %s",rcont_name);
    	system(cmd);
    }


    /*------------------------------------------------------------------*/
    /*	 		INTERPOLATE AND ROTATE CONTOURS			*/
    /*------------------------------------------------------------------*/


    /* 1. INTERPOLATE ALONG EDGES AND STORE RASTERIZED CONTOURS */
    interpol_edges(rcont_name);


    /* 2. INTERPOLATE AND STORE PROFILES */
    for (profile=1;profile<=nprofiles;profile++)
    	interpol_layer(rast_out_name,rcont_name,profile);


    /* 3. CALCULATE FINAL INTERPOLATED MAP AND RMSE SURFACE */
    printf("\n\nSTEP 4 - Calculating weighted average of surfaces and RMSE \n\t");
    sprintf(cmd,"rmse.sh %s",rast_out_name);
    system(cmd);

    /* 4. REMOVE TEMPORARY FILES */
    sprintf(cmd,"tidy.sh %s",rast_out_name);
    system(cmd); 

}
