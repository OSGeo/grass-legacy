/**************************************************************************/
/***                       interpol_edges()                             ***/
/*** Function to interpolate along raster contour edges.                ***/
/*** Jo Wood, V1.0, December, 1991, V1.2 March 7th, 1992                ***/
/*** V2.0 Modified to conform to GRASS module strcture, 23rd July, 1995 ***/
/***                                                                    ***/
/**************************************************************************/

#include "spline.h"

interpol_edges(rcont_name)

char	*rcont_name;
{
    /*------------------------------------------------------------------*/
    /*                          INITIALISE                              */
    /*------------------------------------------------------------------*/
 
    char 		rcont_temp[127],
			cmd[127],
			*rmapset;

    int 		row,col,
			fd_in,
			fd_out;

    CELL		*cont_row,
			*weight_row,
			*weight_col,
			*cont_west,
			*cont_east;

    printf("\nSTEP 2 - Calculating splines for edges of contour map      ");
     
    /*------------------------------------------------------------------*/
    /*	    OPEN RASTER FILE CONTAING CONTOURS FOR INPUT AND OUTPUT	*/
    /*------------------------------------------------------------------*/

    if ((rmapset=G_find_cell(rcont_name,"")) == NULL)
    {
	char err[256];
	sprintf(err,"Rasterized contours [%s] could not be opened\n",rcont_name);
	G_fatal_error(err);
    }

    sprintf(rcont_temp,"%s.temp",rcont_name);

    fd_in   = G_open_cell_old(rcont_name,rmapset);
    fd_out  = G_open_cell_new(rcont_temp,rmapset);


    /*------------------------------------------------------------------*/
    /*     ALLOCATE MEMORY FOR BUFFERS HOLDING EDGE INFORMATION         */
    /*------------------------------------------------------------------*/

    cont_row	= G_allocate_cell_buf();
    cont_west	= (CELL *) malloc(nrows*sizeof(CELL));
    cont_east	= (CELL *) malloc(nrows*sizeof(CELL));
    weight_row	= G_allocate_cell_buf();
    weight_col	= (CELL *) malloc(nrows*sizeof(CELL));
  
 
    /*------------------------------------------------------------------*/
    /*    	     READ IN BOUNDARY CONTOUR INFORMATION		*/
    /*------------------------------------------------------------------*/

    for (row=0; row<nrows; row++)
    {
    	G_get_map_row(fd_in,cont_row,row);

    	*(cont_west+row) = *(cont_row);
    	*(cont_east+row) = *(cont_row+ncols-1);
    }

    inter_spline(nrows,cont_west,weight_col,3);
    inter_spline(nrows,cont_east,weight_col,3);


    /*------------------------------------------------------------------*/
    /*	     	   STORE CONTOURS AND INTERPOLATED EDGES		*/
    /*------------------------------------------------------------------*/

    G_get_map_row(fd_in,cont_row,0);		/* North edge.		*/
    inter_spline(ncols,cont_row,weight_row,1);

    						/* Corners are average  */
						/* of intersecting edge	*/
						/* profiles.		*/
    *cont_row = (*cont_row + *cont_west)/2;
    *(cont_row + ncols-1) = (*(cont_row + ncols-1) + *cont_east)/2;


    G_put_map_row(fd_out,cont_row);

    for (row=1;row<nrows-1;row++)		/* Most of raster.	*/
    {
    	G_get_map_row(fd_in,cont_row,row);
	*(cont_row)		= *(cont_west+row);
	*(cont_row + ncols-1)	= *(cont_east+row);
    	G_put_map_row(fd_out,cont_row);
    }

    G_get_map_row(fd_in,cont_row,nrows-1);	/* South edge.		*/
    inter_spline(ncols,cont_row,weight_row,1);
    						/* Corners are average  */
						/* of intersecting edge	*/
						/* profiles.		*/
    *cont_row = (*cont_row + *(cont_west+nrows-1))/2;
    *(cont_row + ncols-1) = (*(cont_row + ncols-1) + *(cont_east+nrows-1))/2;

    G_put_map_row(fd_out,cont_row);


    /*------------------------------------------------------------------*/
    /*				CLOSE DOWN				*/
    /*------------------------------------------------------------------*/

    G_close_cell(fd_in);
    G_close_cell(fd_out);

    sprintf(cmd,"g.remove %s\n",rcont_name);
    system(cmd);
    sprintf(cmd,"g.rename %s,%s\n",rcont_temp,rcont_name);
    system(cmd);

    free(cont_row);
    free(weight_row);
    free(weight_col);
    free(cont_west);
    free(cont_east);
}
   
	
