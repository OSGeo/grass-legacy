/***************************************************************************/
/***                                                                     ***/
/***                             centroid()                              ***/
/***   Finds the centre of gravity of a group of adjacent raster cells.  ***/
/***                  Jo Wood, V1.0, 18th July, 1995			 ***/
/***                                                                     ***/
/***************************************************************************/

#include "feature.h"

centroid(raster,row,col)
    CELL *raster;
    int row,col;
{

    /*------------------------------------------------------------------*/
    /*                              INITIALISE                         	*/
    /*------------------------------------------------------------------*/ 

    CELL attrib;			/* attribute to look for.	*/

    attrib = *(raster + row*ncols + col);
    n = 0;
    xbar = 0;
    ybar = 0;
    

    /*------------------------------------------------------------------*/
    /*                      SEARCH FOR SIMILAR CELLS                   	*/
    /*------------------------------------------------------------------*/ 

    search(raster,attrib,row,col);	/* Call to recursive function 	*/

    xbar = (xbar + n/2) /n;		/* Average x and y coords.	*/
    ybar = (ybar + n/2) /n;
    *(raster + ybar*ncols + xbar) = attrib;	/* Allocate centroid.	*/
}



search(raster,attrib,row,col)
    CELL *raster,attrib;
    int row,col;
{
    xbar += col;			/* Update centroid coordinates */
    ybar += row;
    n++;
    *(raster + row*ncols + col) = (CELL)0;	/* Set cell to 0.	 */

    if ((col > 0) && (*(raster + row*ncols + col-1) == attrib))
	search(raster,attrib,row,col-1);
    
    if ((col < ncols-1) && (*(raster + row*ncols + col+1) == attrib))
	search(raster,attrib,row,col+1);

    if ((row > 0) && (*(raster + (row-1)*ncols + col) == attrib))
	search(raster,attrib,row-1,col);

    if ((row < nrows-1) && (*(raster + (row+1)*ncols + col) == attrib))
	search(raster,attrib,row+1,col);

    if ((col > 0) && (row > 0) && (*(raster + (row-1)*ncols + col-1) == attrib))
	search(raster,attrib,row-1,col-1);

    if ((col > 0) && (row < nrows-1) && (*(raster + (row+1)*ncols + col-1) == attrib))
	search(raster,attrib,row+1,col-1);

    if ((col < ncols-1) && (row > 0) && (*(raster + (row-1)*ncols + col+1) == attrib))
	search(raster,attrib,row-1,col+1);

    if ((col < ncols-1) && (row < nrows-1) && (*(raster+(row+1)*ncols+col+1) == attrib))
	search(raster,attrib,row+1,col+1);


}

