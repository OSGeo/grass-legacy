/************************************************************************/
/*			  	  comatrix()				*/
/* Function to calulate various co-occurrence matrix texture measures.	*/
/* Jo Wood, V1.0 December, 1992, V1.1 October, 1995.			*/
/*									*/
/************************************************************************/

#include "lags.h"

comatrix()
{
    /*------------------------------------------------------------------*/
    /*				INITIALISE			     	*/
    /*------------------------------------------------------------------*/

    CELL    *raster,		/* Buffer hold one raster row.		*/
	    zi,zj,		/* The two raster values to compare.	*/
	    zmin,zmax,		/* Range of raster values.		*/
	    *con_row,		/* Buffers storing texture measures.	*/
	    *asmo_row,
	    *ent_row,
	    *asym_row,
	    *idm_row;

    int	    row,col,
            nrows,ncols,	/* Number of cell rows and columns.	*/
	    xoffset,yoffset,	/* 2D lag vector displacement.		*/
	    *cocr,		/* Raster to hold co-occurrence matrix.	*/
	    n;			/* The number of pairs to consider.	*/

    double  con,		/* The five texture measures.		*/
	    asmo,
	    ent,
	    asym,
	    idm;

    nrows = G_window_rows();	/* Find dimensions of raster.		*/
    ncols = G_window_cols();

				/* Reserve memory for entire raster	*/
				/* co-occurrence matrix, and texture	*/
				/* measures.				*/
    raster = (CELL*) G_malloc(nrows*ncols*sizeof(CELL));
    cocr   = (int *) G_malloc(GLEVS*GLEVS*sizeof(int));

    con_row  = G_allocate_cell_buf();
    asmo_row = G_allocate_cell_buf();
    ent_row  = G_allocate_cell_buf();
    asym_row = G_allocate_cell_buf();
    idm_row  = G_allocate_cell_buf();


    /*------------------------------------------------------------------*/
    /*		 READ IN RASTER AND CREATE CO-OCCURRENCE MATRIX		*/
    /*------------------------------------------------------------------*/


    printf("STAGE ONE - Reading in data\n");

    for (row=0;row<nrows; row++)
	G_get_map_row(fd_in,raster+ncols*row,row);


    printf("STAGE TWO - Calculating co-occurrence matrix\n");


    for(yoffset=-1*(nrows/2);yoffset<(nrows/2);yoffset++)
    {
    	for(xoffset=-1*(ncols/2);xoffset<(ncols/2);xoffset++)
	{
	    G_percent(yoffset+(nrows/2),nrows,1);

	    /* Find range of data. */
	    /* ------------------- */

	    init_matrix(cocr,GLEVS);	/* Initialise co-occurence matrix */
	    n    = 0;			/* Reset counters.		  */
	    con  = 0.0;
	    asmo = 0.0;
	    ent  = 0.0;
	    asym = 0.0;
	    idm  = 0.0;
	    zmin =  999999;
	    zmax = -999999;

	    for (row=0;row<nrows;row++)
	 	if ((row+yoffset<nrows) && (row+yoffset>=0))
                    for (col=0;col<ncols;col++)
		   	if ((col+xoffset<ncols) && (col+xoffset>=0))
		 	{
			    n++;
			    zi = *(raster + row*ncols + col);
			    zj = *(raster + (row+yoffset)*ncols + col+xoffset);

			    if ( MIN(zi,zj) < zmin)
				zmin = MIN(zi,zj);

			    if (MAX(zi,zj) > zmax)
				zmax = MAX(zi,zj);
			}

	    /* Calculate co-occurrence matrix. */
	    /* ------------------------------- */

	    for (row=0;row<nrows;row++)
	 	if ((row+yoffset<nrows) && (row+yoffset>=0))
                    for (col=0;col<ncols;col++)
		   	if ((col+xoffset<ncols) && (col+xoffset>=0))
		 	{
			    zi =quant(*(raster + row*ncols + col),
							zmin,zmax,GLEVS);
			    zj =quant(*(raster+(row+yoffset)*ncols+col+xoffset),
							zmin,zmax,GLEVS);

			    *(cocr + zj*GLEVS + zi) = *(cocr + zj*GLEVS + zi)+1;
			}

            /* Calculate matrix statistics. */
            /* ---------------------------- */

            for(row=0; row<GLEVS; row++)
                for (col=0; col<GLEVS; col++)
                {
                    con  += contrast(row,col,
				(double) *(cocr + row*GLEVS + col)/(double)n); 
                    asmo += asmoment(
				(double) *(cocr + row*GLEVS + col)/(double)n); 
                    ent  += entropy(
				(double) *(cocr + row*GLEVS + col)/(double)n); 
                    asym += asymmetry(
				(double) *(cocr + col*GLEVS + row)/(double)n,
				(double) *(cocr + row*GLEVS + col)/(double)n); 
                    idm  += idmoment(row,col,
				(double) *(cocr + row*GLEVS + col)/(double)n); 
                }

	    con_row[xoffset+(ncols/2)] = rint(10*con);
	    asmo_row[xoffset+(ncols/2)] = rint(1000*asmo);
	    ent_row[xoffset+(ncols/2)] = rint(1000*ent);
	    asym_row[xoffset+(ncols/2)] = rint(1000*asym);
	    idm_row[xoffset+(ncols/2)] = rint(1000*idm);

	}

	/* Output raster rows. */
	/* ------------------- */

	G_put_map_row(fd_con,con_row);
	G_put_map_row(fd_asmo,asmo_row);
	G_put_map_row(fd_ent,ent_row);
	G_put_map_row(fd_asym,asym_row);
	G_put_map_row(fd_idm,idm_row);

    }

    free(raster);
    free(cocr);
}

