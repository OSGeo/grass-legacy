#include <gis.h>
#include <stdio.h>
#include "dtedgis.h"

/*
 *  This could be faster by using seeks to avoid reading the
 *  entire column record to fill the cached block, but then
 *  we wouldn't be able to make use of the checksum, so I
 *  wrote it the slow, precise way.  The DTED may be read
 *  several times, depending on the value of BLOCK_ROWS and
 *  the number of rows in the file.  The latest version of
 *  GRASS didn't support random writing, although earlier 
 *  versions did - so we avoid it here.
*/

int do_read_write(dtedfp, grassfd, nrows, ncols, quiet, ebuf, pole_flag)
FILE *dtedfp;
int grassfd;
int nrows, ncols;
int quiet, pole_flag;
char *ebuf;
{
    CELL *cell;
    register int row,col,i,j;
    dted_d *write_block, *dbuf, val;
    int brow, erow, cacherows;
    int pole_skip;

    /* see get_header for explanation */
    switch(pole_flag){
	case NORTH_POLE:
	    pole_skip = 0;        /* GRASS row to leave out */
	    nrows++;
	    break;
	case SOUTH_POLE:
	    pole_skip = nrows-1;  /* GRASS row to leave out */
	    nrows++;
	    break;
	default:
	    break;
    }
    
    /* allocate one GRASS row */
    cell = G_allocate_cell_buf();

    /* allocate a block of dted_d to hold a "ribbon" of
       2s complement data values */
    if(NULL == (write_block = 
	(dted_d *)malloc(BLOCK_ROWS*ncols*sizeof(dted_d)))){
	sprintf(ebuf,"Unable to allocate buffer\n");
	return(-2);
    }

    /* allocate a block of dted_d to hold data from a single record */ 
    if(NULL == (dbuf = 
	(dted_d *)malloc(nrows*sizeof(dted_d)))){
	sprintf(ebuf,"Unable to allocate buffer\n");
	return(-2);
    }

    if (!quiet) fprintf(stderr, "Percent Complete: ");

    for (row = 0; row < nrows; row++)
    {
	    if(!quiet) G_percent(row, nrows, 5);

	    if(!(row%BLOCK_ROWS)){
		if(row) {
		    erow = brow-1;
		    brow = (erow < BLOCK_ROWS? 0: brow-BLOCK_ROWS);
		}
		else {
		    erow = nrows-1;
		    brow = (nrows <= BLOCK_ROWS? 0: nrows-BLOCK_ROWS);
		}
		cacherows = erow - brow +1;
		fseek(dtedfp,FIRST_REC,SEEK_SET);
		for (col = 0; col < ncols; col++){
		    if(col != read_record(dtedfp, nrows, dbuf)){
			sprintf(ebuf,"Read error column %d\n", col);
			free(write_block);
			free(dbuf);
			free(cell);
			return (-1);
		    }
		    i = (col*BLOCK_ROWS)+(BLOCK_ROWS-cacherows);
		    for(j=brow; j<=erow;  j++, i++){
			write_block[i] = dbuf[j];
		    }
		}
	    }

	    if(pole_flag && pole_skip == row) continue;
	    /* copy values from data block to current row, converting
	       dted nulls to GRASS nulls */
	    for (col = 0; col < ncols; col++){
		val=write_block[(col*BLOCK_ROWS)+
				(BLOCK_ROWS-1-(row%BLOCK_ROWS))];
		if(val == DTED_NULL)
		    G_set_c_null_value ((cell+col), 1);
		else
		    cell[col] = val;
	    }

	    if (G_put_raster_row(grassfd, cell, CELL_TYPE) < 0 ){
		sprintf(ebuf, "Can't write new raster row!!");
		free(write_block);
		free(dbuf);
		free(cell);
		return(-1);
	    }
    }
    if(!quiet) G_percent(row,nrows, 5);

    free(write_block);
    free(dbuf);
    free(cell);

    return(1);
}
