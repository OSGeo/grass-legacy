#include <gis.h>
#include <stdio.h>
#include "dtedgis.h"

int do_read_write(dtedfp, grassfd, nrows, ncols, quiet, ebuf)
FILE *dtedfp;
int grassfd;
int nrows, ncols;
int quiet;
char *ebuf;
{
    CELL *cell;
    register int row,col,i,j;
    dted_d *write_block, *dbuf, val;
    int brow, erow, cacherows;
    
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
		fseek(dtedfp,3428L,SEEK_SET);
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
	    if (G_put_map_row(grassfd, cell) < 0 ){
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
