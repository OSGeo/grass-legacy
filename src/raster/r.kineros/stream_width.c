#include <stdio.h>
#include <math.h>
#include "define.h"
#include "global.h"

/*****************************************************************************/
/*  Program to soil parameters based on texture.                             */
/*****************************************************************************/

stream_width(stream, map, mapset)

int **stream;
char *map; 
char *mapset;

{
    CELL *cell;

    int fd;

    int col;
    int ee;
    int row;
/*
 *  Open the file with the stream widths:
 */
    fd = G_open_cell_old (map, mapset);
    if(fd < 0)
    	exit(1);
/*
 *  Initialize data:
 */
    for(ee=1;ee<=num_ele;ee++)
	element[ee].stream_width = 0.0;
/*
 *  Open up a vector that is just long enough to hold one row of data.
 */
    cell = G_allocate_cell_buf();
/*
 *  Read in data and assign to element:
 */
    for (row=(nrows-1); row>=0; row--) {
	if(G_get_map_row (fd, cell, row) < 0)
	    exit(1);
	for (col = 0; col < ncols; col++) {
/*
 *  Find element number for this cell:
 */
	    ee = index(stream[row][col],STREAM);

	    if(ee >  0) {
	        if(element[ee].stream_width != 0.0) {
		    if(element[ee].stream_width != (float)cell[col]) {
		          printf("\n ERROR: different stream widths assigned to a single element");
		        exit(0);
		    }
	        }
	        else
		    element[ee].stream_width = (float)cell[col];
	    }
	}
    }
    G_close_cell(fd);
}
