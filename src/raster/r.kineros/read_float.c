#include <stdio.h>
#include "define.h"
#include "global.h"

/*****************************************************************************/
/*  Program to determing the topology of a stream network.  A file is        */
/*  generated that reports the tributaries that are joined at the head       */
/*  of each stream segement.                                                 */
/*****************************************************************************/

read_float(map, mapset, data)

char *map;
char *mapset;
float **data;

{
/*
 *  Matricies
 */
    CELL *cell;

    int fd;

    int col;
    int row;

/*****************************************************************************/
/*  Open the cell file "map" in "mapset".                                    */
/*****************************************************************************/

    fd = G_open_cell_old (map, mapset);
    if (fd < 0)
    	exit(1);
    else {
        printf("\n\n Opening:");
        printf("\n     map    =  %s",map);
        printf("\n     mapset =  %s",mapset);
        printf("\n\n");
    }

/*****************************************************************************/
/*  Open up a vector that is just long enough to hold one row of data.       */
/*****************************************************************************/
 
    cell = G_allocate_cell_buf();

/*****************************************************************************/
/*  Read in data.                                                            */
/*****************************************************************************/

    for (row=(nrows-1); row>=0; row--) {
	if(G_get_map_row (fd, cell, row) < 0)
	    exit(1);
	for (col = 0; col < ncols; col++)
	    data[row][col] = (float)cell[col];
    }
    G_close_cell(fd);
}
