#include <stdio.h>
#include <math.h>
#include "define.h"
#include "global.h"

/*****************************************************************************/
/*  Program to soil parameters based on texture.                             */
/*****************************************************************************/

soil_param(half_basin, soilmap, rockmap, mapset)

int **half_basin;
char *rockmap; 
char *soilmap; 
char *mapset;

{
/*
 *  Matricies
 */
    int *soil;
    int *rock;

    CELL *cell_rock;
    CELL *cell_soil;

    int fd_rock;
    int fd_soil;

    int col;
    int ee;
    int row;
/*
 *  Open the file with the soil texture indicies and soil rockiness:
 */
    fd_rock = G_open_cell_old (rockmap, mapset);
    if (fd_rock < 0)
    	exit(1);
    fd_soil = G_open_cell_old (soilmap, mapset);
    if (fd_soil < 0)
    	exit(1);
/*
 *  Allocate space for and initialize soil texture indicies:
 */
    soil = ivector(0,num_ele+1);
    rock = ivector(0,num_ele+1);

    for(ee=1;ee<=num_ele;ee++) {
	soil[ee] = -1;
	rock[ee] = -1;
    }
/*
 *  Open up a vector that is just long enough to hold one row of data.
 */
    cell_rock = G_allocate_cell_buf();
    cell_soil = G_allocate_cell_buf();
/*
 *  Read in data and assign to soil vector:
 */
    for(row=(nrows-1);row>=0;row--) {
	if(G_get_map_row (fd_soil, cell_soil, row) < 0)
	    exit(1);
	if(G_get_map_row (fd_rock, cell_rock, row) < 0)
	    exit(1);
	for (col = 0; col < ncols; col++) {
/*
 *  Find element number for this cell:
 */
	    ee = index(half_basin[row][col],PLANE);

	    if((soil[ee] != -1) || (rock[ee] != -1)) {
		if(soil[ee] != (int)cell_soil[col]) {
		    printf("\n ERROR: different soil types assigned to a single element");
		    exit(0);
		}
		if(rock[ee] != (int)cell_rock[col]) {
		    printf("\n ERROR: different rock types assigned to a single element");
		    exit(0);
		}
	    }
	    else {
		rock[ee] = (int)cell_rock[col];
		soil[ee] = (int)cell_soil[col];
	    }
	}
    }
    G_close_cell(fd_soil);
    G_close_cell(fd_rock);
/*
 *  Assign infiltration parameters to elements based on soil textures and rockiness.
 */
    for(ee=1;ee<=num_ele;ee++) {
	if(soil[ee] != -1) {
	    if((soil[ee] >= min_soil) && (soil[ee] <= max_soil)) {
	        element[ee].porosity = porosity[soil[ee]];
	        element[ee].Sint = Sint[soil[ee]];
	        element[ee].Smax = Smax[soil[ee]];
	        element[ee].fmin = fmin[soil[ee]]*(1.0 - ((float)rock[ee]/100.0));
	        element[ee].G    = G[soil[ee]];
	        element[ee].Rock = (float)rock[ee]/100.0;
	    }
	    else {
	        printf("\n ERROR: soil texture not properly specified");
		exit(0);
	    }

	    printf("\n ee = %4d",ee);
	    printf(" por = %8.5f",element[ee].porosity);
	    printf(" Sint = %8.5f",element[ee].Sint);
	    printf(" Smax = %8.5f",element[ee].Smax);
	    printf(" fmin = %8.5f",element[ee].fmin);
	    printf(" G = %8.5f",element[ee].G);
	    printf(" Rock = %8.5f",element[ee].Rock);
	}
    }
}
