/******************************************************************************
	Reads the header lines of CTG file format and places information in 
	the Cell_head structure.	

	The map_code indicates which overlays have been included in the 
	CTG data file.  The code is formed by the addition of the separate
	GISRAS map type codes for each of the overlays as defined by USGS.

	The function will return the number of overlays contained the 
	CTG file.  0 will be returned if no overlays exist.

	NOTE:  map_name must be declared with a length of a least 80.
*******************************************************************************/
#include <math.h>
#include <gis.h>

#define WINDOW_FORMAT	0  /* 1 byte of information */
#define COMPRESSION	1  /* compression           */

char buffer[255];

int read_ctg_header(window,map_name,map_code)
	struct Cell_head *window;
	char *map_name;
	int  *map_code;
{
	int num, resolution, north, east;

	/* Set Default Information */
	window->format = WINDOW_FORMAT; 
	window->compressed = COMPRESSION;

	fscanf(stdin,"%d %*d %d %*d %d %d %d %d %d %*d %*d\n",&window->rows,
		&window->cols,&resolution,&num,map_code,&window->zone,
		&window->proj);

	window->ew_res = window->ns_res = resolution;
	
	fgets(buffer,81,stdin);
	fgets(buffer,81,stdin);
	fscanf(stdin,"%*d %*d %*d %*d %d %d %*d\n",&east,&north);
	window->north = north, window->west = east;
	window->south = window->north - (window->ns_res * window->rows);
	window->east  = window->west  + (window->ew_res * window->cols);
	gets(map_name,80);
	return(num);
}
