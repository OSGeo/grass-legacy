/* This function extracts cell values from grass raster maps. Uses GRASS4.0 libraries */
#include "gis.h"
#define FALSE 0
#define TRUE 1
void 
get_cell_values (double *east, double *north, int max, double *value_array, char *mapname)
{
	char *mapset;
	int fd;
	int i,row,col;
	CELL *cell;
	struct Cell_head window;
	double G_northing_to_row(),G_easting_to_col();

	mapset = G_find_cell(mapname,"");
	fd = G_open_cell_old(mapname,mapset);
	if(fd < 0){
		fprintf(stderr,"%s map does not exist in the current mapset\n",mapname);
		exit(1);
	}
	cell = G_allocate_cell_buf();
	G_get_window(&window);
	if(G_set_window(&window) == -1)
		G_fatal_error("can't set current graphics window");
	G_get_set_window(&window);
	for(i=0;i<max;i++){
		row =(int) G_northing_to_row(north[i],&window);
		col =(int) G_easting_to_col(east[i],&window);
		G_get_map_row(fd,cell,row);
		value_array[i] = cell[col];
	}
	G_close_cell(fd);
}

/* Program to read constant const_values value */
void 
const_value (char *variable, int maximum, double *array, double uplim, double lowlim)
{	
	char line[31];
	int i,ret = FALSE;
	double const_value;

	do{
		fprintf (stdout,"\n%s(%g - %g):",variable,lowlim,uplim);
		if(!fgets(line,30,stdin)) exit(2);
		ret = sscanf(line,"%lf",&const_value);
		if(const_value < lowlim || const_value > uplim){
			fprintf(stderr,"**<%g>**Invalid value\n",const_value);
			ret = FALSE;
		}
	}while(ret != TRUE );

	for(i=0;i<maximum;i++)
		array[i] = const_value;
}
