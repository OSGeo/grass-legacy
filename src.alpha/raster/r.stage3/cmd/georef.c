/* Program to convert site_list to map */
#include 	"gis.h"
#include 	"stage3.h"
#define RESOLUTION "0:02:59"
static 	struct Cell_head	window;
static int out_fd;
extern int verbose;

void
map_initialize(mapname)
char 		*mapname;
{

	double resolution;


	if (G__get_window (&window, "", "WIND", G_mapset()) < 0) {
				G_get_default_window (&window);
				G_put_window (&window);
	}

	G_scan_resolution(RESOLUTION,&resolution,G_projection());
	window.ns_res = resolution;
	window.ew_res = resolution;
	G_set_window(&window);
	G_get_set_window(&window);

	out_fd = G_open_cell_new_random(mapname);
	if(out_fd == NULL)
	{
		fprintf(stderr,"%s - Cannot create raster file [%s]",G_program_name(),mapname);
		exit(1);
	}
}

void
place_points_in_map(easting, northing, value)
double easting, northing, value;
{
	CELL val;
	int row,col;
	int in_window = 1;
	double G_northing_to_row(), G_easting_to_col();

		row = (int) G_northing_to_row(northing,&window);
		col = (int) G_easting_to_col(easting,&window);
		if((row < 0||row >= window.rows)||(col < 0||col >= window.cols))
			in_window = 0;
		else
			in_window = 1;
		if(in_window){
			val = (CELL)value;
			G_put_map_row_random(out_fd,&val,row,col,1);
		}
}

void
close_map(mapname)
char 	*mapname;
{
	char command[512];
	struct Colors colors;
	struct Range range;
	CELL max, min;

		G_close_cell(out_fd);
		G_read_range(mapname, G_mapset(), &range);
		G_get_range_min_max(&range, &min, &max);
		G_init_colors(&colors);
		/* Write Color table */
		G_make_rainbow_colors(&colors, min, max);
		G_write_colors(mapname,G_mapset(),&colors);
		if(verbose){	
			if(*title)
				fprintf(stderr,"\nTitle: %s\n", title);
			fprintf(stderr,"\nColor table for map [%s] set to rainbow.\n", mapname);
			fprintf(stderr,"Maximum=%d, Minimum=%d\n",max,min);
			fprintf(stderr,"Compressing raster file [%s]\n",mapname);
		}
		
		sprintf(command,"r.compress %s > /dev/null",mapname);
		system( command);

		if(verbose)	
			fprintf(stderr,"Raster Map [%s] created\n",mapname);
		
		if (*title)
			G_put_cell_title (mapname, title);
		
}
