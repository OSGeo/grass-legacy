#include "gis.h"
#include <stdio.h>

main(argc,argv) 
int   argc;
char *argv[];
{
    CELL *cell;
    char *name; 
    char *mapset;
    int fd;
    int row,col;
    int nrows, ncols;
    int number;
    char fmt[20];
    struct
    {
	struct Option *map ;
	struct Option *digits ;
    } parm;
    struct
    {
	struct Flag *noheader;
    } flag;

/* Define the different options */

    parm.map = G_define_option() ;
    parm.map->key        = "map";
    parm.map->type       = TYPE_STRING;
    parm.map->required   = YES;
    parm.map->gisprompt  = "old,cell,raster" ;
    parm.map->description= "Name of an existing raster map" ;

    parm.digits = G_define_option() ;
    parm.digits->key        = "digits";
    parm.digits->type       = TYPE_INTEGER;
    parm.digits->required   = NO;
    parm.digits->description= "The minimum number of digits (per cell) to be printed";

    flag.noheader = G_define_flag();
    flag.noheader->key = 'h';
    flag.noheader->description = "Suppress printing of header information";


    G_gisinit (argv[0]);

    if (G_parser(argc, argv))
       	exit (-1);

    strcpy (fmt, "%ld ");
    if (parm.digits->answer != NULL)
    {
        sscanf (parm.digits->answer, "%ld", &number);
        if (number > 0)
            sprintf (fmt, "%%%dld ", number);
	else
	    sprintf (fmt, "%%%dld", number);
    }
	
    name = parm.map->answer;
    mapset = G_find_cell2 (name, "");
    if (mapset == NULL)
    {
        char msg[100];	
		
	sprintf (msg, "%s: <%s> cellfile not found\n", G_program_name(), name);
		G_fatal_error (msg);
        exit(1);
    }

    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
    	exit(1);

    cell = G_allocate_cell_buf();

    nrows = G_window_rows();
    ncols = G_window_cols();

    if (!flag.noheader->answer)
    {
	struct Cell_head region;
	char buf[128];

	G_get_window (&region);
	G_format_northing (region.north, buf, region.proj);
	printf ("north: %s\n", buf);
	G_format_northing (region.south, buf, region.proj);
	printf ("south: %s\n", buf);
	G_format_easting (region.east, buf, region.proj);
	printf ("east: %s\n", buf);
	G_format_easting (region.west, buf, region.proj);
	printf ("west: %s\n", buf);

	printf ("rows: %d\n", region.rows);
	printf ("cols: %d\n", region.cols);
    }


    for (row = 0; row < nrows; row++)
    {
	if (G_get_map_row (fd, cell, row) < 0)
	    exit(1);
	for (col = 0; col < ncols; col++)
	    printf (fmt, (long)cell[col]);
	printf ("\n");
    }
    exit(0);
}
