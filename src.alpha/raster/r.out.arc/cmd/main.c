#include "gis.h"
#include <math.h>
#include <stdio.h>

main(argc,argv) 
int   argc;
char  *argv[];
{
    CELL *cell;
    char *name; 
    char *mapset;
    int fd;
    int row,col;
    int nrows, ncols;
    double cellsize;
    int number;
    char fmt[25];
    struct
    {
	struct Option *map ;
	struct Option *digits ;
    } parm;
    struct
    {
	struct Flag *noheader;
        struct Flag *singleline;
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

    
  /* Added to optionaly produce a single line output.     -- emes -- 12.10.92 */
    flag.singleline = G_define_flag();
    flag.singleline->key = '1';
    flag.singleline->description = "List one entry per line. (The Guendra special)";
    

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
	printf ("ncols %d\n", region.cols);
	printf ("nrows %d\n", region.rows);
	if(G_projection() != 3)  /* Ist Projektion != LL (3) */
	{
	  G_format_easting (region.west, buf, region.proj);
	  printf ("xllcorner %s\n", buf);
	  G_format_northing (region.south, buf, region.proj);
	  printf ("yllcorner %s\n", buf);
	}
	else
	{
	  printf ("xllcorner %lf\n", region.west);
	  printf ("yllcorner %lf\n", region.south);
	}
	
	
	cellsize= fabs(region.east - region.west)/region.cols;
	printf("cellsize %lf\n",cellsize);
        	
    }

    if(flag.singleline->answer) /* -- emes -- 12.10.92 */ 
       strcat(fmt,"\n");
    
    for (row = 0; row < nrows; row++)
    {
	if (G_get_map_row (fd, cell, row) < 0)
	    exit(1);
	for (col = 0; col < ncols; col++)
           printf (fmt, (long)cell[col]);
        if(!flag.singleline->answer) /* -- emes -- 12.10.92 */          
	  printf ("\n");
    }
    exit(0);
}
