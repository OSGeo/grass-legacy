/* r.to.sites written by Markus Neteler 6. Sept. 1998
                         neteler@geog.uni-hannover.de
              code from r.out.ascii used and modified  */

#include "gis.h"
#include <stdio.h>

main(argc,argv) 
int   argc;
char *argv[];
{
    CELL *cell;
    struct Cell_head Wind;
    struct Categories cats;
    char *name; 
    char *mapset;
    int fd;
    int row,col;
    int nrows, ncols;
    int number;
    double northing, easting;
    char buf1[100];
    char buf2[100];
    char *site_name;
    FILE *out;
    char desc[100], desc_cats[100];
    char *label;
    struct Option *new;
    struct {
	struct Option *map ;
    } parm;

    parm.map = G_define_option() ;
    parm.map->key        = "input";
    parm.map->type       = TYPE_STRING;
    parm.map->required   = YES;
    parm.map->gisprompt  = "old,cell,raster" ;
    parm.map->description= "Name of an existing raster map" ;
                      
    new = G_define_option();
    new->key	= "output";	
    new->type	= TYPE_STRING;	
    new->required	= YES;	
    new->multiple	= NO;	
    new->gisprompt	= "new,site_lists,sites";
    new->description	= "sites file to be made from raster file";	
    
    G_gisinit (argv[0]);

    if (G_parser(argc, argv))
       	exit (-1);

    name = parm.map->answer;
    site_name = new->answer;

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

    G_get_window (&Wind);
    nrows = Wind.rows;
    ncols = Wind.cols;
    out = G_fopen_sites_new (site_name);
    fprintf (stderr, "Creating site file with category value, but without category label\n");
    
    for (row = 0; row < nrows; row++)
	{
	   G_percent(row, nrows, 2);
	   northing = Wind.north - (row * Wind.ns_res);
	   G_format_northing (northing, buf1, -1);
	    if (G_get_map_row (fd, cell, row) < 0)
		exit(1);

	    for (col = 0; col < ncols; col++)
	    {
		easting = Wind.west + (col * Wind.ew_res);
		G_format_easting (easting, buf2, -1);
		
             /* convert the coordinates to strings */
		sprintf (desc, "#%ld ",(long) cell[col]);

/* not yet working :-( */
             /* get the cats and add them with '|' delimiter to desc */
/*  		desc_cats[0]='|';  
 * 		label = G_get_cat(cell[col],cats);
 *		strcat(desc_cats,label);
 *		strcat(desc,desc_cats);
 */
	     /* test output */
	/*	fprintf (stderr,"%s %s %s\n", buf2, buf1, desc); */
	
	     /* write the sites file */
		G_put_site (out, easting, northing, desc);
	    }
	}
    fclose (out);
    exit(0);
}
