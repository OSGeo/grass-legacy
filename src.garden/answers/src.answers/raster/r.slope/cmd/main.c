#include "gis.h"

/* Chris Rewerts and Raghaven Srinivasan
   Agricultural Engineering, Purdue University

   This is a modification of r.slope.aspect
   
   The purpose is to compute slope percent in the format ANSWERS
   requires (slope * 10; i.e. a slope of 1.1% will be a cat value
   of 11)

*/

main (argc, argv) char *argv[];
{
    int verbose;
    int elevation_fd;
    int slope_fd ;
    int i,j;
    int nrows, row;
    int ncols;
    long int c1, c2, c3, c4, c5, c6, c7, c8;
    CELL *elev_cell[3], *temp;
    CELL *slope_cell;
    double sqrt();
    double H,V;
    double p;              /* slope in ew direction */
    double q;              /* slope in ns direction */
    double key;
    float cat_label;
    char *elev_name;
    char *slope_name;
    char buf[300];
    char *mapset;
    struct Cell_head window;
    struct Categories cats;
    struct
    {
	struct Option *elevation, *slope;
    } parm;
    struct
    {
	struct Flag *q;
    } flag;

    parm.elevation = G_define_option() ;
    parm.elevation->key        = "elevation" ;
    parm.elevation->type       = TYPE_STRING ;
    parm.elevation->required   = YES ;
    parm.elevation->gisprompt  = "old,cell,raster" ;
    parm.elevation->description= "Raster elevation file name";

    parm.slope = G_define_option() ;
    parm.slope->key        = "slope" ;
    parm.slope->type       = TYPE_STRING ;
    parm.slope->required   = YES ;
    parm.slope->answer     = NULL ;
    parm.slope->gisprompt  = "any,cell,raster" ;
    parm.slope->description= "Output slope filename" ;

    flag.q = G_define_flag() ;
    flag.q->key         = 'q' ;
    flag.q->description = "Quiet" ;

    G_gisinit (argv[0]);

    if (G_parser(argc, argv))
        exit(-1);

    verbose = (!flag.q->answer);

    elev_name = parm.elevation->answer;
    slope_name = parm.slope->answer;

    if (slope_name == NULL)
    {
	fprintf (stderr, "\nYou must specify the parameter <%s>\n",
	    parm.slope->key);
	G_usage();
	exit(1);
    }

    /* check elevation file existence */
    mapset = G_find_cell2(elev_name, "");
    if (!mapset)
    {
        sprintf (buf, "elevation file <%s> not found\n", elev_name);
        G_fatal_error (buf);
        exit(1);
    }
    
    G_get_set_window (&window);

    nrows = G_window_rows();
    ncols = G_window_cols();

    H = window.ew_res * 4 * 2;  /* horizontal (east-west) run 
                                   times 4 for weighted difference */
    V = window.ns_res * 4 * 2;  /* vertical (north-south) run 
                                   times 4 for weighted difference */
    /* open the elevation file for reading */
    elevation_fd = G_open_cell_old (elev_name, mapset);
    if (elevation_fd < 0) exit(1);
    elev_cell[0] = G_allocate_cell_buf();
    elev_cell[1] = G_allocate_cell_buf();
    elev_cell[2] = G_allocate_cell_buf();

    slope_fd = opennew (slope_name);
    slope_cell = G_allocate_cell_buf();
    G_put_map_row (slope_fd, slope_cell);

    if (slope_fd < 0)
        exit(1);

    G_get_map_row_nomask (elevation_fd, elev_cell[1], 0);
    G_get_map_row_nomask (elevation_fd, elev_cell[2], 1);

    if (verbose) fprintf (stderr, "percent complete: ");
    for (row = 2; row < nrows; row++)
    {
        if (verbose) G_percent (row, nrows, 2);
        temp = elev_cell[0];
        elev_cell[0] = elev_cell[1];
        elev_cell[1] = elev_cell[2];
        G_get_map_row_nomask (elevation_fd, elev_cell[2] = temp, row);
        
        G_zero_cell_buf(slope_cell);
        
	   for(j=1;j < ncols-1;j++)
	   {
	   	c1 = elev_cell[0][j-1];
	   	c2 = elev_cell[0][j];
	   	c3 = elev_cell[0][j+1];
	   	c4 = elev_cell[1][j-1];
	   	c5 = elev_cell[1][j+1];
	   	c6 = elev_cell[2][j-1];
	   	c7 = elev_cell[2][j];
	   	c8 = elev_cell[2][j+1];
	    
	   	p = ((c1 + c4 + c4 + c6) - (c3 + c5 + c5 + c8)) / H;
	   	q = ((c6 + c7 + c7 + c8) - (c1 + c2 + c2 + c3)) / V;
	   
	   	key = sqrt(p*p + q*q);
		slope_cell[j] = (int) (key*1000);
	   }

        
        G_put_map_row (slope_fd, slope_cell);
    }
    if (verbose) G_percent (row, nrows, 2);

    G_close_cell (elevation_fd);
    if (verbose)
        fprintf (stderr,"Creating support files...\n");

    G_zero_cell_buf (slope_cell);
    G_put_map_row (slope_fd, slope_cell);
    G_close_cell (slope_fd);

    G_read_cats (slope_name, G_mapset(), &cats);
    G_set_cats_title ("ANSWERS format slope (in percent * 10)", &cats);
    G_set_cat ((CELL)0, "", &cats);
    for (i = 1; i <= cats.num; i++)
    {
        cat_label = i;
        sprintf (buf, "%.1f percent", (cat_label/10));
        G_set_cat ((CELL)(i), buf, &cats);
    }
    G_write_cats (slope_name, &cats);
    printf ("slope map <%s> complete\n", slope_name);

    exit(0);
}
