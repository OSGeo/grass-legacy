/* %W% %G% */
#include "gis.h"
#define abs(x) ((x)<0?-(x):(x))

/**************************************************************
 * input is from command line.
 * arguments are elevation file, slope file and aspect file
 * optional no verbose flag (-v)
 * elevation filename required
 * either slope filename or aspect filename required
 * usage: Gslope.aspect [-v] elevation=input slope=output1 aspect=output2
 **************************************************************/

static float aspect_colors[] =
{
    0.325,
    0.4,
    0.475,
    0.55,
    0.625,
    0.7,
    0.775,
    0.85,
    0.925,
    1.0,
    0.925,
    0.85,
    0.775,
    0.7,
    0.625,
    0.55,
    0.475,
    0.4,
    0.325,
    0.25,
    0.175,
    0.1,
    0.175,
    0.25,
    0.0,
    -1.0	/* marks end of color list */
};

static char *aspect_cats[] =
{
    "no data",
    "east facing",
    "15 degrees north of east",
    "30 degrees north of east",
    "northeast facing",
    "30 degrees east of north",
    "15 degrees east of north",
    "north facing",
    "15 degrees west of north",
    "30 degrees west of north",
    "northwest facing",
    "30 degrees north of west",
    "15 degrees north of west",
    "west facing",
    "15 degrees south of west",
    "30 degrees south of west",
    "southwest facing",
    "30 degrees west of south",
    "15 degrees west of south",
    "south facing",
    "15 degrees east of south",
    "30 degrees east of south",
    "southeast facing",
    "30 degrees south of east",
    "15 degrees south of east",
    "no aspect",
    (char *) NULL
};

main (argc, argv) char *argv[];
{
    struct Colors colr;
    struct Categories cats;
    static int verbose = 1;
    int elevation_fd;
    static int aspect_fd = -1 ;
    static int slope_fd = -1;
    static int elev_flag = 0;
    static int slope_flag = 0;
    static int aspect_flag = 0;
    CELL *elev_cell[3], *temp;
    CELL *c1, *c2, *c3, *c4, *c5, *c6, *c7, *c8;
    CELL *aspect_cell, *ac ;
    CELL *slope_cell, *sc ;
    int i;
    struct Cell_head window;

    char elev_name[300];
    char aspect_name[100];
    char slope_name[100];
    char buf[300];
    char *mapset;
    int nrows, row;
    int ncols, col;


    double radians_to_degrees;
    double degrees_to_radians;
    double sqrt(), tan();
    double H,V;
    double p;              /* slope in ew direction */
    double q;              /* slope in ns direction */
    double quotient;
    double pos_quot;
    double ratio1, ratio2, ratio3, ratio4, ratio5, ratio6;
    int aspect;

    double answer[92];
    double degrees;
    double tan_ans;
    double key;
    int low, hi, test, flag;


    G_gisinit (argv[0]);

    radians_to_degrees = 180.0 / 3.14159 ;
    degrees_to_radians = 3.14159 / 180.0 ;

    answer[0] = 0.0;
    answer[91] = 15000.0;

    for (i = 1; i < 91; i++)
    {
	degrees = i - .5;
	tan_ans = tan ( degrees  / radians_to_degrees );
	answer[i] = tan_ans * tan_ans;
    }

/* aspect is given one of 24 values, based on directions of
   E,15 degrees north of east, 30 degrees north of east, NE,
   30 degrees east of north, 15 degrees east of north, N,
   and so on.  each direction is given 15 degrees of the
   360 degree circle, with intervals centered on the E, NE,
   N, NW, W, SW, S, and SE directions.  aspect value is then
   determined by the interval in which the raio of q to p
   falls.
*/

    ratio1 = tan( 7.5 * degrees_to_radians );
    ratio2 = tan( 22.5 * degrees_to_radians );
    ratio3 = tan( 37.5 * degrees_to_radians );
    ratio4 = tan( 52.5 * degrees_to_radians );
    ratio5 = tan( 67.5 * degrees_to_radians );
    ratio6 = tan( 82.5 * degrees_to_radians );

/* parse command line */
    for (i = 1; i < argc; i++)
    {
        if (strcmp (argv[i], "-v") == 0)
        {
            verbose = 0;
            continue;
        }
        if (sscanf (argv[i], "elevation=%[^\n]", elev_name) == 1)
        {
            if (elev_flag++) usage(argv[0]);
            continue;
        }
        if (sscanf (argv[i], "slope=%s", slope_name) == 1)
        {
            if (slope_flag++) usage(argv[0]);
            continue;
        }
        if (sscanf (argv[i], "aspect=%s", aspect_name) == 1)
        {
            if (aspect_flag++) usage(argv[0]);
            continue;
        }
	usage(argv[0]);
        exit(1);
    }

    if ((!elev_flag) || ((!slope_flag) && (!aspect_flag)))
         usage(argv[0]);

/* check elevation file existence */
    mapset = G_find_file ("cell", elev_name, "");
    if (!mapset)
    {
	sprintf (buf, "elevation file [%s] not found\n", elev_name);
	G_fatal_error (buf);
	exit(1);
    }

/* set the window from the header for the elevation file */
    if (G_get_cellhd (elev_name, mapset, &window) >= 0)
	G_set_window (&window);

    nrows = G_window_rows();
    ncols = G_window_cols();

    H = window.ew_res * 4 * 2;	/* horizontal (east-west) run 
				times 4 for weighted difference */
    V = window.ns_res * 4 * 2;	/* vertical (north-south) run 
				times 4 for weighted difference */

/* open the elevation file for reading */
    elevation_fd = G_open_cell_old (elev_name, mapset);
    if (elevation_fd < 0) exit(1);
    elev_cell[0] = G_allocate_cell_buf();
    elev_cell[1] = G_allocate_cell_buf();
    elev_cell[2] = G_allocate_cell_buf();

    if (slope_flag)
    {
	slope_fd = opennew (slope_name);
	slope_cell = G_allocate_cell_buf();
	G_zero_cell_buf (slope_cell);
	G_put_map_row (slope_fd, slope_cell);
    }
    if (aspect_flag)
    {
	aspect_fd = opennew (aspect_name);
	aspect_cell = G_allocate_cell_buf();
	G_zero_cell_buf (aspect_cell);
	G_put_map_row (aspect_fd, aspect_cell);
    }
    if (aspect_fd < 0 && slope_fd < 0)
	exit(1);


    G_get_map_row_nomask (elevation_fd, elev_cell[1], 0);
    G_get_map_row_nomask (elevation_fd, elev_cell[2], 1);

    if (verbose) fprintf (stderr, "percent complete: ");
    for (row = 2; row < nrows; row++)
    {
	if (verbose) percent (row, nrows, 10);
	temp = elev_cell[0];
	elev_cell[0] = elev_cell[1];
	elev_cell[1] = elev_cell[2];
	G_get_map_row_nomask (elevation_fd, elev_cell[2] = temp, row);

	c1 = elev_cell[0];
	c2 = c1+1;
	c3 = c2+1;
	c4 = elev_cell[1];
	c5 = c4+2;
	c6 = elev_cell[2];
	c7 = c6+1;
	c8 = c7+1;

	ac = aspect_cell + 1;
	sc = slope_cell + 1;

	col = ncols - 2;
	while (col-- > 0)
	{
	    p = ((*c1 + *c4 + *c4 + *c6) - (*c3 + *c5 + *c5 + *c8)) / H;
	    q = ((*c6 + *c7 + *c7 + *c8) - (*c1 + *c2 + *c2 + *c3)) / V;

	    c1++; c2++; c3++; c4++; c5++; c6++; c7++; c8++;

	    key = p*p + q*q;
	    low = 1;
	    hi = 91;
	    test = 20;

	    while (hi >= low)
	    {
		if ( key >= answer[test] )
		    low = test + 1;
		else if ( key < answer[test-1] )
		    hi = test - 1;
		else
		    break;
		test = (low + hi) / 2;
	    }
	    if (slope_fd > 0)
		*sc++ = test;


	    if (aspect_fd > 0)
	    {
		if (key == 0) aspect = 25;	/* no slope, no aspect */
		else if (p == 0)
		{
		    if (q > 0) aspect = 7;	/* north */
		    else aspect = 19;	/* or south */
		}
		else
		{
		    quotient = q / p;
		    pos_quot = abs(quotient);
		    if (pos_quot <= ratio1)
		    {
			if (p > 0) aspect = 1;	/* east */
			else aspect = 13;	/* or west */
		    }
		    else if (pos_quot <= ratio2)
		    {
		    /* check for first or second quadrant */
			if (quotient > 0) aspect = 2;
			else aspect = 12;
		    }
		    else if (pos_quot <= ratio3)
		    {
			if (quotient > 0) aspect = 3;
			else aspect = 11;
		    }
		    else if (pos_quot <= ratio4)
		    {
			if (quotient > 0) aspect = 4;
			else aspect = 10;
		    }
		    else if (pos_quot <= ratio5)
		    {
			if (quotient > 0) aspect = 5;
			else aspect = 9;
		    }
		    else if (pos_quot <= ratio6)
		    {
			if (quotient > 0) aspect = 6;
			else aspect = 8;
		    }
		    else
			aspect = 7;
	    /* check for third or fourth quadrant */
		    if (( pos_quot > ratio1 ) && ( q < 0 )) aspect += 12;
		}
		*ac++ = aspect;
	    }
	}
	if (aspect_fd > 0)
	    G_put_map_row (aspect_fd, aspect_cell);
	if (slope_fd > 0)
	    G_put_map_row (slope_fd, slope_cell);
    }
    if (verbose) percent (row, nrows, 10);

    G_close_cell (elevation_fd);
    if (verbose)
	fprintf (stderr,"CREATING SUPPORT FILES\n");

    printf ("ELEVATION PRODUCTS for mapset [%s] in [%s]\n",
		G_mapset(), G_location());

    if (aspect_fd >= 0)
    {
	G_zero_cell_buf (aspect_cell);
	G_put_map_row (aspect_fd, aspect_cell);

	G_close_cell (aspect_fd);
	G_init_colors (&colr);
	G_set_color (0, 255, 0, 0, &colr);

	for (i = 0; aspect_colors[i] >= 0.0; i++)
	{
	    int grey;
	    grey = aspect_colors[i] * 256;
	    G_set_color (i+1, grey, grey, grey, &colr);
	}

	G_write_colors (aspect_name, G_mapset(), &colr);
	G_free_colors (&colr);

	G_read_cats (aspect_name, G_mapset(), &cats);
	G_set_cats_title ("aspect", &cats);

	for (i = 0; aspect_cats[i] != NULL; i++)
	    G_set_cat (i, aspect_cats[i], &cats);

	G_write_cats (aspect_name, &cats);
	G_free_cats (&cats);

	printf ("ASPECT [%s] COMPLETE\n", aspect_name);
    }

    if (slope_fd >= 0)
    {
	G_zero_cell_buf (slope_cell);
	G_put_map_row (slope_fd, slope_cell);
	G_close_cell (slope_fd);

	G_read_cats (slope_name, G_mapset(), &cats);
	G_set_cats_title ("slope (in degrees)", &cats);
	G_set_cat ((CELL)0, "no data", &cats);
	for (i = 0; i < cats.num; i++)
	{
	    sprintf (buf, "%d degree%s", i, i==1?"":"s");
	    G_set_cat ((CELL)(i+1), buf, &cats);
	}
	G_write_cats (slope_name, &cats);
	printf ("SLOPE [%s] COMPLETE\n", slope_name);
    }

    exit(0);
}
