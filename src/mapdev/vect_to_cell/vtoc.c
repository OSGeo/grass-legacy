 /*
 *
 *  Input: 
 *  digit_file - This is a BINARY encoded version of GRASS's internal vector
 *             format (digit).  (GRASS utilities exist to convert back and
 *             forth between binary and ascii versions of GRASS digit file.
 *  cell_header - This is a support file for the cell file that will be
 *             created.  It also provides DIGIT_TO_BMIF with the geographic
 *             location and number of rows and columns in the grid-cell file.
 *
 *  Modified by Dave Gerdes and Mike Higgins from vect_to_cell 2.0
 *  Modified 8/89 by Dave Gerdes to better support sites
 */
#include "digit.h"
#include "gis.h"
#include "vtoc.h"

extern int cur_category;
char * G_malloc ();
char * G_calloc ();
double dig_x_intersect();
double fabs();
double _which_point_in_area();
double calc_area_area ();
int format;

static struct line_pnts Points;

#define MAX_DIST  99999999.0


struct area_size {
    double size;
    int area;
};

    static unsigned char     *Carray;		
    static short *Sarray;		
    static CELL      *Iarray;		
    static int num_cols, num_rows;

    static int tmp_file;	/* if true (default) will use a temp file */
				/*  if false, will use a RAM buffer       */
				/*  do this by setting GTUNE_VTOC_RAM     */
    static FILE *tmpfp;
    static long file_size;

main(argc, argv)
	int argc ;
	char *argv[] ;
{
	register int col, row, newmap, file_row;
	int pkgs ;
	int cell_type ;
	int packages ;
	char *data;
	char *memptr;
	char *file_name;	/* for temp file */

	struct Map_info Map ;
	struct Cell_head header ;
	CELL *record;
	int row_off;
	struct Categories cats;
	int stat;
	char *mapset;
	int area;
	long timer;
	long f_size;
	int bufsize;

	/* needed to turn off buffering to make clock stuff work right */
	setbuf (stdout, NULL);
/* Check for correct arguments */
	if (argc != 2)
	{
		fprintf(stderr, "USAGE: %s map_name\n", argv[0]) ;
		exit(-1) ;
	}

    /* check to see if we want to run using RAM buffer instead of tmp file */
	if (getenv ("GTUNE_VTOC_RAM") != NULL)
	    tmp_file = 0;
	else
	    tmp_file = 1;

	/* this will be over all time */
	start_clock (&timer);

	/* initialize Points structure */
	Points.alloc_points = 0;

/* Initialize gis library */
	G_gisinit("VECT_TO_CELL") ;

    
	if ((mapset = G_find_file2 ("dig", argv[1], "")) == NULL)
	    G_fatal_error ("Could not find DIG file %s\n", argv[1]);

/* Do initial read of DIGIT file */
	printf ("\nLoading vector information.\n");
	if (dig_P_init(argv[1], mapset, &Map ) == -1 )
		G_fatal_error("Reading digit file.") ;

	format = getformat (&Map);

/* Create header file */
	/*
	cell_type = format - 1 ;
	G_get_window(&header) ;
	header.format = cell_type ;
	G_put_cellhd(argv[1], &header) ;
	*/

	G_get_window(&header) ;
	prime_convert (&header);

/*  Show user stats on cell file size  */
	printf( "\n CELL WINDOW\n") ;
	printf( "   north: %10.2lf,   south: %10.2lf\n",
		header.north, header.south) ;
	printf( "   east: %10.2lf,   west: %10.2lf\n",
		header.east, header.west) ;
	printf( "\n CELL SIZE\n") ;
	printf( "   rows: %d,   cols: %d,   total: %10.2lf\n\n",
		header.rows, header.cols,
		(double)(header.rows * header.cols)) ;

	packages = 0 ;

/* Process areas */

	num_rows = header.rows;
	num_cols = header.cols;

	if (!tmp_file)
	{
	    /* using RAM buffer */
	    switch (format) {
		case USE_CHAR:
		    Carray = (unsigned char *)G_calloc (header.rows * header.cols , sizeof (unsigned char));
/*DEBUG*/ fprintf (stderr, "Size of tmp array %d\n", header.rows*header.cols*sizeof (unsigned char));
		    break;
		case USE_SHORT:
		    Sarray = (short *)G_calloc (header.rows * header.cols , sizeof (short));
/*DEBUG*/ fprintf (stderr, "Size of tmp array %d\n", header.rows*header.cols*sizeof (short));
		    break;
		case USE_CELL:
		default:
		    Iarray = (CELL *)G_calloc (header.rows * header.cols , sizeof (CELL));
/*DEBUG*/ fprintf (stderr, "Size of tmp array %d\n", header.rows*header.cols*sizeof (CELL));
		    break;
	    }
	}
	else
	{
	    /* using tmp file */
	    register char *p;

	    file_name = G_tempfile();
	    if ((tmpfp = fopen (file_name, "w+")) == NULL)
		G_fatal_error ("Can't open tmp file\n");

	    /* initialize file to all 0s */

	    switch (format) {
		case USE_CHAR:
		    file_size = num_rows * num_cols * sizeof (unsigned char);
		    break;
		case USE_SHORT:
		    file_size = num_rows * num_cols * sizeof (short);
		    break;
		case USE_CELL:
		default:
		    file_size = num_rows * num_cols * sizeof (CELL);
		    break;
	    }
/*DEBUG*/ fprintf (stderr, "Size of tmpfile %ld\n", file_size);
	    f_size = file_size;
	    p = G_calloc (10240, 1);

	    while (f_size > 0)
	    {
		if (f_size > 10240)
		    bufsize = 10240;
		else
		    bufsize = f_size;
		fwrite (p, bufsize, 1, tmpfp);
		f_size -= bufsize;
	    }
	    free (p);
	}


	pkgs = do_areas(&Map, &header) ;
	if (pkgs < 0)
		G_fatal_error("Working on digit areas.") ;
	packages += pkgs ;

	start_clock (NULL);
	pkgs = do_lines(&Map, &header) ;
	if (pkgs < 0)
		G_fatal_error("Working on digit lines.") ;
	packages += pkgs ;
	stop_clock (NULL);

	printf( "Step 4: Creating Site information...   ") ;
	start_clock (NULL);
	pkgs = do_sites(&Map, &header) ;
	if (pkgs < 0)
		G_fatal_error("Working on digit sites.") ;
	packages += pkgs ;
	stop_clock (NULL);

	fclose(Map.digit) ;

	printf( "Step 5: Creating cell file...          ") ;
	start_clock (NULL);

	record = G_allocate_cell_buf() ;
	switch (format) {
	    case USE_CHAR:
		data = G_calloc (header.cols , sizeof (unsigned char));
		break;
	    case USE_SHORT:
		data = G_calloc (header.cols , sizeof (short));
		break;
	    case USE_CELL:
	    default:
		data = G_calloc (header.cols , sizeof (CELL));
		break;
	}

	if ( (newmap = G_open_cell_new (argv[1]) ) == -1)
	{
	    fprintf(stderr, "ERROR vect_to_cell: can't open cell file %s\n", argv[1]) ;
	    exit(-1) ;
	}

	if (tmp_file)	/* get back to beginning */
	    fseek (tmpfp, 0l, 0);

	/* Loop for all data rows */
	for (file_row = num_rows-1, row = 0 ; row < num_rows ; row++,file_row--)
	{
	    if (tmp_file)
	    {
		switch (format) {
		    case USE_CHAR:
			fseek (tmpfp, (long)file_row * num_cols * sizeof (unsigned char), 0);
			fread (data, num_cols, sizeof (unsigned char), tmpfp);
			break;
		    case USE_SHORT:
			fseek (tmpfp, (long)file_row * num_cols * sizeof (short), 0);
			fread (data, num_cols, sizeof (short), tmpfp);
			break;
		    case USE_CELL:
		    default:
			fseek (tmpfp, (long)file_row * num_cols * sizeof (CELL), 0);
			fread (data, num_cols, sizeof (CELL), tmpfp);
			break;
		}
		expand (record, data, num_cols, format);
	    }
	    else
	    {
		row_off = file_row * num_cols;

		switch (format) {
		    case USE_CHAR:
		       expand(record, (char *)&(Carray[row_off]),num_cols,format);
			break;
		    case USE_SHORT:
		       expand(record, (char *)&(Sarray[row_off]),num_cols,format);
			break;
		    case USE_CELL:
		    default:
		       expand(record, (char *)&(Iarray[row_off]),num_cols,format);
			break;
		}
	    }
	    G_put_map_row (newmap, record);
	}

	G_close_cell (newmap);
	if (tmp_file)
	{
	    fclose (tmpfp);
	    unlink (file_name);
	}
	stop_clock (NULL);

finish:
	dig_P_fini (&Map);

	if ( 0  >  update_hist(argv[1]) )
	{
		fprintf( stderr, "ERROR: Couldn't update history file : %s.\n", argv[1]) ;
	}

	G_suppress_warnings (1);
	stat = G_read_vector_cats (argv[1], mapset, &cats);
	G_suppress_warnings (0);
	if (stat >= 0)	/* if cats file existed */
	{
	    /*
	    printf ( "Copying vector category file\n");
	    */
	    stat = G_write_cats (argv[1], &cats);
	}
	printf ( "                  Total time:          ");
	stop_clock (&timer);
	printf ( "\n");
	exit(0) ;
}

/* file a row from x1 to x2 */
row_fill (row, x1, x2)
    int row;
    double x1, x2;
{
    register int i, start, stop, row_off;

    /* want to make sure we only get those cells whose centers are inside x1/x2
    */
    start = iceiling (x1 - .5);
    stop = ifloor (x2 - .5);

    /* clip it to the window */
    if (row < 0 || row >= num_rows)
	return 0;
    if (stop < 0 || start >= num_cols)
	return 0;
    if (stop >= num_cols)
	stop = num_cols-1;
    if (start < 0)
	start = 0;

    /* and fill the row */
    row_off = row * num_cols;
    if (!tmp_file)
    {
	switch (format) {
	    case USE_CHAR:
		for (i = start ; i <= stop ; i++)
		    Carray[row_off + i] = (unsigned char) cur_category;
		break;
	    case USE_SHORT:
		for (i = start ; i <= stop ; i++)
		    Sarray[row_off + i] = (short) cur_category;
		break;
	    case USE_CELL:
	    default:
		for (i = start ; i <= stop ; i++)
		    Iarray[row_off + i] = (CELL) cur_category;
		break;
	}
    }
    else
    {
	long offset;
	static int first_time;
	static CELL *Ibuf;
	static unsigned char *Cbuf;
	static short *Sbuf;

	if (first_time == 0)
	{

	    switch (format) {
		case USE_CHAR:
		    Cbuf = (unsigned char *) G_calloc (num_cols + 1, sizeof (unsigned char));
		    break;
		case USE_SHORT:
		    Sbuf = (short *) G_calloc (num_cols + 1, sizeof (short));
		    break;
		case USE_CELL:
		default:
		    Ibuf = (CELL *) G_calloc (num_cols + 1, sizeof (CELL));
		    break;
	    }
	    first_time = -1;
	}
	switch (format) {
	    case USE_CHAR:
		offset = (row_off + start) * sizeof (unsigned char);
		fseek (tmpfp, offset, 0);
		for (i = start ; i <= stop ; i++)
		    Cbuf[i] =  cur_category;
		fwrite ((char *)(Cbuf+start), stop-start+1, sizeof (unsigned char), tmpfp);
		break;
	    case USE_SHORT:
		offset = (row_off + start) * sizeof (short);
		fseek (tmpfp, offset, 0);
		for (i = start ; i <= stop ; i++)
		    Sbuf[i] =  cur_category;
		fwrite ((char *)(Sbuf+start), stop-start+1, sizeof (short), tmpfp);
		break;
	    case USE_CELL:
	    default:
		offset = (row_off + start) * sizeof (CELL);
		fseek (tmpfp, offset, 0);
		for (i = start ; i <= stop ; i++)
		    Ibuf[i] =  cur_category;
		fwrite ((char *)(Ibuf+start), stop-start+1, sizeof (CELL), tmpfp);
		break;
	}
    }

    return (0);
}

/* place 1 row/col value into array */
row_put (row, col)
    int row;
    int col;
{
    register int ndex;

    if (row < 0 || row >= num_rows)
	return 0;
    if (col < 0 || col >= num_cols)
	return 0;

    if (!tmp_file)
    {
	switch (format) {
	    case USE_CHAR:
		Carray[row * num_cols + col] = (unsigned char) cur_category;
		break;
	    case USE_SHORT:
		Sarray[row * num_cols + col] = (short) cur_category;
		break;
	    case USE_CELL:
	    default:
		Iarray[row * num_cols + col] = (CELL) cur_category;
		break;
	}
    }
    else
    {
	long offset;
	unsigned char 	  Cbuf;
	short  Sbuf;
	CELL 	  Ibuf;


	switch (format) {
	    case USE_CHAR:
		Cbuf = cur_category;
		offset = (row * num_cols + col) * sizeof (unsigned char);
		fseek (tmpfp, offset, 0);
		fwrite ((char *)&Cbuf, 1, sizeof (unsigned char), tmpfp);
		break;
	    case USE_SHORT:
		Sbuf = cur_category;
		offset = (row * num_cols + col) * sizeof (short);
		fseek (tmpfp, offset, 0);
		fwrite ((char *)&Sbuf, 1, sizeof (short), tmpfp);
		break;
	    case USE_CELL:
	    default:
		Ibuf = cur_category;
		offset = (row * num_cols + col) * sizeof (CELL);
		fseek (tmpfp, offset, 0);
		fwrite ((char *)&Ibuf, 1, sizeof (CELL), tmpfp);
		break;
	}
    }
    return (0);
}

quit()
{
	fprintf(stderr,"READ ERROR in bmif_to_cell\n") ;
	exit(-1) ;
}

/* we want to sort areas from greatest to least size */
sort_areas (a, b)
    struct area_size *a, *b;
{
    if (a->size < b->size)
	return (1);
    if (a->size > b->size)
	return (-1);
    return (0);
}

do_areas (Map, header)
    struct Map_info *Map;
    struct Cell_head *header ;
{
    register int i, row, col, area, a_index;
    int att;
    int num_areas;
    double x, y;
    struct area_size *area_list;
    double rtmp;
    int del_areas;
    int ndex;
    int cat;

/* clean out unlabeled areas */
/* note, this does not touch the files, just our copy of the data */
    printf ( "Step 1: Calculating Areas...           ");
    start_clock (NULL);
    del_areas = 0;
    for (i = 1 ; i <= Map->n_areas ; i++)
    {

	/*  we are going to process all areas making those w/out labels no data
	if (Map->Area[i].att == 0)
	{
	    dig__del_area (Map, i);
	    del_areas++;
	}
	else
	*/
	{
	    P_AREA *Area;
	    int grid_W, grid_E, grid_N, grid_S;

	    Area = &(Map->Area[i]);

	    /* get grid bounding box */
	    map_to_cell (Area->S, Area->W, &grid_S, &grid_W);
	    map_to_cell (Area->N, Area->E, &grid_N, &grid_E);
	    /* expand the box by 1, cuz of truncation in map_to_cell */
	    grid_W--;
	    grid_S--;
	    grid_N++;
	    grid_E++;
	    /*
	    grid_W -= grid_W == 0 ? 0 : 1;
	    grid_S -= grid_S == 0 ? 0 : 1;
	    grid_N += grid_N == num_rows ? 0 : 1;
	    grid_E += grid_E == num_cols ? 0 : 1;
	    */

	    /* check bbox to see if it is totally outside of window */
	    /* if so, we can skip it. */
	    if (grid_E<0 || grid_N<0 || grid_W > num_cols || grid_S > num_rows)
	    {
		dig__del_area (Map, i);
		del_areas++;
	    }
	} /* else */
    } /* for */

     
    num_areas = Map->n_areas - del_areas;
    area_list = (struct area_size *)
	    G_malloc (num_areas * sizeof (struct area_size));
    for (ndex = 0, area = 1 ; area <= Map->n_areas ; area++)
    {
	if (!AREA_ALIVE (&(Map->Area[area])))
	    continue;
	/* the following didnt work on MASSCOMP 1.1 compiler */
	/* it does work on 1.3 */
	/*
	area_list[area-1].size = calc_area_area (Map, area);
	*/
	rtmp = calc_area_area (Map, area);
	area_list[ndex].size = rtmp;
	area_list[ndex].area = area;
	ndex++;
    }

    qsort(area_list, num_areas, sizeof(struct area_size), sort_areas) ;
    stop_clock (NULL);


    printf ( "Step 2: Creating Area information...   ");
    start_clock (NULL);
    for (a_index = 0 ; a_index < num_areas ; a_index++)
    {
	area = area_list[a_index].area;
	att = Map->Area[area].att;
	cur_category = att ? Map->Att[att].cat : 0;
	process_area (Map, num_rows, num_cols, area);
    }
    stop_clock (NULL);
    return (num_areas);
}
static double west;
static double east;
static double north;
static double south;
static double ew_res;
static double ns_res;
static double dnrows;

prime_convert(header)
	struct Cell_head *header ;
{
	south    = header->south ;
	west     = header->west ;
	ew_res   = header->ew_res ;
	ns_res   = header->ns_res ;
	dnrows   = (double)header->rows ;

	if (ew_res == 0.0 || ns_res == 0.0 || dnrows == 0.0)
	{
	    fprintf (stderr, "Window not properly defined\n");
	    exit (-1);
	}
}


/* translate from map to row/col */
map_to_cell (row, col, uy, ux)
    double row, col;
    int *uy, *ux;
{
	*ux = (int) ((col - west ) / ew_res);
	*uy = (int) ((row - south) / ns_res);
}

/* translate from row/col to map */
cell_to_map (row, col, uy, ux)
    int row, col;
    double *uy, *ux;
{

	*ux = (col + .5) * ew_res + west;
	*uy = (row + .5) * ns_res + south;
}

/* translate from map to row/col */
translate (x, y, n)
    double *x, *y;
    int n;
{
    double round;

    for ( ; n-- > 0 ; y++, x++)
    {
	*x =  (*x - west ) / ew_res;
	*y =  (*y - south) / ns_res;

	if (*y < 0)
	    round = -.5;
	else
	    round = .5;

	if ((int) (*y - round) == (int) *y)
	    *y *= 1.000000000001;
	    /* *y += .00001; */
    }
}


/* this is here to shut up the unix ld.  am calling dig__del_area() which
** calls dig__del_att ()  which is in the same file as dig_del_att () 
** which calls write_att () which is in a diffent library
*/
write_att ()
{
}
static double *xarr, *yarr;
static int n_vert;

double
calc_area_area (map, area)
    struct Map_info *map;
    int area;
{
    double totalarea;
    /*
    double cent_x, cent_y, south;
    */
    
    /* temporary.  need to fill this in w/ south border of map */
    /* 3.1
    south = 0.0;
    dig_find_area(map, &(map->Area[area]), &totalarea, &cent_x, &cent_y, south);
    */
    dig_find_area2 (map, &(map->Area[area]), &totalarea);

    return (totalarea);
}

expand (record, data, num_cols, format)
    CELL *record;
    char *data;
    int num_cols;
    int format;
{
    register int i;
    unsigned char     *Cdata;
    short *Sdata;
    CELL     *Idata;

    switch (format) {
	case USE_CHAR:
	    Cdata = (unsigned char *) data;
	    for (i = 0 ; i < num_cols ; i ++)
		record[i] = Cdata[i];
	    break;
	case USE_SHORT:
	    Sdata = (short *) data;
	    for (i = 0 ; i < num_cols ; i ++)
		record[i] = Sdata[i];
	    break;
	case USE_CELL:
	default:
	    Idata = (CELL *) data;
	    for (i = 0 ; i < num_cols ; i ++)
		record[i] = Idata[i];
	    break;
    }
}

getformat (map)
    struct Map_info *map;
{
    register i;
    int size;
    CELL largest, smallest, label;
    unsigned char Csmallest, Clargest;
    short Ssmallest, Slargest;
    int first;

    largest = smallest = 0;
    first = 1;
    for (i = 1 ; i <= map->n_atts ; i++)
    {
	if (! ATT_ALIVE (&(map->Att[i])))
	    continue;
	label = map->Att[i].cat;
	if (first)
	{
	    first = 0;
	    largest = label;
	    smallest = label;
	}
	if (label > largest)
	    largest = label;
	if (label < smallest)
	    smallest = label;
    }

    /* test char */

    Clargest =  largest;
    Csmallest =  smallest;
    if (Csmallest == smallest && Clargest == largest)
	return (USE_CHAR);

    Slargest = (short) largest;
    Ssmallest = (short) smallest;
    if (Ssmallest == smallest && Slargest == largest)
	return (USE_SHORT);

    return (USE_CELL);
}
