#include "geo.h"
#include "rowio.h"
main(argc,argv) char*argv[];
{
    ROWIO geo_data;
    CELL *out_data, *uptr;

    struct Cell_head cell_hd;
    struct GEO geo;
    int ll_fd;
    int cell_fd;
    char *infile;
    char *outfile;
    int n, nr;
    double north, east;
    double lat, lon;
    int row, col;
    int lrow, lcol;
    int projection;
    int getrow();

    G_gisinit (argv[0]);
    G_get_window (&cell_hd);

    getargs (argc, argv, &geo, &infile, &outfile);

    projection = (geo.lflag) ? PROJECTION_LL : PROJECTION_UTM;
    if (G_projection() != projection) {
	if (geo.lflag)
	    G_fatal_error 
	     ("Current projection must be latitude-logitude when -l flag set");
	else
	    G_fatal_error
	     ("Current projection must be UTM when -l flag NOT set");
	}

    if (G_legal_filename (outfile) < 0)
    {
	fprintf (stderr,"%s - illegal raster file name\n", outfile);
	exit(1);
    }

    if ((ll_fd = open (infile, 0)) < 0)
    {	
	perror (infile);
	exit(1);
    }

/* check input file size */
    nr = geo.nrows * geo.ncols * geo.bpc;
    n = lseek (ll_fd, 0L, 2);
    if (n < nr)
    {
	fprintf (stderr, "%s - file size to small for input specifications\n",
		infile);
	exit(1);
    }
    if (n > nr)
    {
	fprintf (stderr,"warning: %s - file bigger than input specifications\n",
		infile);
    }

    if (geo.lflag)
	nr = 1;
    else {	/* prepare for reprojection to UTM coordinates */
/* set the conversion parameters */
	CC_u2ll_spheroid_parameters (geo.a, geo.e);
	CC_u2ll_zone (cell_hd.zone);

/* find out how many rows of input per row of output */
	CC_u2ll_north (cell_hd.north);
	CC_u2ll (cell_hd.east, &lat, &lon);
	row_col (&geo, lat, lon, &lrow, &lcol);
	nr = lrow;
	CC_u2ll (cell_hd.west, &lat, &lon);
	row_col (&geo, lat, lon, &lrow, &lcol);
	nr -= lrow;
	if (nr < 0) nr = -nr;

	CC_u2ll_north (cell_hd.south);
	CC_u2ll (cell_hd.east, &lat, &lon);
	row_col (&geo, lat, lon, &lrow, &lcol);
	n = lrow;
	CC_u2ll (cell_hd.west, &lat, &lon);
	row_col (&geo, lat, lon, &lrow, &lcol);
	n -= lrow;
	if (n < 0) n = -n;
	if (n > nr) nr = n;
	nr += 4;	/* a bit extra */
	}

/* set up the input paging */
    rowio_setup (&geo_data, ll_fd, nr, geo.ncols * geo.bpc, getrow, NULL);

    cell_fd = G_open_cell_new (outfile);
    if (cell_fd < 0)
    {
	fprintf (stderr, "can't open new raster file %s\n", outfile);
	exit(1);
    }

    if (geo.lflag)
	build_LL_raster (&cell_hd, &geo, &geo_data, cell_fd);
    else
	build_UTM_raster (&cell_hd, &geo, &geo_data, cell_fd);

    G_close_cell (cell_fd);
    exit(0);
}

getrow (fd, buf, row, len)
    char *buf;
{
    lseek (fd, (long) row*len, 0);
    if(read (fd, buf, len) == len)
	return 1;
    fprintf (stderr, "WARNING: error reading row %d of input file\n", row);
    G_zero (buf, len);
    return 1;
}


build_LL_raster (ll, geo, geo_data, cell_fd) 
struct Cell_head *ll;
struct GEO *geo;
ROWIO *geo_data;
int cell_fd;
/*
original code for r.in.ll assumes Eastern Hemisphere to have negative longitude;
thus the kludge for converting geo->lon and geo->lon_res
*/
{
    CELL *ll_data, *ll_ptr;
    int row, col;
    double north, east;
    
    ll_data = G_allocate_cell_buf();

    /* reverses the scan_lon assignment where east is negative */
    geo->lon = -geo->lon;
    geo->lon_res = -geo->lon_res;

    north = ll->north - ll->ns_res/2 ;
    for (row = 0; row < ll->rows; row++)
    {
	east = ll->west + ll->ew_res/2 ;
	ll_ptr = ll_data;
	for (col = 0; col < ll->cols; col++)
	{
	    *ll_ptr++ = geo_value (north * 3600., east * 3600., geo, geo_data);
	    east += ll->ew_res;
	}
	G_put_map_row (cell_fd, ll_data);
	north -= ll->ns_res ;
    }
}


build_UTM_raster (utm, geo, geo_data, cell_fd) 
struct Cell_head *utm;
struct GEO *geo;
ROWIO *geo_data;
int cell_fd;
{
    CELL *utm_data, *uptr;
    int row, col;
    double north, east;
    double lat, lon;
    
    utm_data = G_allocate_cell_buf();

    north = utm->north - utm->ns_res/2 ;
    for (row = 0; row < utm->rows; row++)
    {
	CC_u2ll_north (north);

	east = utm->west + utm->ew_res/2 ;
	uptr = utm_data;
	for (col = 0; col < utm->cols; col++)
	{
	    CC_u2ll (east, &lat, &lon);
	    *uptr++ = geo_value (lat, lon, geo, geo_data);
	    east += utm->ew_res;
	}
	G_put_map_row (cell_fd, utm_data);
	north -= utm->ns_res ;
    }
}
