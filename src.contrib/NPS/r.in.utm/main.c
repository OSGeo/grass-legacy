/************************************************************
* name
*    r.out.ll
*
* function
*    convert from utm raster files to latitude/longitude raster files
*
* usage
*    r.out.ll spheroid=name [zone=value] [input=file] [output=file]
*               rows=# columns=# bpc=# res_ns=# res_ew=# north=# east=#
*
* r.out.ll spheroid=clark66 zone=7 input=cell/boundaries output=xyz
*          rows=4833 columns=5016 bpc=1 res_ns=30.0 res_ew=30.0
*          north=7290000.0 easting=500500.0
*
*
* Note:
*   This program attempt to preserve as much of the input
*   in its orginal form as possible, replacing east north with lon lat.
****************************************************************/

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include "gis.h"
#include "rowio.h"
#include "CC.h"
#include "utm.h"

int 
main (int argc, char *argv[])
{
    ROWIO utm_data;		/* Input file will be read using ROWIO. */
    CELL *ll_data, *lptr;   	/* Pointers to output */
    struct UTM utm;		/* UTM struct defined in utm.h */

    struct Cell_head ll_head; 	/* Cell header struct. */
    int ll_fd, utm_fd;		/* File descriptors. */
    int n, nr;
    double north, east;		/* Holds the UTM values. */
    double lat, lon;		/* Holds the LL values. */
    int row, col;		/* Holds row/col value. */
    int lrow, lcol;		/* Values passed to functions. */
    double a,e;
    int zone, len = 1024;
    char buf[1024], buf1[1024];
    struct
    {
	struct Option *spheroid, *zone, *input, *output, *rows, *cols, 
                      *bpc, *res_ns, *res_ew, *northing, *easting;
    } parm;
    struct
    {
	struct Flag *southern, *o, *w, *r;
    } flag;
    int getrow();
    char *spheroid_list();
    G_zero (buf, len);

    G_gisinit(argv[0]);

    parm.spheroid = G_define_option();
    parm.spheroid->key = "spheroid";
    parm.spheroid->description = "reference spheroid (ellipsoid)";
    parm.spheroid->type = TYPE_STRING;
    parm.spheroid->required = YES;
    parm.spheroid->options = spheroid_list();

    parm.zone = G_define_option();
    parm.zone->key = "zone";
    parm.zone->description = "utm zone";
    parm.zone->type = TYPE_INTEGER;
    parm.zone->required = YES;
    parm.zone->options = "1-60";
	/* note: on output southern hemisphere zones are negative */

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->description = "input file";
    parm.input->type = TYPE_STRING;
    parm.input->required = YES;

    parm.output = G_define_option();
    parm.output->key = "output";
    parm.output->description = "output file";
    parm.output->type = TYPE_STRING;
    parm.output->required = YES;

    parm.rows = G_define_option();
    parm.rows->key = "rows";
    parm.rows->description = "# of rows in utm file";
    parm.rows->type = TYPE_INTEGER;
    parm.rows->required = YES;

    parm.cols = G_define_option();
    parm.cols->key = "columns";
    parm.cols->description = "# of columns in utm file";
    parm.cols->type = TYPE_INTEGER;
    parm.cols->required = YES;

    parm.bpc = G_define_option();
    parm.bpc->key = "bpc";
    parm.bpc->description = "Bytes per column";
    parm.bpc->type = TYPE_INTEGER;
    parm.bpc->required = YES;

    parm.res_ns = G_define_option();
    parm.res_ns->key = "res_ns";
    parm.res_ns->description = "north/south resolution of utm file";
    parm.res_ns->type = TYPE_DOUBLE;
    parm.res_ns->required = YES;

    parm.res_ew = G_define_option();
    parm.res_ew->key = "res_ew";
    parm.res_ew->description = "east/west resolution of utm file";
    parm.res_ew->type = TYPE_DOUBLE;
    parm.res_ew->required = YES;

    parm.northing = G_define_option();
    parm.northing->key = "north";
    parm.northing->description = "northing of utm file";
    parm.northing->type = TYPE_DOUBLE;
    parm.northing->required = YES;

    parm.easting = G_define_option();
    parm.easting->key = "east";
    parm.easting->description = "easting of utm file";
    parm.easting->type = TYPE_DOUBLE;
    parm.easting->required = YES;

    flag.southern = G_define_flag();
    flag.southern->key = 's';
    flag.southern->description = "Specified zone is in the southern hemisphere";

    flag.w = G_define_flag();
    flag.w->key = 'w';
    flag.w->description = "Do not flag invalid east,north input lines as errors";

    flag.o = G_define_flag();
    flag.o->key = 'o';
    flag.o->description = "Flag other input lines as errors";

    if (G_parser(argc,argv))
	exit(1);
    
    G_get_window (&ll_head);

    sscanf (parm.northing->answer, "%lf", &utm.north);
    sscanf (parm.easting->answer, "%lf", &utm.east);
    sscanf (parm.res_ns->answer, "%lf", &utm.ns_res);
    sscanf (parm.res_ew->answer, "%lf", &utm.ew_res);
    utm.ncols = atoi(parm.cols->answer);
    utm.nrows = atoi(parm.rows->answer);
    utm.south = utm.north - utm.ns_res * utm.nrows;
    utm.west = utm.east - utm.ew_res * utm.ncols;
    utm.bpc = atoi(parm.bpc->answer);
    zone = 0;
    if (parm.zone->answer)
	sscanf (parm.zone->answer, "%d", &zone);

    if(!G_get_ellipsoid_by_name(parm.spheroid->answer, &a, &e))
    {
	fprintf (stderr, "%s=%s - unknown spheroid\n",
		parm.spheroid->key, parm.spheroid->answer);
	G_usage();
	exit(1);
    }
    CC_u2ll_spheroid_parameters(a,e);

    if (parm.output->answer)
    {
        if (G_legal_filename (parm.output->answer) < 0)
        {
	    fprintf (stderr, "%s: %s=", G_program_name(), parm.output->key);
	    perror (parm.output->answer);
	    exit(1);
	}
    }
    if ((utm_fd = open (parm.input->answer, 0)) < 0)
    {	
	fprintf (stderr, "%s: %s=", G_program_name(), parm.input->key);
	perror (parm.input->answer);
	exit(1);
    }

/* check input file size */
/*
    nr = atoi(parm.rows->answer) * atoi(parm.cols->answer) * atoi(parm.bpc->answer);
*/
    nr = atoi(parm.rows->answer) * atoi(parm.cols->answer);
    n = lseek (utm_fd, 0L, 2);
    if (n < nr)
    {
fprintf(stderr, "ROWS * COLS = %d,  LSEEK = %d\n", nr, n);
	fprintf (stderr, "%s - file size to small for input specifications\n",
		parm.input->answer);
	exit(1);
    }
    if (n > nr)
    {
	fprintf (stderr,"warning: %s - file bigger than input specifications\n",
		parm.input->answer);
    }

/* find out how many rows of input per row of output */
    lat = ll_head.north;
    lon = ll_head.east;
    lat *= 3600.0; /* convert to arc seconds for CC library */
    lon *= 3600.0; /* convert to arc seconds for CC library */
    lon = -lon;    /* CC lib expects negative in the west, which is
			      reverse from what G_scan_easting() returns */
    CC_ll2u (lat, lon, &east, &north, &zone);
    row_col (&utm, north, east, &lrow, &lcol);
/*
    row_col (parm.northing, parm.easting, atoi(parm.res_ns->answer), atoi(parm.res_ew->answer), north, east, &lrow, &lcol);
*/
    nr = lrow;
    lat = ll_head.north;
    lon = ll_head.west;
    lat *= 3600.0; /* convert to arc seconds for CC library */
    lon *= 3600.0; /* convert to arc seconds for CC library */
    lon = -lon;    /* CC lib expects negative in the west, which is
			      reverse from what G_scan_easting() returns */
    CC_ll2u (lat, lon, &east, &north, &zone);
    row_col (&utm, north, east, &lrow, &lcol);
    nr -= lrow;
    if (nr < 0) nr = -nr;

    lat = ll_head.south;
    lon = ll_head.east;
    lat *= 3600.0; /* convert to arc seconds for CC library */
    lon *= 3600.0; /* convert to arc seconds for CC library */
    lon = -lon;    /* CC lib expects negative in the west, which is
			      reverse from what G_scan_easting() returns */
    CC_ll2u (lat, lon, &east, &north, &zone);
    row_col (&utm, north, east, &lrow, &lcol);
    n = lrow;
    lat = ll_head.south;
    lon = ll_head.west;
    lat *= 3600.0; /* convert to arc seconds for CC library */
    lon *= 3600.0; /* convert to arc seconds for CC library */
    lon = -lon;    /* CC lib expects negative in the west, which is
			      reverse from what G_scan_easting() returns */
    CC_ll2u (lat, lon, &east, &north, &zone);
    row_col (&utm, north, east, &lrow, &lcol);
    n -= lrow;
    if (n < 0) n = -n;
    if (n > nr) nr = n;
    nr += 4;	/* a bit extra */

/* set up the input paging
fprintf(stderr, "NRows = %d, Cols = %s,  BPC = %s\n", nr, parm.cols->answer, parm.bpc->answer);
exit(0); */
    rowio_setup (&utm_data, utm_fd, nr, atoi(parm.cols->answer) * atoi(parm.bpc->answer), getrow, NULL);
    fprintf(stderr, "LEN = %d\n", atoi(parm.cols->answer) * atoi(parm.bpc->answer));

    ll_fd = G_open_cell_new (parm.output->answer);
    if (ll_fd < 0)
    {
	fprintf (stderr, "can't open new cell file %s\n", parm.output->answer);
	exit(1);
    }

    ll_data = G_allocate_cell_buf();

    lat = ll_head.north - ll_head.ns_res/2;
    lat *= 3600.0; /* convert to arc seconds for CC library */
    G_adjust_Cell_head(&ll_head, ll_head.rows, ll_head.cols);
    fprintf(stderr, "Rows = %d,  Cols = %d\n", ll_head.rows, ll_head.cols);
    for (row = 0; row < ll_head.rows; row++)
    {
        lon = ll_head.west + ll_head.ew_res/2;
        lon *= 3600.0; /* convert to arc seconds for CC library */

        lon = -lon;    /* CC lib expects negative in the west, which is
			      reverse from what G_scan_easting() returns */
	lptr = ll_data;
	for (col = 0; col < ll_head.cols; col++)
	{
            CC_ll2u (lat, lon, &east, &north, &zone);
/*
G_format_northing(lat/3600, buf1, PROJECTION_LL);
G_format_easting(lon/3600, buf, PROJECTION_LL);
fprintf(stderr, "%s %s\n", buf, buf1);
fprintf(stderr, "East = %f   Lon = %f  Z = %d\n", east, lon, zone);
*/
	    *lptr++ = utm_value (north, east, &utm, &utm_data);
	    lon += ll_head.ew_res*3600.0;
        }
	G_put_map_row (ll_fd, ll_data);
        lat -= ll_head.ns_res*3600.0;
    }
   G_close_cell(ll_fd);

   return 0;
}

int getrow (int fd, char *buf, int row, int len)
{
/*
fprintf(stderr, "Row = %d, Len = %d\n", row, len);
*/
    lseek (fd, (long) row*len, 0);
    if(read (fd, buf, len) == len)
	return 1;
    fprintf (stderr, "WARNING: error reading row %d of input file\n", row);
    G_zero (buf, len);
    return 1;
}
