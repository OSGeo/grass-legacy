#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "local_proto.h"

/********************************************************************/
/*                                                                  */
/* getraster - import grid component of e00 - M. Wurtz (1998-10-10) */
/*                                                                  */
/********************************************************************/

extern int debug;		/* debug level (verbosity) */
extern double scale;		/* scale of coordinates (Cf PRJ) */
extern FILE *fdlog;		/* log file descriptor */

void getraster( char *name, int flag, int prec)
{
    long rows, cols, depth;
    double xres, yres;
    double xmin, ymin;
    double xmax, ymax;
    char line[1024];

    struct Cell_head region;	/* region and cellhd structure */
    int raster;			/* file descriptor for raster */
    CELL *buf;			/* buffer for data */
    FCELL *fbuf;
    DCELL *dbuf;

    long i, j, *p;
    float *f;
    double *d;

    if (debug)
	fprintf( fdlog, "Extracting grid\n");

    read_e00_line( line);
    sscanf( line, "%ld%ld%ld", &cols, &rows, &depth);
    read_e00_line( line);
    sscanf( line, "%lf%lf", &xres, &yres);
    read_e00_line( line);
    sscanf( line, "%lf%lf", &xmin, &ymin);
    read_e00_line( line);
    sscanf( line, "%lf%lf", &xmax, &ymax);

    if (debug > 3) {
	fprintf( fdlog, "%ld cols, %ld rows, depth = %ld\n", cols, rows, depth);
	fprintf( fdlog, "xres = %f, yres = %f\n", xres, yres);
	fprintf( fdlog, "xmin = %f, ymin = %f\n", xmin, ymin);
	fprintf( fdlog, "xmax = %f, ymax = %f\n", xmax, ymax);
    }

    if (flag == 0) {			/* Only analyse */
	ignore( "EOG", 0);
	return;
    }

    if (debug)
	fprintf( fdlog, "Creating region\n");

    G_get_set_window( &region);		/* Use the region defined in getinfo */

    region.rows = rows;
    region.cols = cols;
    region.east = xmax * scale;		/* scale is 1.0 but PRJ modify it */
    region.west = xmin * scale;		/* to be corrected : we must read */
    region.north = ymax * scale;	/* PRJ section before GRD section */
    region.south = ymin * scale;
    G_adjust_Cell_head( &region, 1, 1);	/* compute resolution */
/***************************
    if (region.ew_res != xres * scale || region.ns_res != yres * scale)
	G_fatal_error( "resolution don't match extension");
***************************/
    region.format = 4;			/* should be gessed from info .STA */
    region.compressed = 0;		/* default not compressed */

    if (flag > 1)			/* We could create a WIND file */
	G_put_window( &region);
    G_set_window( &region);

    if (debug)
	fprintf( fdlog, "Creating raster file \"%s\"\n", name);


    switch (depth) {
    case 1 :
	raster = G_open_cell_new( name);
	buf = G_allocate_c_raster_buf();
	fprintf( stderr, "Percent complete : ");
	for (i=0; i < rows; i++) {
	/* we assume that CELL = long and that sscanf only use */
	/* the pointers for which a value is found in line */
	    p = (long *) buf;
	    for (j = 0; j < cols; j += 5) {
		read_e00_line( line);
		if ((cols - j) < 5)
		    line[ 14 * (cols-j+1)] = 0;
		sscanf( line, "%ld%ld%ld%ld%ld", p, p+1, p+2, p+3, p+4);
		p += 5;
	    }
	    G_put_c_raster_row( raster, buf);
	    G_percent( i, rows, 10);
	}
	G_percent( i, rows, 10);
	G_close_cell( raster);
	break;
    case 2 :			/* no float data in cell - wait for grass 5.0 */
	if (prec) {
	    G_set_fp_type( DCELL_TYPE);
	    raster = G_open_fp_cell_new( name);
	    dbuf = G_allocate_d_raster_buf();
	    fprintf( stderr, "Percent complete : ");
	    for (i=0; i < rows; i++) {
	    /* we assume that sscanf only use */
	    /* the pointers for which a value is found in line */
		d = (double *) dbuf;
		for (j = 0; j < cols; j += 3) {
		    read_e00_line( line);
		    if ((cols - j) < 3)
			line[ 21 * (cols-j+1)] = 0;
		    sscanf( line, "%lf%lf%lf", d, d+1, d+2);
		    d += 3;
		}
		G_put_d_raster_row( raster, dbuf);
		G_percent( i, rows, 10);
	    }
	} else {
	    G_set_fp_type( FCELL_TYPE);
	    raster = G_open_fp_cell_new( name);
	    fbuf = G_allocate_f_raster_buf();
	    fprintf( stderr, "Percent complete : ");
	    for (i=0; i < rows; i++) {
	    /* we assume that sscanf only use */
	    /* the pointers for which a value is found in line */
		f = (float *) fbuf;
		for (j = 0; j < cols; j += 5) {
		    read_e00_line( line);
		    if ((cols - j) < 5)
			line[ 14 * (cols-j+1)] = 0;
		    sscanf( line, "%f%f%f%f%f", f, f+1, f+2, f+3, f+4);
		    f += 5;
		}
		G_put_f_raster_row( raster, fbuf);
		G_percent( i, rows, 10);
	    }
	}
	G_percent( i, rows, 10);
	G_close_cell( raster);
	break;
    default:
	fprintf( stderr, "Unknown grid format %ld ; operation aborted\n", depth);
	if (fdlog != stderr)
	    fprintf( fdlog, "Unknown grid format %ld ; operation aborted\n",
		depth);
	ignore( "EOG", 0);
	return;
    }
    ignore( "EOG", 0);
    return;
}
