#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "gis.h"

/********************************************************************/
/*                                                                  */
/* getraster - import grid component of e00 - M. Wurtz (1998-10-10) */
/*                                                                  */
/********************************************************************/

extern int debug;		/* debug level (verbosity) */
extern double scale;		/* scale of coordinates (Cf PRJ) */
extern FILE *fde00, *fdlog;	/* input and log file descriptors */

extern int G_open_cell_new( char *name);
extern CELL *G_allocate_cell_buf();
extern void G_adjust_Cell_head( struct Cell_head*, int, int);

void getraster( char *name, int flag)
{
    long rows, cols, depth;
    double xres, yres;
    double xmin, ymin;
    double xmax, ymax;
    char line[1024];

    struct Cell_head region;	/* region and cellhd structure */
    int raster;			/* file descriptor for raster */
    CELL *buf;			/* buffer for data */

    long i, j, val, *p;

    if (debug)
	fprintf( fdlog, "Extracting grid\n");

    fscanf( fde00, "%ld", &cols);
    fscanf( fde00, "%ld", &rows);
    fscanf( fde00, "%ld", &depth);
    fgets( line, 1024, fde00);
    fscanf( fde00, "%lf%lf%lf%lf%lf%lf",
	&xres, &yres, &xmin, &ymin, &xmax, &ymax);

    if (debug > 3) {
	fprintf( fdlog, "%ld cols, %ld rows, depth = %d\n", cols, rows, depth);
	fprintf( fdlog, "xres = %lf, yres = %lf\n", xres, yres);
	fprintf( fdlog, "xmin = %lf, ymin = %lf\n", xmin, ymin);
	fprintf( fdlog, "xmax = %lf, ymax = %lf\n", xmax, ymax);
    }

    if (flag == 0) {			/* Only analyse */
	ignore( "EOG");
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
/***************************
    region.ew_res = xres * scale;
    region.ns_res = yres * scale;
***************************/
    G_adjust_Cell_head( &region, 1, 1);	/* compute resolution */
    region.format = 4;			/* should be gessed from info .STA */
    region.compressed = 0;		/* default not compressed */

    if (flag > 1)			/* We could create a WIND file */
	G_put_window( &region);
    G_set_window( &region);

    if (debug)
	fprintf( fdlog, "Creating raster file \"%s\"\n", name);

    raster = G_open_cell_new( name);
    buf = G_allocate_cell_buf();

    switch (depth) {
    case 1 :
	fprintf( stderr, "Percent complete : ");
	for (i=0; i < rows; i++) {
	    p = (long *) buf;
	    for (j = 0; j < cols; j++) {
		fscanf( fde00, "%d", &val);
		*p++ = val;
	    }
	    G_put_map_row( raster, buf);
	    G_percent( i, rows, 10);
	}
	G_percent( i, rows, 10);
	G_close_cell( raster);
	break;
    case 2 :			/* no float data in cell - wait for grass 5.0 */
    default:
	fprintf( stderr, "Unknown grid format %d ; operation aborted\n", depth);
	fprintf( fdlog, "Unknown grid format %d ; operation aborted\n", depth);
	ignore( "EOG");
	return;
    }
    ignore( "EOG");
    return;
}
