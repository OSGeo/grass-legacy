#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "gis.h"
#include "Vect.h"

/******************************************************************/
/*                                                                */
/* getarcs - import arc geometry from e00 - M. Wurtz (1998-10-10) */
/*                                                                */
/******************************************************************/

extern int debug;               /* debug level (verbosity) */
extern double scale;            /* scale of coordinates (Cf PRJ) */
extern FILE *fde00, *fdlog;     /* input and log file descriptors */

int getarcs( char *name, int cover_type)
{
    struct Map_info map;
    struct line_pnts *points;
    double *x, *y;
    char line[1024];
    int covnum, cov_id, fnode, tnode, lpol, rpol, npts;
    int i, j;
    double xc, yc;
    double xmin, ymin, xmax, ymax;
    long offset;
    FILE *f;

    extern FILE *G_fopen_new( char*, char*);
    extern FILE *G_fopen_append( char*, char*);
    extern char *G_find_file( char*, char*, char*);
    extern char *G_mapset();

    for (j=0;;j++) {
	fscanf( fde00, "%d %d %d %d %d %d %d", &covnum, &cov_id, &fnode, &tnode,
		&lpol, &rpol, &npts);
	if (covnum == -1)
	    break;
	else if (j == 0) {
	    Vect_open_new( &map, name);
	    if (G_find_file( "dig_att", name, G_mapset()) == NULL) {
		f = G_fopen_new( "dig_att", name);
		if (debug)
		    fprintf( fdlog, "Creating dig_att(L) file \"%s\"\n", name);
	    } else {
		f = G_fopen_append( "dig_att", name);
		if (debug)
		    fprintf( fdlog, "Updating dig_att(L) file \"%s\"\n", name);
	    }
	    if (f == NULL)
		G_fatal_error( "Unable to create attribute file");
	}
	if (debug > 4)
	    fprintf( fdlog, "line %d (%d pnts) id=%d\n", j, npts, cov_id);
	x = (double *)G_malloc( sizeof(double) * npts);
	y = (double *)G_malloc( sizeof(double) * npts);
	for (i = 0; i < npts; i++) {
	    fscanf( fde00, "%lf %lf", x+i, y+i);
	    if (scale != 1.0) {
		x[i] *= scale;
		y[i] *= scale;
	    }
	    if (i == 0 && j == 0) {
		xmin = xmax = x[0];
		ymin = ymax = y[0];
		continue;
	    }
	    if (xmin > x[i])
		xmin = x[i];
	    if (xmax < x[i])
		xmax = x[i];
	    if (ymin > y[i])
		ymin = y[i];
	    if (ymax < y[i])
		ymax = y[i];
	}
	points = Vect_new_line_struct();
	Vect_copy_xy_to_pnts( points, x, y, npts);
	offset = Vect_write_line( &map, cover_type, points);
	xc = x[(npts+1)/2]; yc = y[(npts+1)/2];
	if (cover_type == LINE)
            fprintf( f, "L  %-12lf  %-12lf  %-8d \n", xc, yc, cov_id);
	Vect_destroy_line_struct( points);
	free( x); free( y);
    }
    if (j == 0)
	return DOT;
    map.head.orig_scale = 100000l;
    G_strncpy( map.head.your_name, G_whoami(), 20);
    G_strncpy( map.head.date, G_date(), 20);
    G_strncpy( map.head.map_name, name, 20);
    map.head.W = xmin;
    map.head.S = ymin;
    map.head.E = xmax;
    map.head.N = ymax;
    Vect_close( &map);
    fclose( f);
    return cover_type;
}

