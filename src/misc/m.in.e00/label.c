#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "site.h"
#include "Vect.h"
#include "local_proto.h"

/**********************************************************************/
/*                                                                    */
/* getlabels - import labels section from e00 - M. Wurtz (1998-10-10) */
/*                                                                    */
/**********************************************************************/

extern int debug;		/* debug level (verbosity) */
extern double scale;		/* scale of coordinates (Cf PRJ) */
extern FILE *fdlog;	        /* log file descriptor */

void getlabels( char *name, int cover_type, int prec)
{
    FILE *f;
    int num, id;		/* coverage-# and coverage-ID */
    double x, y;                /* coordinates		    */

    char line[84];              /* line buffer for reading  */
    char type;			/* 'A' (area) or 'L' (line) */
    extern FILE *G_fopen_new( char*, char*);
    extern FILE *G_fopen_append( char*, char*);
    extern char *G_find_file( char*, char*, char*);
    extern char *G_mapset();

    if (cover_type == LINE)
	type = 'L';
    else
	type = 'A';
    if (G_find_file( "dig_att", name, G_mapset()) == NULL) {
	f = G_fopen_new( "dig_att", name);
	if (debug)
	    fprintf( fdlog, "Creating dig_att(%c) file \"%s\"\n", type, name);
    } else {
	f = G_fopen_append( "dig_att", name);
	if (debug)
	    fprintf( fdlog, "Updating dig_att(%c) file \"%s\"\n", type, name);
    }
    if (f == NULL)
	G_fatal_error( "Unable to create attribute file");
    for(;;) {
	read_e00_line( line);
        sscanf( line, "%d%d%lf%lf", &id, &num, &x, &y);
        if (id == -1)
            break;
        fprintf( f, "%c  %-12f  %-12f  %-8d \n", type,
		x*scale, y*scale, num);
	read_e00_line( line);		/* 4 values to skip */
	if (prec)
	    read_e00_line( line);	/* on 2 line when double precision */
    }
    fclose( f);

}

/**********************************************************************/
/*                                                                    */
/* getsites - create a site file from e00 - M. Wurtz (1998-10-10)     */
/* must be changed, and use PAT info to fill the site structure       */
/*                                                                    */
/**********************************************************************/

void getsites( char *name, int prec)
{
    FILE *f;
    int id;
    double x, y;		/* coordinates */
    char line[84];      	/* line buffer for reading */
    Site *sites;

    if (debug)
	fprintf( fdlog, "Creating Site file \"%s\"\n", name);
    if ((f = G_sites_open_new( name)) == NULL)
	G_fatal_error( "Unable to create site file");
    sites = G_site_new_struct( CELL_TYPE, 2, 0, 0);

    for(;;) {
	read_e00_line( line);
        sscanf( line, "%d%*d%lf%lf", &id, &x, &y);
        if (id == -1)
            break;
	sites->east = x;
	sites->north = y;
	sites->ccat = id;
	G_site_put( f, sites);
	read_e00_line( line);		/* 4 values to skip */
	if (prec)
	    read_e00_line( line);	/* on 2 line when double precision */
    }
    fclose( f);
    G_site_free_struct( sites);
}
