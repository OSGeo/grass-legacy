#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "dig_defines.h"

/**********************************************************************/
/*                                                                    */
/* getlabels - import labels section from e00 - M. Wurtz (1998-10-10) */
/*                                                                    */
/**********************************************************************/

extern int debug;		/* debug level (verbosity) */
extern double scale;		/* scale of coordinates (Cf PRJ) */
extern FILE *fde00, *fdlog;	/* input and log file descriptors */

void getlabels( char *name, int cover_type)
{
    FILE *f;
    int id;
    double x, y;                /* coordinates		    */
    char line[1024];            /* line buffer for reading  */
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
    while (fgets( line, 1024, fde00) != NULL) {
        sscanf( line, "%d%*d%lf%lf", &id, &x, &y);
        if (id == -1)
            break;
        fprintf( f, "%c  %-12lf  %-12lf  %-8d \n", type,
		x*scale, y*scale, id);
        fscanf( fde00, "%lf%lf", &x, &y);	/* 4 values to skip */
        fscanf( fde00, "%lf%lf", &x, &y);
	fgets( line, 1024, fde00);		/* skip end of line */
    }
    fclose( f);

}

/**********************************************************************/
/*                                                                    */
/* getsites - create a site file from e00 - M. Wurtz (1998-10-10)     */
/*                                                                    */
/**********************************************************************/

void getsites( char *name)
{
    FILE *f;
    int id;
    double x, y;		/* coordinates */
    char ident[8];		/* ident */
    char line[1024];            /* line buffer for reading */
    extern FILE *G_fopen_sites_new( char *name);

    if (debug)
	fprintf( fdlog, "Creating Site file \"%s\"\n", name);
    if ((f = G_fopen_sites_new( name)) == NULL)
	G_fatal_error( "Unable to create site file");
    while (fgets( line, 1024, fde00) != NULL) {
        sscanf( line, "%d%*d%lf%lf", &id, &x, &y);
        if (id == -1)
            break;
	sprintf( ident, "%d", id);
	G_put_site( f, x, y, ident);
        fscanf( fde00, "%lf%lf", &x, &y);	/* 4 values to skip */
        fscanf( fde00, "%lf%lf", &x, &y);
	fgets( line, 1024, fde00);		/* skip end of line */
    }
    fclose( f);
}
