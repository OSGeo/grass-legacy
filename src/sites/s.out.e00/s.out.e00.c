#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <ctype.h>
#include <math.h>
#include <signal.h>
#include "gis.h"
#include "Vect.h"

/******************************************************************/
/*                                                                */
/* s.out.e00 -- export sites to an ESRI e00 archive               */
/*  - M. Wurtz (v1.1) jan 2000                                    */
/*                                                                */
/* This program is an attempt to write a .e00 file                */
/* Since e00 is NOT a public format, this program                 */
/* is mainly based on analysis of existing files.                 */
/*                                                                */
/* There is then no warranty about this program and you are       */
/* warned that it will run at your own risks.                     */
/*                                                                */
/******************************************************************/

FILE *fsite, *fde00;		/* input and output file descriptors */

int main( int argc, char *argv[]) 
{
    char *infile, *outfile;	/* name of output files */
    char msg[128];		/* for error messages */
    char name[32], nm[32], *p;	/* name of cover */
    char nnu[32], nid[32];
    double xmin, xmax, ymin, ymax;

    RASTER_MAP_TYPE cat;
    int ns, *id;		/* number of sites */
    int dims, strs, dbls;
    struct Cell_head region;
    struct GModule *module;
    Site *site;

    struct {
	struct Option *input, *output;
    } parm;

    /* Are we running in Grass environment ? */

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =        
                    "Write an Arc-Info point coverage in e00 format.";
                    
    /* define the different options */

    parm.input = G_define_option() ;
    parm.input->key        = "input";
    parm.input->type       = TYPE_STRING;
    parm.input->required   = YES;
    parm.input->description= "Name of site file to be exported";

    parm.output = G_define_option() ;
    parm.output->key        = "output";
    parm.output->type       = TYPE_STRING;
    parm.output->required   = NO;
    parm.output->description= "Name of .e00 output file";


    /* get options and test their validity */

    if (G_parser(argc, argv))
	exit(-1);
    
    infile = parm.input->answer;
    outfile = parm.output->answer;
    strncpy( name, infile, 27);
    for (p=name; *p; p++) {
	if (!isalnum( *p))
	    if (p == name)
		*p = 'X';
	    else
		*p = 0;
    }
    G_toucase( name);

    /* Open input file */

    if ((fsite = G_sites_open_old( infile, G_mapset())) == NULL)
	G_fatal_error( "Site file not found\n");
    if (G_site_describe( fsite, &dims, &cat, &strs, &dbls))
	G_fatal_error( "Failed to guess site file format\n");
    if ((site = G_site_new_struct( cat, dims, strs, dbls)) == NULL)
	G_fatal_error( "Unable to allocate site structure\n"); 

    /* Open output file */

    if (outfile == NULL)
	fde00 = stdout;
    else
	if ((fde00 = fopen( outfile, "w")) == NULL) {    
	    sprintf (msg, "Cannot open output file \"%s\"", outfile);
	    G_fatal_error( msg);
	}


    fprintf( fde00, "EXP  0 %s/site_lists/%s\n", G_location_path(), infile);

    G_get_window( &region);
    xmin = region.east;
    xmax = region.west;
    ymin = region.south;
    ymax = region.north;

    /* LAB SECTION */

    ns = 0;
    fprintf( fde00, "LAB  3\n");
    while( G_site_get( fsite, site) == 0) {
	fprintf( fde00, "%10d%10d%21.14E%21.14E\n",
		 ++ns, 0, site->east, site->north);
	fprintf( fde00, "%21.14E%21.14E\n", site->east, site->north);
	fprintf( fde00, "%21.14E%21.14E\n", site->east, site->north);

	if (site->east < xmin)
	    xmin = site->east;
	if (site->east > xmax)
	    xmax = site->east;
	if (site->north < ymin)
	    ymin = site->north;
	if (site->north > ymax)
	    ymax = site->north;
    }
    fprintf( fde00, "%10d%10d%21.14E%21.14E\n", -1, 0, 0.0, 0.0);
    fclose( fsite);

    /* no TOLERANCE SECTION */
    /* must put e00write_tol() here */

    /* no SPATIAL INDEX SECTION */
    /* must put e00write_sin() here */

    /* no LOG SECTION */
    /* must put e00write_log() here */

    /* no PROJECTION INFOS */
    /* must put e00write_prj() here */

    /* INFO SECTION */

    fprintf( fde00, "IFO  2\n");
    strcpy( nm, name);
    strncat( nm, ".BND", 31);

    fprintf( fde00, "%-32.32sXX   4   4  16         1\n", nm);

    fprintf( fde00, "%s\n%s\n%s\n%s\n",
      "XMIN              4-1   14-1  12 3 60-1  -1  -1-1                   1-",
      "YMIN              4-1   54-1  12 3 60-1  -1  -1-1                   2-",
      "XMAX              4-1   94-1  12 3 60-1  -1  -1-1                   3-",
      "YMAX              4-1  134-1  12 3 60-1  -1  -1-1                   4-");
    fprintf( fde00, "%14.7E%14.7E%14.7E%14.7E\n", xmin, xmax, ymin, ymax);

    fsite = G_sites_open_old( infile, G_mapset());
    
    strncpy( nnu, name, 13);
    strncpy( nid, name, 13);
    strcpy( nm, name);
    strncat( nm, ".PAT", 31);
    strncat( nnu, "#", 31);
    strncat( nid, "-ID", 31);

    fprintf( fde00, "%-32.32sXX   4   4  16%10ld\n", nm, ns);

    fprintf( fde00, "%-16.16s%s\n%-16.16s%s\n%-16.16s%s\n%-16.16s%s\n",
	"AREA", "  4-1   14-1  12 3 60-1  -1  -1-1                   1-",
	"PERIMETER", "  4-1   54-1  12 3 60-1  -1  -1-1                   2-",
	nnu, "  4-1   94-1   5-1 50-1  -1  -1-1                   3-",
	nid, "  4-1  134-1   5-1 50-1  -1  -1-1                   4-");

    if (site->cattype == CELL_TYPE)
	id = &(site->ccat);
    else
	id = &ns;
    ns = 0;
    while (G_site_get( fsite, site) == 0) {
	fprintf( fde00, "%14.7E%14.7E%11ld%11ld\n", 0.0, 0.0, ++ns, *id);
    }

    fclose( fsite);
    G_site_free_struct( site);
    fprintf( fde00, "EOI\nEOS\n");
    fclose( fde00);
}
