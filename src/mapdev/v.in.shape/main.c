#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <ctype.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include "gis.h"
#include "Vect.h"
#include "shapefil.h"

/******************************************************************/
/*                                                                */
/* v.in.shape -- import an ESRI Shapefile			  */
/*                                                                */
/* Frank Warmerdam, warmerda@home.com				  */
/* Based on my shapelib library:				  */
/*   http://gdal.velocet.ca/projects/shapelib/			  */
/******************************************************************/

/* 10/1999 added dig_cats file support
 *         Markus Neteler neteler@geog.uni-hannover.de
 *
 ******************************************************************/

enum {ANALYSE, RASTER, LINES, VECTOR, ALL} todo;

int debug = 0;			/* debug level (verbosity) */
FILE *fde00, *fdlog;		/* input and log file descriptors */

double scale = 1.0;

int main( int   argc, char *argv[])
{
    SHPHandle	hShapeDB;
    DBFHandle   hDBF;
    double	adfMinBound[4], adfMaxBound[4];
    int		nShapeType, nShapes, iShape;
    int		cat_field;

    char name[128], *p;	/* name of output files */

    char *infile, *newmapset;
    int cover_type;		/* type of coverage (line, point, area) */

    FILE *f_att = NULL;
    struct Map_info map;
    struct line_pnts *points;

    struct Categories cats;  /* added MN 10/99 */
    char    AttText[512];    /* added MN 10/99 */
    
    char buf[256];

    struct {
	struct Option *input, *mapset, *logfile, *verbose, *attribute;
    } parm;

    /* Are we running in Grass environment ? */

    G_gisinit (argv[0]);

    /* define the different options */

    parm.input = G_define_option() ;
    parm.input->key        = "input";
    parm.input->type       = TYPE_STRING;
    parm.input->required   = YES;
    parm.input->description= "Name of .shp file to be imported";

    parm.mapset = G_define_option() ;
    parm.mapset->key        = "mapset";
    parm.mapset->type       = TYPE_STRING;
    parm.mapset->required   = NO;
    parm.mapset->description= "Name of mapset to hold resulting files (Default = current)";

    parm.verbose = G_define_option() ;
    parm.verbose->key        = "verbose";
    parm.verbose->type       = TYPE_INTEGER;
    parm.verbose->required   = NO;
    parm.verbose->description= "Debugging level : 0 (silent) - 9 (verbose)" ;
    parm.verbose->answer     = "0" ;

    parm.logfile = G_define_option() ;
    parm.logfile->key        = "logfile";
    parm.logfile->type       = TYPE_STRING;
    parm.logfile->required   = NO;
    parm.logfile->description= "Name of file where log operations";

    parm.attribute = G_define_option() ;
    parm.attribute->key        = "attribute";
    parm.attribute->type       = TYPE_STRING;
    parm.attribute->required   = NO;
    parm.attribute->description= "Name of attribute to use as category";
    parm.attribute->answer     = "";

    /* get options and test their validity */

    if (G_parser(argc, argv))
	exit(-1);
    
    infile = parm.input->answer;
    newmapset = parm.mapset->answer;

    debug = atoi( parm.verbose->answer);
    if (parm.logfile->answer == NULL)
	fdlog = stderr;
    else
	if ((fdlog = fopen( parm.logfile->answer, "w")) == NULL) {    
	    sprintf (buf, "Cannot open log file \"%s\"", parm.logfile->answer);
	    G_fatal_error( buf);
	}
    
    /* Open input file and verify that's a good shapefile file */

    hShapeDB = SHPOpen( infile, "r" );
    if (hShapeDB == NULL)
    {
	sprintf (buf, "%s - not found, or wrong format.\n", infile);
	G_fatal_error (buf);
    }

    if (debug)
	fprintf( fdlog, "\"%s\" successfully opened\n", infile);

    /* Create a mapset and made it current mapset for this program */

    if (newmapset != NULL) {
	if (G_legal_filename( newmapset) < 0) {
	    sprintf (buf, "MAPSET <%s> - illegal name\n", newmapset);
	    G_fatal_error( buf);
	}
	if (todo == ANALYSE)
	    fprintf( fdlog, "Mapset %s not created (analyse only)\n", newmapset);
	else {    
	    sprintf( buf, "%s/%s", G_location_path(), newmapset);
	    if (access( buf, F_OK) == -1)
		if (mkdir( buf, 0755) == -1) {
		    sprintf( buf, "Cannot create MAPSET %s", newmapset);
		    G_fatal_error( buf);
		}
	    G__setenv( "MAPSET", newmapset);
	    if (debug > 2)
		fprintf( fdlog, "Mapset \"%s\" created for import\n", G_mapset());
	}
    }

    /* Establish the shape types and corresponding GRASS type */
    
    SHPGetInfo( hShapeDB, &nShapes, &nShapeType, adfMinBound, adfMaxBound );
    
    switch (nShapeType) {
      case SHPT_POINT:
      case SHPT_MULTIPOINT:
      case SHPT_POINTZ:
      case SHPT_MULTIPOINTZ:
      case SHPT_POINTM:
      case SHPT_MULTIPOINTM:
        cover_type = DOT;
        break;

      case SHPT_ARC:
      case SHPT_ARCZ:
      case SHPT_ARCM:
        cover_type = LINE;

      case SHPT_POLYGON:
      case SHPT_POLYGONZ:
      case SHPT_POLYGONM:
        cover_type = AREA;
        break;
    }
    
/* -------------------------------------------------------------------- */
/*      Extract basename of shapefile.                                  */
/* -------------------------------------------------------------------- */
    for( p = infile+strlen(infile)-1;
         p != infile-1 && (isalnum(*p) || *p == '_' || *p == '.' );
         p-- ) {}
    strcpy( name, p+1);
    
    p = strrchr( name, '.');
    if (p != NULL)
        *p = '\0';

    if (debug > 4)
	fprintf( fdlog, "Name of output file is \"%s\"\n", name);

/* -------------------------------------------------------------------- */
/*      Create the GRASS vector layer based on the basename of the      */
/*      shapefile.							*/
/* -------------------------------------------------------------------- */
    Vect_open_new( &map, name);

/* -------------------------------------------------------------------- */
/*	Identify the attribute (if any) to be extracted.		*/
/* -------------------------------------------------------------------- */
    hDBF = NULL;
    if( strcmp(parm.attribute->answer,"") == 0 ) {
        cat_field = -1;
        
    } else if( strcmp(parm.attribute->answer,"list") == 0 ) {
        int	i;
        
        hDBF = DBFOpen( infile, "r" );
        if( hDBF == NULL )
        {
            sprintf (buf, "%s - DBF not found, or wrong format.\n", infile);
            G_fatal_error (buf);
        }

        fprintf (stdout, "Attributes available in %s\n", infile );
        for( i = 0; i < DBFGetFieldCount(hDBF); i++ )
        {
            char	field_name[15];

            DBFGetFieldInfo( hDBF, i, field_name, NULL, NULL );
            fprintf (stdout, "%s\n", field_name );
        }
        
        DBFClose( hDBF );
        SHPClose( hShapeDB );

        exit( 0 );
        
    } else {
        int	i;
        
        hDBF = DBFOpen( infile, "r" );
        if( hDBF == NULL )
        {
            sprintf (buf, "%s - DBF not found, or wrong format.\n", infile);
            G_fatal_error (buf);
        }

        for( i = 0; i < DBFGetFieldCount(hDBF); i++ )
        {
            char	field_name[15];

            DBFGetFieldInfo( hDBF, i, field_name, NULL, NULL );
            if( strcasecmp( parm.attribute->answer, field_name ) == 0 )
                cat_field = i;
        }

        if( cat_field == -1 ) {
            sprintf( buf,
                     "No attribute `%s' found on %s.\n"
                     "Use attribute=list to get a list of attributes.\n",
                     parm.attribute->answer, infile );
            
            DBFClose( hDBF );
            SHPClose( hShapeDB );

            G_fatal_error( buf );
        }

        if (debug > 4)
            fprintf( fdlog, "Selected attribute field %d.\n", cat_field);

        /*
         * Create the dig_att file (or append).
         */
        if (G_find_file( "dig_att", name, G_mapset()) == NULL) {
            f_att = G_fopen_new( "dig_att", name);
            if (debug)
                fprintf( fdlog, "Creating dig_att(L) file \"%s\"\n", name);
        } else {
            f_att = G_fopen_append( "dig_att", name);
            if (debug)
                fprintf( fdlog, "Updating dig_att(L) file \"%s\"\n", name);
        }
        if (f_att == NULL)
        {
            sprintf( buf, "Unable to create attribute file `%s'.", name );
            G_fatal_error( buf );
        }

        /*
         * Create the dig_cats file
         */
        G_init_cats(nShapes,(char *)NULL,&cats);
        if (G_write_vector_cats(name, &cats) != 1)
                    G_fatal_error("Writing dig_cats file");
                        
    }

  /* -------------------------------------------------------------------- */
  /*      Loop over each shape in the file.                               */
  /* -------------------------------------------------------------------- */
    for( iShape = 0; iShape < nShapes; iShape++ )
    {
        SHPObject	*psShape = SHPReadObject( hShapeDB, iShape );

        if( psShape->nVertices == 0 )
        {
            SHPDestroyObject( psShape );
            continue;
        }

	points = Vect_new_line_struct();
	Vect_copy_xy_to_pnts( points, psShape->padfX, psShape->padfY,
                              psShape->nVertices );
	Vect_write_line( &map, cover_type, points);
	Vect_destroy_line_struct( points );

        if( f_att != NULL ) {
            double	xc, yc;

            if( psShape->nVertices == 1 )
            {
                xc = psShape->padfX[0];
                yc = psShape->padfY[0];
            }
            else 
            {
                xc = (psShape->padfX[0] + psShape->padfX[1]) / 2.0;
                yc = (psShape->padfY[0] + psShape->padfY[1]) / 2.0;
            }
            
            fprintf( f_att, "L  %-12f  %-12f  %-8d \n",
                     xc, yc,
                     DBFReadIntegerAttribute( hDBF, iShape, cat_field ) );
                     
            /* set cat for dig_cats file*/ /* M Neteler 10/99 */
            sprintf(AttText, "%-8d", DBFReadIntegerAttribute( hDBF, iShape, cat_field ));
            if (G_set_cat(iShape, AttText, &cats) != 1)
                       G_fatal_error("Call to G_set_cats");
        }

        SHPDestroyObject( psShape );
    }

    map.head.orig_scale = 100000l;
    G_strncpy( map.head.your_name, G_whoami(), 20);
    G_strncpy( map.head.date, G_date(), 20);
    G_strncpy( map.head.map_name, name, 20);
    map.head.W = adfMinBound[0];
    map.head.S = adfMinBound[1];
    map.head.E = adfMaxBound[0];
    map.head.N = adfMaxBound[1];

    Vect_close( &map);

    SHPClose( hShapeDB );

    if( hDBF != NULL )
    {
        DBFClose( hDBF );
        fclose( f_att );
    }
    
    exit(0);
}

