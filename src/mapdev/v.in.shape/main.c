
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
 
/*03/2000 minor modifications to read data from shp2dig
 *         structures, after processing
 *04/2000 further modifications. Now splits lines at nodes
 *         and removes duplicate shapes.
 *
 *        Point file import now handled by separate module
 *         s.in.shape.
 *
 *05/2000
 *        Some  enhancements to assist in improving
 *         linework and fixing topological errors.
 *
 *          David D Gray  <ddgray@armadce.demon.co.uk>
 *
 *     
 ******************************************************************/


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
#include "shp2dig.h"
#include "dbutils.h"
#include "writelin.h"
#include "cleanup.h"
#include "bounds.h"


#define	round(x)	(int)((x) + 0.5)


static char *extract_base_name( char *, const char * );

int debug = 0;			/* debug level (verbosity) */
FILE *fde00, *fdlog;		/* input and log file descriptors */

double scale = 1.0;

int main( int   argc, char *argv[])
{
    SHPHandle	hShapeDB;
    DBFHandle   hDBF;
    double	adfMinBound[4], adfMaxBound[4];
    int		nShapeType, nShapes, iShape, iPart, iArc;
    int         iPoint, iRec, iField;
    int         pntCount;
    int		cat_field;
    int 	pgdmp, no_rattle;

    char name[512], rejname[512], *p;	/* name of output files */

    char infile[512], *newmapset;
    int cover_type;		/* type of coverage (line, point, area) */

    FILE *f_att = NULL;
    FILE *f_cats = NULL;
    struct Map_info map, rej;
    struct line_pnts *points;

    struct Categories cats;  /* added MN 10/99 */
    char    AttText[512];    /* added MN 10/99 */
    int attval;
    int lab_field = -1;


    /* DDG: Create structures for processing of shapefile contents */
    lineList *ll0;
    fieldDescript *fd0;
    segmentList *segl;
    BTREE *hVB;
    int fc1;

    char errbuf[1000];

    /* DDG: variables for controlling snap distance, slivers and scale */

    float sd0, psi;
    char *sdc, *cpsi;

    int init_scale;

    /************************************/

    double *pntxlist, *pntylist;
    double *xlab, *ylab;
    
    int (*btrkeycmp)(char *, char *);
    char buf[256];

	struct GModule *module;
    struct {
	struct Option *input, *logfile, *verbose, *attribute, *snapd, *minangle;
	struct Option *scale, *pgdump, *dumpmode, *catlabel;
    } parm;

    struct Flag *listflag, *rejflag;

    /* Are we running in Grass environment ? */

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Read an ArcView Shapefile.";

    /* define the different options */

    parm.input = G_define_option() ;
    parm.input->key        = "input";
    parm.input->type       = TYPE_STRING;
    parm.input->required   = YES;
    parm.input->description= "Name of .shp (or just .dbf) file to be imported";

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

    parm.snapd = G_define_option() ;
    parm.snapd->key        = "snapdist";
    parm.snapd->type       = TYPE_STRING;
    parm.snapd->required   = NO;
    parm.snapd->description= "Snap distance in ground units (Default = 10^-10)";
    parm.snapd->answer     = "1.0e-10";

    parm.minangle = G_define_option() ;
    parm.minangle->key        = "sliver";
    parm.minangle->type       = TYPE_STRING;
    parm.minangle->required   = NO;
    parm.minangle->description= "Min. angle subtended by a wedge at node (degrees - default 0.0001)";
    parm.minangle->answer     = "0.0001";

    parm.scale = G_define_option() ;
    parm.scale->key        = "scale";
    parm.scale->type       = TYPE_INTEGER;
    parm.scale->required   = NO;
    parm.scale->description= "Set initial scale [1:2400]";
    parm.scale->answer     = "2400";

    parm.attribute = G_define_option() ;
    parm.attribute->key        = "attribute";
    parm.attribute->type       = TYPE_STRING;
    parm.attribute->required   = NO;
    parm.attribute->description= "Name of attribute to use as category";
    parm.attribute->answer     = "";
    
    parm.catlabel = G_define_option() ;
    parm.catlabel->key        = "label";
    parm.catlabel->type       = TYPE_STRING;
    parm.catlabel->required   = NO;
    parm.catlabel->description= "Name of attribute to use as category label";
    parm.catlabel->answer     = "";
    

    /* Set flag for listing fields of database */

    listflag = G_define_flag();
    listflag->key     = 'l';
    listflag->description = "List fields of DBF file";

    /* Set flag for listing fields of database */

    rejflag = G_define_flag();
    rejflag->key     = 'r';
    rejflag->description = "Create reject lines file";

    /* get options and test their validity */

    if (G_parser(argc, argv))
	exit(-1);
    
    /* extract name of file and get names of dbf file */

    strcpy(infile, parm.input->answer);
    extract_base_name( name, infile );
    strcpy( rejname, name );
    strcat( rejname, "_rej" );


    /* Examine the flag `-l' first */

    if(listflag->answer) {
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

      exit( 0 );
    }

    debug = atoi( parm.verbose->answer);
    if (parm.logfile->answer == NULL)
	fdlog = stderr;
    else
	if ((fdlog = fopen( parm.logfile->answer, "w")) == NULL) {    
	    sprintf (buf, "Cannot open log file \"%s\"", parm.logfile->answer);
	    G_fatal_error( buf);
	}

    sdc = (char *)malloc(20);
    strncpy( sdc, parm.snapd->answer, 19 );
    sd0 = (float)atof(sdc);
    if( sd0 < 0 ) sd0 = -sd0;
    if( fabs( (double)sd0 ) < 1.0e-10 ) sd0 = 1.0e-10;

    free(sdc);

    if( procSnapDistance( SET_VAL, &sd0 ) != 0 ) {
      G_fatal_error( "Error setting snap distance" );
    }

    cpsi = (char *)malloc(20);
    strncpy( cpsi, parm.minangle->answer, 19 );
    psi = M_PI * (float)atof(cpsi) / 180.0;
    if( psi < 0 || psi > 1.5708 )
      psi = 1.74533e-6;

    free(cpsi);

    if( procMinSubtend( SET_VAL, &psi ) != 0 ) {
      G_fatal_error( "Error setting minimum angle" );
    }

    init_scale = atoi(parm.scale->answer);
    
    /* Open input file and verify that's a good shapefile file */

    hShapeDB = SHPOpen( infile, "r" );
    if (hShapeDB == NULL)
    {
	sprintf (buf, "%s - shapefile not found, or wrong format.\n", infile);
	G_fatal_error (buf);
    }

    if (debug)
	fprintf( fdlog, "\"%s\" successfully opened\n", infile);

    	
    /* Find the overall bounding box and set parameters for key values */

    if(set_bounds(hShapeDB) < 0)
      G_fatal_error("Could not determine limits of region");

    /* Establish the shape types and corresponding GRASS type */
    
    SHPGetInfo( hShapeDB, &nShapes, &nShapeType, adfMinBound, adfMaxBound );

    if( nShapeType == SHPT_MULTIPATCH ) {
      sprintf( buf, "Multipatch type not yet supported" );
      SHPClose( hShapeDB );
      G_fatal_error( buf );
    }

    if( nShapeType == SHPT_POINT || nShapeType == SHPT_MULTIPOINT || nShapeType == SHPT_POINTZ ||
	nShapeType == SHPT_MULTIPOINTZ || nShapeType == SHPT_POINTM || 
	nShapeType == SHPT_MULTIPOINTM ) {
      sprintf( buf, "Point map import not now supported by this module: use s.in.shape" );
      SHPClose( hShapeDB );
      G_fatal_error( buf );
    }
    
    switch (nShapeType) {
      case SHPT_ARC:
      case SHPT_ARCZ:
      case SHPT_ARCM:
        cover_type = LINE;
        break;

      case SHPT_POLYGON:
      case SHPT_POLYGONZ:
      case SHPT_POLYGONM:
        cover_type = AREA;
        break;
    }

    if( procMapType( SET_VAL, &cover_type ) != 0 ) 
      G_fatal_error( "Could not set map type. Aborting\n"  );
    

/* -------------------------------------------------------------------- */
/*      Create the GRASS vector layer based on the basename of the      */
/*      shapefile.							*/
/* -------------------------------------------------------------------- */
    Vect_open_new( &map, name);

    if(rejflag->answer)
      Vect_open_new( &rej, rejname);

/* -------------------------------------------------------------------- */
/*	Identify the attribute (if any) to be extracted.		*/
/* -------------------------------------------------------------------- */
    hDBF = NULL;
    if( strcmp(parm.attribute->answer,"") == 0 ) {
        cat_field = -1;
        
    } 

    else {
      int	i;
        
      hDBF = DBFOpen( infile, "r" );
      if( hDBF == NULL )
        {
	  sprintf (buf, "%s - DBF not found, or wrong format.\n", infile);
	  G_fatal_error (buf);
        }

      cat_field = -1;
      for( i = 0; i < DBFGetFieldCount(hDBF); i++ )
        {
	  char	field_name[15];

	  DBFGetFieldInfo( hDBF, i, field_name, NULL, NULL );
	  if( strcasecmp( parm.attribute->answer, field_name ) == 0 )
	    cat_field = i;
        }

      if( cat_field == -1 ) {
	sprintf( buf,
		 "No attribute `%s' found on %s.\nUse attribute=list to get a list of attributes.\n",
		 parm.attribute->answer, strcat(name, ".shp") );
            
	DBFClose( hDBF );
	SHPClose( hShapeDB );

	G_fatal_error( buf );
      }

      if (debug > 4)
	fprintf( fdlog, "Selected attribute field %d.\n", cat_field);


      if(hDBF != NULL) DBFClose( hDBF );
	
    }

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


    /* -------------------------------------------------------------------- */
    /*	Identify the category label to be used (if any)		            */
    /* -------------------------------------------------------------------- */

    if( strcmp(parm.catlabel->answer,"") == 0 ) {
        lab_field = -1;
        
    } 
    else {
      int	i;
        
      hDBF = DBFOpen( infile, "r" );
      if( hDBF == NULL )
        {
	  sprintf (buf, "%s - DBF not found, or wrong format.\n", infile);
	  G_fatal_error (buf);
        }

      /* Find field number of category label */ /* May '00 : DDG */

      if( strcmp(parm.catlabel->answer, "") != 0 ) {
	int j;

	for( j = 0; j < DBFGetFieldCount(hDBF); j++ )
	  {
	    char label_name[15];

	    DBFGetFieldInfo( hDBF, j, label_name, NULL, NULL );
	    if( strcasecmp( parm.catlabel->answer, label_name) == 0 ) {
	      lab_field = j;
	      break;
	    }
	    else lab_field = -2;
	  }
      }

      if( lab_field == -2 ) {
	sprintf( buf, "No attribute `%s' found on %s. \nNot writing category labels.\n",
		 parm.catlabel->answer, strcat(name, ".shp") );
	G_warning( buf );
      }


      if(hDBF != NULL) DBFClose( hDBF );
	
    }


  /* -------------------------------------------------------------------- */
  /*      DDG: Create the line descriptor list and field descriptor.      */
  /*           Also create line segments from vertex database.             */
  /* -------------------------------------------------------------------- */

    hDBF = DBFOpen( infile, "r" );
    if( hDBF == NULL )
      {
	sprintf (buf, "%s - DBF not found, or wrong format.\n", infile);
	G_fatal_error (buf);
      }

    /*****************************/

    ll0 = ( lineList *)malloc( sizeof( lineList ));
    fd0 = ( fieldDescript *)malloc( (DBFGetFieldCount(hDBF) + 4) * 
				    sizeof( fieldDescript ));

    hVB = (BTREE *)malloc(sizeof (BTREE) );
  
    /* Create V-base */
    btrkeycmp = btree_compare;
    if( !btree_create( hVB, btrkeycmp, 200 )) {
      sprintf( errbuf, "Cannot create database. Aborting" );
      G_fatal_error( errbuf );;
    }

    segl = ( segmentList *)malloc( sizeof( segmentList ));
    /* Initialise segment list */
    segl->origID = 0;
    segl->numSegments = 0;
    segl->segments = NULL;

    /* Read shape into line list and fill out V-base */
    linedCreate( ll0, hShapeDB, hDBF, fd0, hVB, &fc1 );

    /* Extract arcs from V-base into segment list */
    vbase2segd( segl, hVB );

    
    /*
     * Create the dig_cats file
     */
    if(lab_field >=  0) {
      G_init_cats( (CELL)0,"",&cats);
      G_write_vector_cats(name, &cats);
    }
    else fprintf( stderr, "Not assigning category labels\n" );
    /* if (G_write_vector_cats(name, &cats) != 1)
       G_fatal_error("Writing dig_cats file"); */
                        




    /* Check the number of records is the same as the number of lines */
    xlab = (double *)malloc( ll0->totalValidParts * sizeof( double ) );
    ylab = (double *)malloc( ll0->totalValidParts * sizeof( double ) );

    pntCount = 0;
    for( iShape = 0; iShape < nShapes; ++iShape ) {
      for( iPart = 0; iPart < ll0->lines[iShape].numParts; ++iPart ) {
	if( ll0->lines[iShape].parts[iPart].duff ) continue;
	xlab[pntCount] = ll0->lines[iShape].parts[iPart].centroid->xcentroid;
	ylab[pntCount++] = ll0->lines[iShape].parts[iPart].centroid->ycentroid;
      }	 
    } 

    /* -------------------------------------------------------------------- */
    /*      Scan segment list to extract and write arcs.                    */
    /* -------------------------------------------------------------------- */

    for( iArc = 0; iArc < segl->numSegments; ++iArc ) {
      if( segl->segments[iArc].duff ) continue;

      pntxlist = (double *)malloc( segl->segments[iArc].numVertices *
				   sizeof( double ) );
      pntylist = (double *)malloc( segl->segments[iArc].numVertices *
				   sizeof( double ) );
      for( iPoint = 0; iPoint < segl->segments[iArc].numVertices; ++iPoint ) {
	pntxlist[iPoint] = segl->segments[iArc].vertices[iPoint]->xPosn;
	pntylist[iPoint] = segl->segments[iArc].vertices[iPoint]->yPosn;
      }
      points = Vect_new_line_struct();
      Vect_copy_xy_to_pnts( points, pntxlist, pntylist,
			    segl->segments[iArc].numVertices );
      if(!segl->segments[iArc].duff)
	Vect_write_line( &map, cover_type, points);
      else if(rejflag->answer)
	Vect_write_line( &rej, cover_type, points);	
      Vect_destroy_line_struct( points );
	    
      free(pntxlist);
      free(pntylist);
      
    }
    

    /* Write attributes and category file */
	
    if( f_att != NULL ) {
      if(cat_field == -1)
	G_warning( "No attribute value field assigned. Using record ID.\n" );	
      else if(fd0[cat_field+4].fldType != 1 && fd0[cat_field+4].fldType != 2)
	G_warning( "Named attribute field is not numeric value. Using record ID.\n" );	
      for( iRec = 0; iRec < fd0[0].nRec; ++iRec ) {
	if( cover_type == LINE ) {
	  if(cat_field > -1 && fd0[cat_field+4].fldType == 1)
	    attval = fd0[cat_field+4].fldRecs[iRec].intField;
	  else if(cat_field > -1 && fd0[cat_field+4].fldType == 2)
	    attval = round( fd0[cat_field+4].fldRecs[iRec].doubleField );
	  else
	    attval = iRec + 1;
	  fprintf( f_att, "L  %-14f  %-14f  %-8d \n",
		   xlab[iRec], ylab[iRec], attval );
	}
	else if( cover_type == AREA ) {
	  if(cat_field > -1 && fd0[cat_field+4].fldType == 1)
	    attval = fd0[cat_field+4].fldRecs[iRec].intField;
	  else if(cat_field > -1 && fd0[cat_field+4].fldType == 2)
	    attval = round( fd0[cat_field+4].fldRecs[iRec].doubleField );
	  else
	    attval = iRec + 1;
	  fprintf( f_att, "A  %-14f  %-14f  %-8d \n",
		   xlab[iRec], ylab[iRec], attval );
	}

                     
	/* set cat for dig_cats file*/ /* M Neteler 10/99 */
	if( lab_field >= 0 ) {

	  switch(fd0[lab_field+4].fldType){
	  case 0:
	    strncpy(AttText, fd0[lab_field+4].fldRecs[iRec].stringField, 511 );
	    break;
	  case 1:
	    snprintf(AttText, 10, "%-d", fd0[lab_field+4].fldRecs[iRec].intField );
	    AttText[strlen(AttText)] = '\0';
	    break;
	  case 2:
	    snprintf(AttText, 20, "%-f", fd0[lab_field+4].fldRecs[iRec].doubleField );
	    AttText[strlen(AttText)] = '\0';
	    break;
	  default:
	    G_warning("Error in category type assignment, not assigning category text.\n");
	    strcpy(AttText, "");
	    break;
	  }

	  if (G_set_cat(attval, AttText, &cats) != 1)
	    G_fatal_error("Error setting category in dig_cats");
	}
	    
      }


      free( xlab );
      free( ylab );
    }

    if( lab_field >= 0 ) {
      G_write_vector_cats(name, &cats) != 0;
    }


	

    map.head.orig_scale = (long)init_scale;
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
        if(f_att) fclose( f_att );
    }
    /* segLDispose( segl );
    btree_free( hVB );
    linedDispose( ll0, fd0, fc1 );
    */

    /* Apply post-processing procedures to clean up map */
    vector_map_cleanup(name);
    
    exit(0);
}


static char *extract_base_name( char *base_name, const char *in_name ) {
  
  /* Extract the basename of a fullname of the input file. ie. the answer to
     the option `in'
  */

  char *name;
  char *p;

  name = (char *)malloc(strlen(in_name)+1);
  strcpy( name, in_name );

  for( p = name+strlen(name)-1;
       p != name-1 && (isalnum(*p) || *p == '_' || *p == '.' );
       p-- ) {}
  strcpy( base_name, p+1);
  free(name);
    
  p = strrchr( base_name, '.');
  if (p != NULL)
    *p = '\0';
  base_name = p;
  return base_name;
}
