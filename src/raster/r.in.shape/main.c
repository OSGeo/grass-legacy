/*
 * $Id$
 */

/******************************************************************/
/*                                                                */
/* r.in.shape -- import an ESRI Shapefile, creates ASCII and      */
/*               calls r.in.poly                                  */ 
/* (was written because of problems with polygons topology        */
/*  after v.in.shape, works only on areas and lines, add points   */
/*  should be simple)                                             */
/*  Note: hole may preced area in shape (I saw it)                */  
/*                                                                */
/* Radim Blazek, Radim.Blazek@dhv.cz, 6/2000                      */
/*                                                                */
/* rewritten from copy of v.in.shape by:                          */
/* Frank Warmerdam, warmerda@home.com				  */
/* Markus Neteler neteler@geog.uni-hannover.de                    */
/* David D Gray  <ddgray@armadce.demon.co.uk>                     */
/******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "shapefil.h"

static char *extract_base_name( );
static int  cmp(); 
static int  ring_dir(); 
static void get_startend ( );
static void get_catlab ( );

typedef struct {  
	int shape;    /* shape (record) number */
	int part;     /* part number */
	double area;  /* area */
} POLY;

int main( int   argc, char *argv[])
{
    int          i,j,cat, pstart, pend, plen, hstart, hend;
    SHPObject    *psShape;
    SHPHandle	 hShapeDB;
    DBFHandle    hDBF;
    double       adfMinBound[4], adfMaxBound[4];
    int          file_type;
    int		 iShape, iPart, nShapeType, nShapes;
    int		 cat_field, lab_field = -1;
    char         label[1024];
    POLY         *poly;   /* list of polygons */
    int          npoly=0; /* number of polygons */
    int          apoly=0; /* size of allocated poly */
    int          nhole=0; /* number of holes */
    int          nline=0; /* number of lines */
    char         *tmpfile;
    struct line_pnts points;    
    FILE         *tmp;
	struct GModule *module;
    struct {
	struct Option *input, *output, *attribute, *catlabel;
    } parm;
    char         infile[512], outfile[512];
    char         name[128], buf[1024];

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Read an ArcView Shapefile (polygons and lines).";

    /* define the different options */

    parm.input = G_define_option() ;
    parm.input->key        = "in";
    parm.input->type       = TYPE_STRING;
    parm.input->required   = YES;
    parm.input->description= "Name of .shp (or just .dbf) file to be imported";

    parm.output = G_define_option() ;
    parm.output->key        = "out";
    parm.output->type       = TYPE_STRING;
    parm.output->required   = YES;
    parm.output->description= "Name of output raster";

    parm.attribute = G_define_option() ;
    parm.attribute->key        = "cat";
    parm.attribute->type       = TYPE_STRING;
    parm.attribute->required   = NO;
    parm.attribute->description= "Name of attribute to use as category";
    parm.attribute->answer     =""; 
    
    parm.catlabel = G_define_option() ;
    parm.catlabel->key        = "lab";
    parm.catlabel->type       = TYPE_STRING;
    parm.catlabel->required   = NO;
    parm.catlabel->description= "Name of attribute to use as category label";
    parm.catlabel->answer     ="";

    /* get options and test their validity */
    if (G_parser(argc, argv))
	exit(-1);
    
    /* extract name of file and get names of dbf file */

    strcpy(infile, parm.input->answer);
    extract_base_name( name, infile );
    strcpy(outfile, parm.output->answer);

    /* Open input file and verify that's a good shapefile file */
    hShapeDB = SHPOpen( infile, "r" );
    if (hShapeDB == NULL)
    {
	sprintf (buf, "%s - shapefile not found, or wrong format.\n", infile);
	G_fatal_error (buf);
    }

    /* Establish the shape types and corresponding GRASS type */
    SHPGetInfo( hShapeDB, &nShapes, &nShapeType, adfMinBound, adfMaxBound );

    if( nShapeType == SHPT_MULTIPATCH ) {
      sprintf( buf, "Multipatch type not yet supported" );
      SHPClose( hShapeDB );
      G_fatal_error( buf );
    }

    if( nShapeType == SHPT_POLYGON || nShapeType == SHPT_POLYGONZ || nShapeType == SHPT_POLYGONM ) {
	file_type = SHPT_POLYGON;
    } else if ( nShapeType == SHPT_ARC || nShapeType == SHPT_ARCZ || nShapeType == SHPT_ARCM ) {
	file_type = SHPT_ARC;
    } else {
      sprintf( buf, "Map type not supported." );
      SHPClose( hShapeDB );
      G_fatal_error( buf );
    }

    /* temporary ASCII file */    
    tmpfile = G_tempfile ();
    if ((tmp = fopen (tmpfile, "w")) == NULL)
	G_fatal_error ("Can't open temp file for write\n");  

   /* ------- Identify the attribute (if any) to be extracted. ---------*/
    hDBF = NULL;
    if( strcmp(parm.attribute->answer,"") == 0 ) {
        cat_field = -1;
    } else {
        
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
	sprintf( buf, "No attribute `%s' found on %s.\n", parm.attribute->answer, strcat(name, ".shp") );
            
	DBFClose( hDBF );
	SHPClose( hShapeDB );

	G_fatal_error( buf );
      }

      if( DBFGetFieldInfo( hDBF, cat_field, NULL, NULL, NULL ) != FTInteger){
         cat_field = -1;
         printf( "Named attribute field is not integer value. Using record ID.\n" );	
      }    

      if(hDBF != NULL) DBFClose( hDBF );
    }



    /*	------------ Identify the category label to be used (if any) ------------ */
    if( strcmp(parm.catlabel->answer,"") == 0 ) {
        lab_field = -1;
    } else {
        
      hDBF = DBFOpen( infile, "r" );
      if( hDBF == NULL ) {
	  sprintf (buf, "%s - DBF not found, or wrong format.\n", infile);
	  G_fatal_error (buf);
      }	

      if( strcmp(parm.catlabel->answer, "") != 0 ) {

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

    hDBF = DBFOpen( infile, "r" );
    if( hDBF == NULL )
      {
	sprintf (buf, "%s - DBF not found, or wrong format.\n", infile);
	G_fatal_error (buf);
      }

    /* --------- write elements to tempfile -------- */
    if ( file_type == SHPT_ARC ){
	printf ("Writing lines in ASCII format to %s\n", tmpfile); 
	for( iShape = 0; iShape < hShapeDB->nRecords; iShape++ ) {
	    psShape = SHPReadObject( hShapeDB, iShape );
	    get_catlab ( hDBF, iShape, cat_field, lab_field, &cat, label );
	    for( iPart = 0; iPart < psShape->nParts; iPart++ ) {
		get_startend ( psShape, iPart, &pstart, &pend );
		/* write hole */
		fprintf( tmp, "L\n");
		for( j = pstart; j <= pend; j++ ) {
    		    fprintf( tmp, " %f %f\n", psShape->padfX[j], psShape->padfY[j]);
		}
		fprintf( tmp, "= %d %s\n", cat, label);
		nline++;
	    }	
	}
	printf ("%d lines written to ASCII file\n", nline); 
    } 
    else if ( file_type == SHPT_POLYGON ){
	/*--------  Read all polygons to poly array --------  */
	printf ("Reading polygon areas from SHP file (%d shapes)\n", hShapeDB->nRecords); 
	apoly = hShapeDB->nRecords;
	poly = ( POLY *) G_malloc( apoly  * sizeof( POLY ));
	G_begin_polygon_area_calculations(); 

	for( iShape = 0; iShape < hShapeDB->nRecords; iShape++ ) {
	    psShape = SHPReadObject( hShapeDB, iShape );
	    for( iPart = 0; iPart < psShape->nParts; iPart++ ) {
		if ( ring_dir(psShape, iPart)  == 1) { /* polygon not hole */
		    if ( npoly >= apoly){ 
			apoly += (int) 1.2 * hShapeDB->nRecords / iShape; /* ?? good size guess  */
			poly = ( POLY *) G_realloc( poly, apoly * sizeof( POLY ));
		    }
		    poly[npoly].shape = iShape;
		    poly[npoly].part = iPart;
		    get_startend ( psShape, iPart, &pstart, &pend );
		    plen = pend-pstart+1;     
		    poly[npoly].area = G_area_of_polygon(&psShape->padfX[pstart],&psShape->padfY[pstart], plen );  
		    npoly++;
		}
	    }		     
	    SHPDestroyObject( psShape );
	}
	printf ("%d areas read (holes not counted)\n", npoly); 

	/* --------- sort from smallest to largest area -------- */
	printf ("Sorting areas\n"); 
	qsort( poly, npoly, sizeof(POLY), cmp);    
    
	/* --------- write polygons to tmpfile from largest to smallest -------- */
	printf ("Writing areas and holes in ASCII format to %s\n", tmpfile); 
	for( i = npoly-1; i >= 0; i-- ) {
	    get_catlab ( hDBF, poly[i].shape, cat_field, lab_field, &cat, label );
	    psShape = SHPReadObject( hShapeDB, poly[i].shape );
	    get_startend ( psShape, poly[i].part, &pstart, &pend );
	    plen= pend - pstart + 1;
	
	    /* write polygon */ 
	    fprintf( tmp, "A\n");
	    for( j = pstart; j <= pend; j++ ) {
    		fprintf( tmp, " %f %f\n", psShape->padfX[j], psShape->padfY[j]);
	    }
	    fprintf( tmp, "= %d %s\n", cat, label);
	
	    /* write all holes inside this polygon */ 
	    for( iPart = 0; iPart < psShape->nParts; iPart++ ) {
		if ( iPart == poly[i].part) continue;
		if ( ring_dir(psShape, iPart) == -1 ) {  /* hole */
		    /* test if hole in polygon */
		    get_startend ( psShape, iPart, &hstart, &hend );
            	    points.x = &psShape->padfX[pstart];
            	    points.y = &psShape->padfY[pstart];
    		    points.n_points = plen;     
		    if ( dig_point_in_poly ( psShape->padfX[hstart], psShape->padfY[hstart], &points) > 0.0 ) { 
			/* write hole */
			fprintf( tmp, "A\n");
			for( j = hstart; j <= hend; j++ ) {
    			    fprintf( tmp, " %f %f\n", psShape->padfX[j], psShape->padfY[j]);
			}
			fprintf( tmp, "= %d\n", 0);
			nhole++;
		    }    
		}
	    }
	    SHPDestroyObject( psShape );
	} 
	printf ("%d areas and %d holes written to ASCII file\n", npoly, nhole); 
    }


    SHPClose( hShapeDB );
    fclose (tmp); 
    if( hDBF != NULL )  DBFClose( hDBF );

    /* run r.in.poly */
    snprintf(buf,1024,"r.in.poly input=%s output=%s", tmpfile, outfile);
    fprintf (stdout, "Running %s\n", buf); 
    fflush (stdout);
    system(buf);  
    unlink (tmpfile); 
    exit(0);
}

/* get category and label*/
static void get_catlab ( DBFHandle hDBF, int rec, int cat_field, int lab_field, int *cat, char *lab ) {
    DBFFieldType ftype;

    if ( cat_field > 0 )  *cat = DBFReadIntegerAttribute( hDBF, rec, cat_field ); 
    else *cat = rec + 1;
    
    if( lab_field >= 0 ) {
        ftype = DBFGetFieldInfo( hDBF, lab_field, NULL, NULL, NULL );  
        switch( ftype ) {
	    case FTString:  
	        strncpy (lab, DBFReadStringAttribute( hDBF, rec, lab_field ), 1023);	
	        break;
	    case FTInteger:  
	        sprintf (lab, "%d", DBFReadIntegerAttribute( hDBF, rec, lab_field ));	
	        break;
	    case FTDouble:  
	        sprintf (lab, "%f", DBFReadDoubleAttribute( hDBF, rec, lab_field ));	
	        break;
	    default:
	        lab[0]='\0';
	    break;
	}	    
    }
}

static void get_startend ( SHPObject *Shape, int Part, int *start, int *end ) {
    *start = Shape->panPartStart[Part];
    if( Part == Shape->nParts - 1 ) *end = Shape->nVertices - 1;
    else *end = Shape->panPartStart[Part+1] - 1; 
}

static char *extract_base_name( char *base_name, const char *in_name ) {
  /* Extract the basename of a fullname of the input file. ie. the answer to the option `in'  */

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


static int cmp ( const void *pa, const void *pb)
{
    POLY  *p1 = (POLY *) pa;
    POLY  *p2 = (POLY *) pb;
	 
    if( p1->area < p2->area)
        return -1;
    if( p1->area > p2->area)
        return 1;
    return 0;
}  

/* **************************************************************************
 * Test Polygon for CW / CCW  ( R+ / R- )
 * return 1  for R+
 * return -1 for R-
 * return 0  for error
 *    
 * ??? Is it realy OK - it looks too simple but seems to work OK
 * **************************************************************************/
static int ring_dir ( SHPObject *psCShape, int Ring ) {
   int          i, start, end, l,r,b,t;
   double       *x, *y;
 
   x = psCShape->padfX;
   y = psCShape->padfY;
 
   if ( Ring >= psCShape->nParts ) return ( 0 );
 
   start=psCShape->panPartStart[Ring]; 
   if ( Ring >= psCShape->nParts -1 ) end = psCShape->nVertices;
   else                               end = psCShape->panPartStart[Ring + 1];

   l =  r =  b =  t = start;
   for ( i = start+1; i < end; i++ ) {
     if ( x[i] < x[l] ) l=i;
     if ( x[i] > x[r] ) r=i;
     if ( y[i] < y[b] ) b=i;
     if ( y[i] > y[t] ) t=i;
   }
 
    i=0;
    if (  b <= l ) i++;
    if (  l <= t ) i++;
    if (  t <= r ) i++;
    if (  r <= b ) i++;
 
    if ( i == 3 ) return (1);
    if ( i == 1 || i == 2 ) return (-1);
    return (0);
}      
