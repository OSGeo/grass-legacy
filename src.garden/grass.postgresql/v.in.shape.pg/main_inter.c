/* @(#)main.c	1.0   04/90 */
/*  Written by  Dave Johnson
**  DBA Systems, Inc.
**
**  modified by R.L.Glenn
**  USDA, Soil COnservation Service, CGIS Staff
*/

/* main.c
 *
 * 
 *
 * NOTES: 
 *	.........
 * 2) Program v.support must be run on the resulting GRASS
 *    vector file and then some cleaning-up may have to be
 *    done using program v.digit.
 *
 *
 * Dave Johnson
 * DBA Systems, Inc.
 * 10560 Arrowhead Drive
 * Fairfax, Virginia 22030
 *
 */
 /** This is free software.  
* You can redistribute it and/or modify it under the terms of 
* the GNU General Public License as published by the Free Software
* Foundation; either version 2 of the License, or (at your option)
* any later version.

* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.

 ******************************************************************************/
 /*	Postgres dump from DBF 
 *	and no need to have cats consequently (so cats == id's) 
 *	Alex Shevlakov sixote@yahoo.com 02/2000 
 */
/*	shp2dig lib @Copyright David D.Gray <ddgray@armadce.demon.co.uk>*/
/*									*/
/*	Shapelib Copyright (c) 1998, Frank Warmerdam	*/

#include  <stdio.h>
#include  <string.h>
#include  "gis.h"
#include  "Vect.h"
#include "v_in_arc.inter.h"
#include "shapefil.h"
#include "shp2dig.h"

int main (int argc, char **argv)
{
	char	tmpbuf[80]="";
	int	neatline=0;
	int	errflag;
	char	name[128]="";
	char	*mapset ;
	char 	errmsg[200]="";
	char	cov_type[80]="polygon";          /* coverage type */
	char	*lines_filename;
	char	*pts_filename;
	char	*s_lines_filename;
	char	*n_lines_filename;
	FILE	*lines_file;
	FILE	*pts_file;
	FILE	*s_lines_file;
	FILE	*n_lines_file;
	FILE	*txt_file;
	
	char	dig_filename[80]="";     /* GRASS vector (dig) file */
	char	dig_filepath[80]="" ;
	FILE	*dig_file;
	char	atts_filename[80]="" ;     /* GRASS vector (dig_atts) file */
	char	atts_filepath[80]="" ;
	FILE	*atts_file;

	struct Map_info  VectMap;
	
	int	rattle, pgdump;
   	char	*infile;
	int	try_again = 1;
	


    	struct {
	struct Option *input;
    	} parm;
	

	
  int i0, j0, k0;

  FILE *txt1, *lab1, *pol1;
  int findx;
  int lcount, lcount1, icount;
  int fieldCount;
  char txt0[40], pol0[40], lab0[40], prefix[40];
  char *chr0;
  char *tmpString;
  int word_switch;

  SHPHandle shp0;
  DBFHandle dbf0;

  lineList *lined0;
  fieldDescript *fieldd0;
	

	
  double	adfMinBound[4], adfMaxBound[4];
  int		nShapeType, nShapes;
  
  char buf[256];
	
	
	G_gisinit("ArcView -  import from shapefile");

	if (argc != 2)
	{                 /* get name for grass vector file from user */
		mapset = G_ask_new( "GRASS vector file:",dig_filename,"dig",
		    "binary vector") ;
		if ( ! mapset) exit(0) ;
	}
	else 
		strcpy(dig_filename,argv[1]);
		
		
	/* define the different options */
	
    	parm.input = G_define_option() ;
    	parm.input->key        = "input";
    	parm.input->type       = TYPE_STRING;
    	parm.input->required   = YES;
    	parm.input->description= "Name of .shp file to be imported";

	fprintf (stdout,"\n Postgres support:\n");
		fprintf (stdout,"Enter \"yes\" if you want to import data to Postgres\n");
		fprintf (stdout,"Hit RETURN if you don't\n");
		fprintf (stdout,"> ");
		fgets(tmpbuf,80,stdin);
/*************************************************************************/
   /*****!!!!!! hier eine Zeile eingefuegt,ebenso bei allen anderen fgets *****/

              tmpbuf[strlen(tmpbuf)-1]='\0';
/********************************************************************/
                

		if (!strlen(tmpbuf)) {
			pgdump = 0;
		}
		else if (!strncmp(tmpbuf,"y",1)) 
			pgdump=1;
		else
			pgdump = 0;
			
	/*Choose mode of dump*/
   if (pgdump) {		
		tmpbuf[0] = '\0';	
	fprintf (stdout,"\n Postgres dump mode:\n");
		fprintf (stdout,"Enter \"user\" if you are not Postgres superuser\n");
		fprintf (stdout,"Hit RETURN if you are\n");
		fprintf (stdout,"> ");
		fgets(tmpbuf,80,stdin);
/*************************************************************************/
   /*****!!!!!! hier eine Zeile eingefuegt,ebenso bei allen anderen fgets *****/

              tmpbuf[strlen(tmpbuf)-1]='\0';
/********************************************************************/
		
		if (!strlen(tmpbuf)) {
			rattle = 0;
		}
		else if (!strncmp(tmpbuf,"user",4)) 
			rattle = 1;
		else
			rattle = 0;
   }
    /* get options and test their validity */

    if (G_parser(argc, argv))
	exit(-1);
	
	infile = parm.input->answer;

	


  /* Open shape file */

  if( (shp0 = SHPOpen( infile, "r" )) == NULL ) {
    printf( "\nCould not open shape file\n" );
    return -1;
  }

  
    /* Establish the shape types and corresponding GRASS type */
    
    SHPGetInfo( shp0, &nShapes, &nShapeType, adfMinBound, adfMaxBound );

    if( nShapeType == SHPT_MULTIPATCH ) {
  
      sprintf( buf, "Multipatch type not yet supported" );
      SHPClose( shp0 );
      G_fatal_error( buf );

    }
    
    switch (nShapeType) {
      case SHPT_POINT:
      case SHPT_MULTIPOINT:
      case SHPT_POINTZ:
      case SHPT_MULTIPOINTZ:
      case SHPT_POINTM:
      case SHPT_MULTIPOINTM:

	fprintf(stdout,"\nType of shape file found: Point\n");
	sprintf( buf, "Point type not supported in this module" );
      
      SHPClose( shp0 );
      G_fatal_error( buf );
        break;

      case SHPT_ARC:
      case SHPT_ARCZ:
      case SHPT_ARCM:
        cov_type[0] = '\0';
        strcat(cov_type,"line");
	fprintf(stdout,"\nType of shape file found: arc\nConverting to GRASS vector lines\n");
        break;
      case SHPT_POLYGON:
      case SHPT_POLYGONZ:
      case SHPT_POLYGONM:
        strcat(cov_type,"polygon");
	
	
       	fprintf(stdout,"\nType of shape file found: area\n");
	while (try_again) {
	tmpbuf[0] = '\0';
	fprintf (stdout,"\n Coverage type:\n");
		fprintf (stdout,"Enter \"line\" if you want to import lines only\n");
		fprintf (stdout,"Hit RETURN to import as areas.\n");
		fprintf (stdout,"> ");
		fgets(tmpbuf,80,stdin);
/*************************************************************************/
   /*****!!!!!! hier eine Zeile eingefuegt,ebenso bei allen anderen fgets *****/

              tmpbuf[strlen(tmpbuf)-1]='\0';
/********************************************************************/
                
		cov_type[0] = '\0';
		
		


		if (!strlen(tmpbuf)) {
			strcat(cov_type,"polygon");
			try_again = 0;
		}
		else {
			if (strcmp(tmpbuf,"line") == 0) {
				strcat(cov_type,"line");
				fprintf(stdout,"\nOK, converting to lines\n");
				try_again = 0;
			}
			else if (strncmp(tmpbuf,"poly",4) == 0) {
				strcat(cov_type,"polygon");
				fprintf(stdout,"\nOK, converting to polygons\n");
				try_again = 0;
			}
			else if (strcmp(tmpbuf,"area") == 0) {
				strcat(cov_type,"polygon");
				fprintf(stdout,"\nOK, converting to polygons\n");
				try_again = 0;
			}
			else
				fprintf(stdout,"\nYou've just typed-%s\nSorry, let's try again\n",tmpbuf);
		}
	}			

        break;
    }
    
    if( (dbf0 = DBFOpen( infile, "r" )) == NULL ) {
    printf( "\nCould not open dbf file\n" );
    return -1;
  }
  
  	if (pgdump) 
		PgDumpFromDBF(infile, rattle);

  /* Process the shape file and build descriptors */

  lined0 = (lineList *)malloc( sizeof( lineList ) );
  fieldd0 = (fieldDescript *)malloc( (DBFGetFieldCount( dbf0 ) + 4 ) * 
				     sizeof( fieldDescript ));

  linedCreate( lined0, shp0, dbf0, fieldd0, &fieldCount );

  lcount = lined0->totalValidParts;
			/* LINES FILENAME */
			

				lines_filename=G_tempfile();
				if ((lines_file=fopen(lines_filename,"w+")) == NULL) {
					G_fatal_error("Can't open tmp file for writing\n");
					exit(-1);
				}

			/* LABEL-POINTS FILENAME */
			

				pts_filename=G_tempfile();
				if ((pts_file=fopen(pts_filename,"w+")) == NULL) {
					G_fatal_error("Can't open tmp file for writing\n");
					exit(-1);
				}
	
 /* Write data files */

  icount = 0;
  for( i0 = 0; i0 < lined0->numLines; ++i0 ) {
    for( j0 = 0; j0 < lined0->lines[i0].numParts; ++j0 ) {
      if( !lined0->lines[i0].parts[j0].duff ) {
	fprintf( lines_file, "%5d\n", ++icount );
	fprintf( pts_file, "%d %.6f %.6f\n", icount,
		 lined0->lines[i0].parts[j0].centroid->xcentroid,
		 lined0->lines[i0].parts[j0].centroid->ycentroid );

	for( k0 = 0; k0 < lined0->lines[i0].parts[j0].numPoints; ++k0 ) {
	  fprintf( lines_file, "  %.6f  %.6f\n", 
		   lined0->lines[i0].parts[j0].linepnts[k0].xPosn,
		   lined0->lines[i0].parts[j0].linepnts[k0].yPosn );
	}
	fprintf( lines_file, "END\n" );
      }
    }
  }

  fprintf( lines_file, "END\n" );
  fprintf( pts_file, "END\n" );


	if (0 > Vect_open_new (&VectMap, dig_filename))
	{
		sprintf(errmsg, "Can't open <%s>\n", dig_filename) ;
		G_fatal_error (errmsg);
	}

	/* open a new GRASS dig_atts file */
	G__make_mapset_element("dig_att") ;
	G__file_name(atts_filepath,"dig_att",dig_filename,G_mapset()) ;
	if ((atts_file=fopen(atts_filepath,"w"))==NULL)
	{
		fprintf (stdout,"Can't open dig_atts <%s>\n",atts_filepath);
		exit(-1);
	}

	s_lines_filename=G_tempfile();
				if ((s_lines_file=fopen(s_lines_filename,"w+")) == NULL) {
					G_fatal_error("Can't open tmp file for writing\n");
					exit(-1);
				}
	n_lines_filename=G_tempfile();
				if ((n_lines_file=fopen(n_lines_filename,"w+")) == NULL) {
					G_fatal_error("Can't open tmp file for writing\n");
					exit(-1);
				}
	fprintf(stderr,"\nRemoving duplicate arcs:\n");
	rewind(lines_file);
	leave_single_lines(lines_file,s_lines_file);
	fprintf(stderr,"\nDone\n");
	rewind(s_lines_file);
	fclose(lines_file);
	fprintf(stderr,"\nShortening arcs to nodes:\n");
	split_arcs_to_nodes(s_lines_file,n_lines_file);
	fprintf(stderr,"\nDone\n");
	fclose(s_lines_file);
	rewind(n_lines_file);
	rewind(pts_file);
/************************
here we hack -A.Sh.*/
			txt_file=n_lines_file;
			
/***********************/
	if ((errflag=BuildDig(cov_type,neatline,n_lines_file,pts_file,txt_file,
	    atts_file,&VectMap, dig_filename))<0)
	{
		switch (errflag)
		{
		case -1:
			G_fatal_error("Error reading LINES"); 
			exit(-1);
		case -2:
			G_fatal_error("Error reading"); 
			exit(-1);
		case -3:
			G_fatal_error("Error reading LABEL-POINTS"); 
			exit(-1);
		case -4:
			G_fatal_error("Error reading LINES"); 
			exit(-1);
		case -5:
			G_fatal_error("Bad coverage type"); 
			exit(-1);
		default: 
			break;
		}
	}
	
	
	Vect_close (&VectMap);

	fclose(pts_file);
	fprintf (stderr, "\n\nv.in.shape.pg finished.\n");
	fprintf(stderr, "\n\nBefore using <%s> in 'v.digit' :\nrun v.support to build topology.\n",
	    dig_filename) ;

	exit(0) ;
}
