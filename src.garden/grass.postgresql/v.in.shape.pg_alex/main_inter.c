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
	
	int	no_rattle;
   	char	*infile;


    	struct {
	struct Option *input, *dumpmode;
    	} parm;
	
	/*ddgray's defines*/
	
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

	parm.dumpmode = G_define_option() ;
    	parm.dumpmode->key        = "dumpmode";
   	parm.dumpmode->type       = TYPE_STRING;
    	parm.dumpmode->required   = NO;
    	parm.dumpmode->description= "Admin/normal user dump mode (Default = Postgres super-user)";
    

    /* get options and test their validity */

    if (G_parser(argc, argv))
	exit(-1);
	
	infile = parm.input->answer;
    	no_rattle = (int) parm.dumpmode->answer;
	
	PgDumpFromDBF(infile, no_rattle);


  /* Open shape file */

  if( (shp0 = SHPOpen( infile, "r" )) == NULL ) {
    printf( "\nCould not open shape file\n" );
    return -1;
  }

  if( (dbf0 = DBFOpen( infile, "r" )) == NULL ) {
    printf( "\nCould not open dbf file\n" );
    return -1;
  }

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
//	fclose(lines_file);
	fclose(pts_file);
	fprintf (stderr, "\n\nv.in.shape.pg finished.\n");
	fprintf(stderr, "\n\nBefore using <%s> in 'v.digit' :\nrun v.support to build topology.\n",
	    dig_filename) ;

	exit(0) ;
}
