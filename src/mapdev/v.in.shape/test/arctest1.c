/******************************************************************************
 * arctest1.c
 * Test program for generating lines and labels from a shapefile
 * using the shp2dig facilities and the Shapefile library
 * ( F. Warmerdam).

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 5th. Mar. 2000
 * Last updated 5th. Mar. 2000
 *

* This is free software.  
* You can redistribute it and/or modify it under the terms of 
* the GNU General Public License as published by the Free Software
* Foundation; either version 2 of the License, or (at your option)
* any later version.

* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.

 ******************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "dbutils.h"
#include "shapefil.h"
#include "shp2dig.h"
#include "gbtree.h"
#include <stdio.h>

int main( int argc, char **argv ) {

  int i0, j0, k0;

  FILE *txt1, *lab1, *pol1;
  int findx;
  int lcount, lcount1, icount;
  int fieldCount;
  char txt0[40], pol0[40], lab0[40], prefix[40];
  char *chr0;
  char *tmpShape ="/tmp/tmpshape.dbm";
  char *tmpString;
  int word_switch;

  int (*btrkeycmp)(char *, char *);

  SHPHandle shp0;
  DBFHandle dbf0;

  lineList *lined0;
  fieldDescript *fieldd0;
  segmentList *segl;

  BTREE *btr;

  if( argc == 1 ) {
    printf( "\nUsage: arctest1 <filename-prefix>, [ index of dbf-field ]\n" );
    return -1;
  }
  else if( argc > 3 ) {
    printf( "Too many arguments.\nUsage: arctest1 <filename-prefix>, [ index of dbf-field ]\n" );
    return -1;
  }
  else if( argc == 2 ) {
    findx = -1;
  }
  else {
    if( !(findx = atoi( argv[2]) ) )  {
      printf( "\nCould not assign dbf category - using running index instead\n" );
      findx = -1;
    }
  }

  /* Extract prefix of shp file - assume suffix .shp */

  strcpy( prefix, argv[1] );

  /* Open shape file */

  if( (shp0 = SHPOpen( argv[1], "r" )) == NULL ) {
    printf( "\nCould not open shape file\n" );
    return -1;
  }

  if( (dbf0 = DBFOpen( argv[1], "r" )) == NULL ) {
    printf( "\nCould not open dbf file\n" );
    return -1;
  }
  
  /* bankinfo = (BTREEINFO *)malloc( sizeof(BTREEINFO) );
  bankinfo->flags = 0;
  bankinfo->cachesize = 1000000;
  bankinfo->minkeypage = 8;
  bankinfo->psize = 0; 
  bankinfo->compare = NULL; 
  bankinfo->prefix = NULL;   
  bankinfo->lorder = 0;  
 */

  btrkeycmp = btree_compare;

  btr = (BTREE *)malloc(sizeof (BTREE) );
  
  if( !btree_create( btr, btrkeycmp, 200 )) {
    fprintf( stderr, "Cannot create database. Aborting" );
    exit -1;
  }
  
  /* Process the shape file and build descriptors */

  lined0 = (lineList *)malloc( sizeof( lineList ) );
  fieldd0 = (fieldDescript *)malloc( (DBFGetFieldCount( dbf0 ) + 4 ) * 
				     sizeof( fieldDescript ));
  segl = (segmentList *)malloc( sizeof (segmentList) );

  linedCreate( lined0, shp0, dbf0, fieldd0, btr, &fieldCount );
  vbase2segd( segl, btr );

  lcount = lined0->totalValidParts;

  /* Extract data and write arc files */
  strcat( strcpy( txt0, prefix ), ".txt" );
  strcat( strcpy( lab0, prefix ), ".lab" );
  strcat( strcpy( pol0, prefix ), ".pol" );

  if( (txt1 = fopen( txt0, "w" )) == NULL ) {
    printf( "\nCould not open data text file for writing\n" );
    return -1;
  }

  if( (lab1 = fopen( lab0, "w" )) == NULL ) {
    printf( "\nCould not open label file for writing\n" );
    return -1;
  }

  if( (pol1 = fopen( pol0, "w" )) == NULL ) {
    printf( "\nCould not open line data file for writing\n" );
    return -1;
  }


  /* Write label points file */

  icount = 0;
  for( i0 = 0; i0 < lined0->numLines; ++i0 ) {
    for( j0 = 0; j0 < lined0->lines[i0].numParts; ++j0 ) {
      if( !lined0->lines[i0].parts[j0].duff ) {
	fprintf( lab1, "%d %.6f %.6f\n", ++icount,
		 lined0->lines[i0].parts[j0].centroid->xcentroid,
		 lined0->lines[i0].parts[j0].centroid->ycentroid );

      }
    }

  }

  fprintf( lab1, "END\n" );

  /* close label points file */
  fclose( lab1 );

  /* Write lines file */

  icount = 0;
  for( i0 = 0; i0 < segl->numSegments; ++i0 ) {
    if( segl->segments[i0].duff ) continue;

    fprintf( pol1, "  %6d\n", ++icount );
    for( j0 = 0; j0 < segl->segments[i0].numVertices; ++j0 ) {
      fprintf( pol1, "  %15.6f  %15.6f\n", 
	       segl->segments[i0].vertices[j0]->xPosn,
	       segl->segments[i0].vertices[j0]->yPosn );
    }
    fprintf( pol1, "END\n" );

  }
  fprintf( pol1, "END\n" );

  /* close lines file */
  fclose( pol1 );

  /* Now the categories */

  lcount1 = 0;
  for( i0 = 0; i0 < lcount; ++i0 ) {
    /* loop through records */
    fprintf( txt1, "%-6d ", ++lcount1 );
    
    for( j0 = 0; j0 < fieldCount + 4; ++j0 ) {
      /* and through fields */
      assert( fieldd0[j0].nRec == lcount );
      
      switch( fieldd0[j0].fldType ) {
      case 0:
	word_switch = 0;
	tmpString = (char *)malloc( fieldd0[j0].fldSize + 1 );
	snprintf( tmpString, fieldd0[j0].fldSize, "%s", fieldd0[j0].fldRecs[i0].stringField );	
	for( k0 = 0; k0 < fieldd0[j0].fldSize; ++k0 ) {
	  if( tmpString[k0] == '\0' ) word_switch = 1;
	  if( word_switch ) tmpString[k0] = ' ';
	  else {
	    if( tmpString[k0] == ' ' ) tmpString[k0] = '_';
	  }
	}
	tmpString[fieldd0[j0].fldSize] = '\0';
	fprintf( txt1, "%s ", tmpString );
	free( tmpString );
	break;
      case 1:
	word_switch = 0;
	tmpString = (char *)malloc( (fieldd0[j0].fldSize + 1 ) * sizeof( char ) );
	snprintf( tmpString, fieldd0[j0].fldSize, "%d", fieldd0[j0].fldRecs[i0].intField );
	for( k0 = 0; k0 < fieldd0[j0].fldSize; ++k0 ) {
	  if( tmpString[k0] == '\0' ) word_switch = 1;
	  if( word_switch ) tmpString[k0] = ' ';
	  else {
	    if( tmpString[k0] == ' ' ) tmpString[k0] = '_';
	  }
	}
	tmpString[fieldd0[j0].fldSize] = '\0';
	fprintf( txt1, "%s ", tmpString );
	free( tmpString );
	break;
      case 2:
	word_switch = 0;
	tmpString = (char *)malloc( (fieldd0[j0].fldSize + 1 ) * sizeof( char ) );
	snprintf( tmpString, fieldd0[j0].fldSize, "%.6f", fieldd0[j0].fldRecs[i0].doubleField );
	for( k0 = 0; k0 < fieldd0[j0].fldSize; ++k0 ) {
	  if( tmpString[k0] == '\0' ) word_switch = 1;
	  if( word_switch ) tmpString[k0] = ' ';
	  else {
	    if( tmpString[k0] == ' ' ) tmpString[k0] = '_';
	  }
	}
	tmpString[fieldd0[j0].fldSize] = '\0';
	fprintf( txt1, "%s ", tmpString );
	free( tmpString );
	break;
      }
    }

    fprintf( txt1, "\n" );
  }

  /* and clode this */
  fclose( txt1 );

  SHPClose( shp0 );
  linedDispose( lined0, fieldd0, fieldCount );
  btree_free( btr );
  /* if( unlink( tmpShape ) ) fprintf( stderr, "Error deleting temp database" ); */
  /* free(keyBank); */
  /* free(dataBank); */

  return 0;
  
} /* Main */
