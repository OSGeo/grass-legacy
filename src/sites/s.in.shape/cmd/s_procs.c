/******************************************************************************
 * s_procs.c
 * functions for handling processing of internal database
 * relating to topological structures held in memory

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>`
 * 12th. Apr. 2000
 * Last updated 31st. May. 2000
 *

* This file is part of GRASS GIS. It is free software. You can 
* redistribute it and/or modify it under the terms of 
* the GNU General Public License as published by the Free Software
* Foundation; either version 2 of the License, or (at your option)
* any later version.

* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.

 ******************************************************************************/

#include "s_procs.h"

int load_mpshape_data( SHPHandle shp0, aggregateList *ag0, 
		     DBFHandle dbf0, fieldDescriptor *fd0, int *nfld ) {

  /* Take the information from a multipoint shape object and store it in
     appropriate intermediate structures.
  */


  /* local variables */
  int i0, j0, k0; /* loop */
  int shpType;
  int nShapes;
  int pntCount;
  SHPObject *shpobj0;
  int recs_count = 500, recs_incr = 200;
  int do_incr = 0;
  int valid;

  FILE *f1;

  char fieldName[15];
  int fieldLength, fieldDecimals;
  DBFFieldType fieldType;



  /* Initialise shape input */

  shpType = shp0->nShapeType;
  nShapes = shp0->nRecords;

  /* Exit if input shapefile is not valid */

  valid = ( shpType == SHPT_MULTIPOINT || shpType == SHPT_MULTIPOINTM || shpType == SHPT_MULTIPOINTZ ||
	    shpType == SHPT_POINT || shpType == SHPT_POINTZ || shpType == SHPT_POINTM );
  if(!valid) return(1);

  /* Initialise the field descriptors */

  fd0[0].duff = 0;
  fd0[0].fldSize = 16;
  fd0[0].fldDec = 6;
  fd0[0].nRec = 0;
  fd0[0].fldType = FTDouble;
  strcpy(fd0[0].fldName, "__X_POSN");
  fd0[0].fldRecs = (dbfRecElement *)malloc( recs_count * sizeof(dbfRecElement) );
  
  fd0[1].duff = 0;
  fd0[1].fldSize = 16;
  fd0[1].fldDec = 6;
  fd0[1].nRec = 0;
  fd0[1].fldType = FTDouble;
  strcpy(fd0[1].fldName, "__Y_POSN");
  fd0[1].fldRecs = (dbfRecElement *)malloc( recs_count * sizeof(dbfRecElement) );
  
  fd0[2].duff = 0;
  fd0[2].fldSize = 16;
  fd0[2].fldDec = 6;
  fd0[2].nRec = 0;
  fd0[2].fldType = FTDouble;
  strcpy(fd0[2].fldName, "__Z_POSN");
  fd0[2].fldRecs = (dbfRecElement *)malloc( recs_count * sizeof(dbfRecElement) );

  fd0[3].duff = 0;
  fd0[3].fldSize = 10;
  fd0[3].fldDec = 0;
  fd0[3].nRec = 0;
  fd0[3].fldType = FTInteger;
  strcpy(fd0[3].fldName, "__REC_ID");
  fd0[3].fldRecs = (dbfRecElement *)malloc( recs_count * sizeof(dbfRecElement) );

  fd0[4].duff = 0;
  fd0[4].fldSize = 10;
  fd0[4].fldDec = 0;
  fd0[4].nRec = 0;
  fd0[4].fldType = FTInteger;
  strcpy(fd0[4].fldName, "__ORIG_ID");
  fd0[4].fldRecs = (dbfRecElement *)malloc( recs_count * sizeof(dbfRecElement) );

  fd0[5].duff = 0;
  fd0[5].fldSize = 20;
  fd0[5].fldDec = 6;
  fd0[5].nRec = 0;
  fd0[5].fldType = FTDouble;
  strcpy(fd0[5].fldName, "__M_VAL");
  fd0[5].fldRecs = (dbfRecElement *)malloc( recs_count * sizeof(dbfRecElement) );


  for( k0 = 0; k0 < *nfld; ++k0 ){  
    fieldType = DBFGetFieldInfo( dbf0, k0, fieldName, &fieldLength, &fieldDecimals );
    fd0[k0+6].duff = 0;
    fd0[k0+6].fldSize = fieldLength;
    fd0[k0+6].fldDec = fieldDecimals;
    fd0[k0+6].nRec = 0;
    fd0[k0+6].fldType = fieldType;
    strcpy(fd0[k0+6].fldName, fieldName);
    fd0[k0+6].fldRecs = (dbfRecElement *)malloc( recs_count * sizeof(dbfRecElement) );
  }


  /* Allocate aggregate descriptors */

  ag0->numaggs = shp0->nRecords;
  ag0->aggs = (aggregateDescriptor *)malloc( ag0->numaggs * sizeof(aggregateDescriptor));
  pntCount = 0;


  /* Loop through records and extract data */

  for( i0 = 0; i0 < nShapes; ++i0 ) {

    /* Read in the shape object and corresponding record */

    shpobj0 = SHPReadObject( shp0, i0 );

    for( j0 = 0; j0 < shpobj0->nVertices; ++j0 ) {

      if( pntCount > recs_count ) {
	do_incr = 1;
	recs_count += recs_incr;
      }

      /* Field 0: x coordinate */
      if( do_incr ) {
	fd0[0].fldRecs = (dbfRecElement *)realloc(fd0[0].fldRecs, recs_count *
						  sizeof(dbfRecElement) );
      }
      fd0[0].fldRecs[pntCount].doubleField = shpobj0->padfX[j0];   
      fd0[0].nRec++;

      /* Field 1: y coordinate */
      if( do_incr ) {
	fd0[1].fldRecs = (dbfRecElement *)realloc(fd0[1].fldRecs, recs_count *
						  sizeof(dbfRecElement) );
      }
      fd0[1].fldRecs[pntCount].doubleField = shpobj0->padfY[j0];
      fd0[1].nRec++;

      /* Field 2: z coordinate */
      if( do_incr ) {      
	fd0[2].fldRecs = (dbfRecElement *)realloc(fd0[2].fldRecs, recs_count *
						  sizeof(dbfRecElement) );
      }
      if( shpType == SHPT_MULTIPOINTZ || shpType == SHPT_POINTZ )
	fd0[2].fldRecs[pntCount].doubleField = shpobj0->padfX[j0];
      else
	fd0[2].fldRecs[pntCount].doubleField = 0.0;

      fd0[2].nRec++;

      /* Field 3: ID number */
      if( do_incr ) {      
	fd0[3].fldRecs = (dbfRecElement *)realloc(fd0[3].fldRecs, recs_count *
						  sizeof(dbfRecElement) );
      }
      fd0[3].fldRecs[pntCount].intField = pntCount + 1;
      fd0[3].nRec++;

      /* Field 4: Aggregate ID number */
      if( do_incr ) {      
	fd0[4].fldRecs = (dbfRecElement *)realloc(fd0[4].fldRecs, recs_count *
						  sizeof(dbfRecElement) );
      }
      fd0[4].fldRecs[pntCount].intField = i0 + 1;
      fd0[4].nRec++;


      /* Field 5: Measure value */
      if( do_incr ) {      
	fd0[5].fldRecs = (dbfRecElement *)realloc(fd0[5].fldRecs, recs_count *
						  sizeof(dbfRecElement) );
      }
      if( shpType == SHPT_MULTIPOINTZ || shpType == SHPT_MULTIPOINTM ||
	  shpType == SHPT_POINTZ || shpType == SHPT_POINTM )
	fd0[5].fldRecs[pntCount].doubleField = shpobj0->padfM[j0];
      else
	fd0[5].fldRecs[pntCount].doubleField = 0.0;
      
      fd0[5].nRec++;



      /* The real fields */

      for( k0 = 0; k0 < *nfld ; ++k0 ) {

	if( do_incr ) {      
	  fd0[k0+6].fldRecs = (dbfRecElement *)realloc(fd0[k0+6].fldRecs, recs_count *
						    sizeof(dbfRecElement) );
	}

	fd0[k0+6].nRec++;

	switch( fd0[k0+6].fldType ) {

	case FTString:
	  {
	    fd0[k0+6].fldRecs[pntCount].stringField = (char *)malloc( MAX_DBF_STRING_FIELD_SIZE + 1 );
	    strncpy( fd0[k0+6].fldRecs[pntCount].stringField, DBFReadStringAttribute( dbf0, i0, k0 ),
		     MAX_DBF_STRING_FIELD_SIZE );
	    break;
	  }
	case FTInteger:
	  {
	    fd0[k0+6].fldRecs[pntCount].intField = DBFReadIntegerAttribute( dbf0, i0, k0 );
	    break;
	  }
	case FTDouble:
	  {
	    fd0[k0+6].fldRecs[pntCount].doubleField = DBFReadDoubleAttribute( dbf0, i0, k0 );
	    break;
	  }
	default:
	  {
	    fd0[k0+6].fldRecs[pntCount].intField = 0;
	  }

	}
	
      }

      pntCount++;

    }
  }

  for( k0 = 0; k0 < *nfld ; ++k0 ) {
    
    /* Reset unrecognised field types to integer type */
    if(fd0[k0+6].fldType > 2) fd0[k0+6].fldType = 1;
  }

  return(0);

}


void dbf_fields_reorder( fieldDescriptor *fd0, const int *nfld ) {

  /* Rearrange the sequence of fields to #..%..@..  */
}

