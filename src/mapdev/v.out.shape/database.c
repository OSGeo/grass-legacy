/******************************************************************************
 * database.c [v.out.shape2]
 * Routines to transfer database info from dig files to dbf files
 * associated with shapefile export.

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>`
 * 1st. Jul. 2000
 * Last updated 2nd. Jul. 2000
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

#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "shapefil.h"
#include "local_structs.h"

int load_dbf_from_fieldD( DBFHandle hDBF, fieldDescriptor *hFD, const int nf ) {

  /* Transfer information contained in a field descriptor to a dbf file
     on disk
  */

  /* Local */
  int nrec;

  /* Loop */
  int i, j;

  /* Get the number of records */

  nrec = hFD[0].nRec;


  /* Loop through all the records and write out the fields */

  for( i = 0; i < nrec; ++i ) {

    /* Loop through each field */
    for( j = 0; j < nf; ++j ) {

      switch( hFD[j].fldType ) {

      case FTString:
	{
	  if( !DBFWriteStringAttribute(hDBF, i, j, hFD[j].fldRecs[i].stringField) ) {
	    fprintf(stderr, "Unable to write string attribute to DBF. \n" );
	    return -1;
	  }
	  break;
	}
      case FTInteger:
	{
	  if( !DBFWriteIntegerAttribute(hDBF, i, j, hFD[j].fldRecs[i].intField) ) {
	    fprintf(stderr, "Unable to write integer attribute to DBF. \n" );
	    return -1;
	  }
	  break;
	}
      case FTDouble:
	{
	  if( !DBFWriteDoubleAttribute(hDBF, i, j, hFD[j].fldRecs[i].doubleField) ) {
	    fprintf(stderr, "Unable to write floating point attribute to DBF. \n" );
	    return -1;
	  }
	  break;
	}
      }
    }
  }

  return 0;
}
