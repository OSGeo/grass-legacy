/******************************************************************************
 * attval.c [v.in.mif]
 * Extract an attribute value (integer) from a text field
 * in a given line.

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 15th. Oct. 2000
 * Last updated 15th. Oct. 2000
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
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "local_structs.h"

extern d_type del0;
extern int has_mid;

/* Obtain the attribute value associated with a field in a given line */

int get_att_field_val(const d_type *fields, const int line_num,
		      const int field_num, FILE *logfp) {

  /* local */

  int j; /* loop */

  field_data fd1;
  d_type tmp1;
  char delim;
  char delims[2];
  int nfields = 0, *strflags;

  int catval;
  int len;

  delim = del0[1];
  delims[0] = delim;
  delims[1] = '\0';

  if(has_mid) {
    parse_all_fields(fields, &fd1, delims, line_num, &nfields);
    strflags = (int *)G_malloc(nfields * sizeof(int) );

    for( j = 0; j < nfields; j++ ) {
      if( fd1[j][0] == '\"' ) {
	len = strlen(fd1[j]);
	fd1[j][len - 1] = '\0';
	strcpy(tmp1, (&fd1[j][0] + 1));
	strcpy(fd1[j], tmp1);
	strflags[j] = 1;
      }
      else {  
	strflags[j] = 0;
	  
      }
    }
    
    for( j = 0; j < nfields; j++ ) {

      if(strflags[j]) {
	fprintf(logfp, "STRING: %s\n", fd1[j]);
	if( j == field_num ) catval = 0;
      }

      else {
	fprintf(logfp, "NUMERIC: %s\n", fd1[j]);
	if( j == field_num ) catval = (int) (atof(fd1[j]) + 0.5);	
      }

    }

    G_free(strflags);

    return catval;

  }

  else return 0;
	  

}
  
