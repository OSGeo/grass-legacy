/******************************************************************************
 * parse.c [v.in.mif]
 * Parse a data line from a .mid file

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
#include "scanner.h"


/* Separate out the fields using the established delimiters */

int parse_all_fields(d_type *dt, field_data *fd0, d_type del0, int icnt, int *fcnt) {

  /* local */

  char *tmpbuf;
  int cnt;

  cnt = 0;

  if( (tmpbuf = strtok(dt[icnt], del0)) == NULL ) {
    fprintf( stderr, "Data contains no fields\n");
    return -1;
  }

  strcpy( (*fd0)[cnt++], tmpbuf );

  while( (tmpbuf = strtok(NULL, del0)) != NULL ) {
    strcpy( (*fd0)[cnt++], tmpbuf );
  }

  *fcnt = cnt;

  return 0;
}
