/******************************************************************************
 * cleanup.c
 * modules for post-processing imported vector files

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 9th. May. 2000
 * Last updated 9th. May. 2000
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
#include <malloc.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include "cleanup.h"
#include "gis.h"


/* Apply vector cleanup procedures */

void vector_map_cleanup( char *mapname ) {

  /* locals */

  char err_buf[100], **par_string;
  char progname[101];

  int err0;
  int res;
  int i0; /* loop variable */

  /* Check that vector map can be found */

  if( G_find_vector2( mapname, G_mapset() ) == NULL ) {
    sprintf( err_buf, "Unable to find vector map `%s' in current mapset\n", mapname );
    G_fatal_error( err_buf );
  }

  /* Call v.spag to elliminate duplicate lines */

  par_string = (char **)malloc( 4 * sizeof( char *) );
  for(i0 = 0; i0 < 3; ++i0) {
    par_string[i0] = (char *)malloc( 50 * sizeof(char) );
  }

  strncpy( progname, getenv("GISBASE"), 99 );
  strncat( progname, "/bin/v.spag", 99);
  strncat( progname, " -i map=", 99 );
  strncat( progname, mapname, 99 );
  
  progname[100] = '\0';

  /*strcpy( par_string[0], "v.spag" );
  strcpy( par_string[1], "-i" );
  strcpy( par_string[2], "map=" );
  strncat( par_string[2], mapname, 45);
  par_string[2][49] = '\0';
  par_string[3] = NULL;
  */

  /* res = execvp( progname, par_string ); */
  system(progname);
  err0 = errno;

  if( res == -1 ) {

    switch(res) {
    case ENOEXEC: {
      sprintf( err_buf, "Insufficient environment space to run v.spag.\n" );
      G_fatal_error(err_buf);
      break;
    }
    case ENOMEM: {
      sprintf( err_buf, "Insufficient memory to run v.spag.\n" );
      G_fatal_error(err_buf); 
      break;
    }
    default: {
      sprintf( err_buf, "v.spag returned error.\n" );
      G_fatal_error(err_buf);
      break;
    }
    }

  }

}
  
  
