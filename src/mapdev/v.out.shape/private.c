/******************************************************************************
 * private.c
 * static storage locations for parameters.

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>`
 * 1st. Jul. 2000
 * Last updated 1st. Jul. 2000
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
#include "gis.h"
#include "Vect.h"
#include "shapefil.h"
#include "local_structs.h"
#include "local_proto.h"

int proc_logfile( int sflag, char *filename ) {

  /* Set or get the value of a filename */

  static char local_file[128] = "";

  if( sflag == SET_VAL)
    if(strcpy( local_file, filename ) != 0) {
      return -1;
    }
    else {
      return 0;
    }
  else if( sflag == GET_VAL )
    if(strcpy( filename, local_file ) != 0) {
      return -1;
    }
    else {
      return 0;
    }
  else return -1;

}
