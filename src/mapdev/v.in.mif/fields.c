/******************************************************************************
 * fields.c [v.in.mif]
 * Dump list and type of MID file fields to stderr

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 17th. Oct. 2000
 * Last updated 17th. Oct. 2000
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
#include "local_structs.h"
#include "local_proto.h"

extern field_data field_info;
extern field_data field_type;
extern int numcols;


void dump_field_info(void) {

  int i;    /* loop */

  fprintf(stderr, "\nAVAILABLE FIELDS:\n\n");

  for( i = 0; i < numcols; i++ )
    fprintf(stderr, "Field %d is %s of type %s\n", i + 1, field_info[i], field_type[i]);
  
}
