/******************************************************************************
 * static.c [v.in.mif]
 * Static storage of import parameters

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 16th. Oct. 2000
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



#include "static.h"
#include "local_structs.h"

int proc_init_scale(int flag, int *scale_val) {

  static int scale_ = 200;

  if(!scale_val || *scale_val <= 0) {
    fprintf( stderr, "Could not set scale value\n");
    return -1;
  }

  if( flag == SET_VAL ) {
    scale_ = *scale_val;
    return 0;
  }

  else if( flag == GET_VAL ) {
    *scale_val = scale_;
    return 0;
  }
  
  else return -1;
  
}
