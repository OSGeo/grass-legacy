/******************************************************************************
 * category.c 
 * Routines to handle category structures.

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 19th. Feb. 2002
 * Last updated 19th. Feb. 2002
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

#include "category.h"

category_ctrl *category_ctrl_init(void) {

  category_ctrl *cac1;

  cac1 = (category_ctrl *)malloc( sizeof(category_ctrl) );
  if(cac1 == NULL)
    return(NULL);

  cac1->n_cats = 0;
  cac1->as_ascii = CATS_AS_BINARY;
  cac1->struct_status = CATS_INITIALISED;

  return(cac1);
}

void category_ctrl_destroy(category_ctrl *cac0) {

  if(cac0->n_cats > 0) {
    free(cac0->cat_names);
    free(cac0->cat_types);
    free(cac0->cat_osets);
    free(cac0->cats);
  }

  free(cac0);
}

