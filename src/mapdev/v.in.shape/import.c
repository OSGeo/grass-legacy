/******************************************************************************
 * import.c [v.in.shape]
 * Import ESRI Shapefile: Routines to manipulate importer.
 * 

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 26th. Feb. 2002
 * Last updated 26th. Feb. 2002
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
#include "import.h"

/******************************************************************************
 *                                                                            *
 *              Initialise a parameter importer structure                     *
 *                                                                            *
 ******************************************************************************/


param_import_ctrl *import_ctrl_init(void) {

  param_import_ctrl *pic1;

  pic1 = (param_import_ctrl *)malloc( sizeof(param_import_ctrl) );
  if(pic1 == NULL) return(NULL);
  else {
    pic1->logptr = NULL;
    pic1->over_write = 0;
    pic1->verbose_level = 1;
    pic1->fle = 0;
    pic1->unique = 0;
    pic1->snap = 0.0;
    pic1->sliver = 0.0;
    strcpy(pic1->src, "");
    strcpy(pic1->dest, "");
    strcpy(pic1->att, "");
    strcpy(pic1->cat, "");
    return (pic1);
  }
}

/******************************************************************************
 *                                                                            *
 *                Destroy a parameter importer structure                      *
 *                                                                            *
 ******************************************************************************/

void import_ctrl_destroy(param_import_ctrl *pic0) {

  free(pic0);
}

/******************************************************************************
 *                                                                            *
 *     Construct a parameter importer structure (with values                  *
 *                                                                            *
 ******************************************************************************/

void import_ctrl_construct(param_import_ctrl *pic0, char *s_name, char *d_name, FILE *fp0, 
			   const int owr, const int vl, const int forcele, const int u_val,
			   const int mapscale, 
			   const double snapdist, const double atol,
			   char *attfield, char *catfield) {

  pic0->logptr = fp0;
  pic0->over_write = owr;
  pic0->verbose_level = vl;
  pic0->fle = forcele;
  pic0->unique = u_val;
  pic0->scale = mapscale;
  pic0->snap = snapdist;
  pic0->sliver = atol;
  strncpy(pic0->src, s_name, 511);
  strncpy(pic0->dest, d_name, 511);
  strncpy(pic0->att, attfield, 63);
  strncpy(pic0->cat, catfield, 63);
}

