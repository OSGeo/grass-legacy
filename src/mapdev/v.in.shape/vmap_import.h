/******************************************************************************
 * vmap_import.h [v.in.shape]
 * Import ESRI Shapefile: main import routine
 * 

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 27th. Feb. 2002
 * Last updated 27th. Mar. 2002
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

#ifndef VMAP_IMPORT_H_
#define VMAP_IMPORT_H_

#include "import.h"
#include "gis.h"
#include "Vect.h"
#include "shapefil.h"

#define IS_NULL_TYPE( Z ) ((Z) == 0)
#define IS_ARC_TYPE( Z ) ((Z) == 3 || (Z) == 13 || (Z) == 23)
#define IS_POINT_TYPE( Z ) ((Z) == 1 || (Z) == 11 || (Z) == 21)
#define IS_MULTIPOINT_TYPE( Z ) ((Z) == 8 || (Z) == 18 || (Z) == 28)
#define IS_POLYGON_TYPE( Z ) ((Z) == 5 || (Z) == 15 || (Z) == 25)
#define IS_MULTIPATCH_TYPE( Z ) ((Z) == 31)


/* Prototypes */

int vmap_import(param_import_ctrl *);
int shp_extract_line(SHPObject *, int, struct line_pnts **);
int shp_add_third_point(struct line_pnts *);
int get_vmap_bounds(double *, double *, double *, double *);
int shp_get_field_idx(DBFHandle, param_import_ctrl *, int *, int *);
int shp_write_fields(param_import_ctrl *, FILE *, DBFHandle, struct Categories *, 
		     const int, const int, const int, const double, const double, char);

#endif  /* VMAP_IMPORT_H_  */



