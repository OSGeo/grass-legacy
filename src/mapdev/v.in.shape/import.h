/******************************************************************************
 * import.h [v.in.shape]
 * Import ESRI Shapefile: Import parameters held in this struct
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

#ifndef IMPORT_SHP_H_
#define IMPORT_SHP_H_

typedef struct parameters_importer_ {

  char src[512];
  char dest[512];
  FILE *logptr;
  int over_write, verbose_level, fle, unique;
  int scale;
  double sliver;
  double snap;
  char att[64];
  char cat[64];

} param_import_ctrl;

/* Prototypes */

param_import_ctrl *import_ctrl_init(void);
void import_ctrl_destroy(param_import_ctrl *);
void import_ctrl_construct(param_import_ctrl *, char *, char *,
			   FILE *, const int, const int,
			   const int, const int, const int, const double, const double,
			   char *, char *);
  

#endif  /*  IMPORT_SHP_H_  */

