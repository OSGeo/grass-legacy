/******************************************************************************
 * dbutils.h
 * Prototypes for database routines

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 14th. Mar. 2000
 * Last updated 14th. Mar. 2000
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

#ifndef _DB_UTILS_TOPO_H

#define _DB_UTILS_TOPO_H

#include "shp2dig.h"

#define SET_SD 1
#define GET_SD 0

#define GET_VAL 0
#define SET_VAL 1

/* Functions */
int vertRegister( BTREE *, partDescript *, int );
char *calcKeyValue( pntDescript *, float, int, double, double );

/* Helper Function Prototypes */

int btree_compare( char *, char * );
int procSnapDistance( int , float * );
int procMinSubtend( int, float * );
int proc_key_params( int, int *, double *, double * );

#endif /* _DB_UTILS_TOPO_H */
