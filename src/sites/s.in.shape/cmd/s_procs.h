/******************************************************************************
 * s_structs.h
 * structure and procedure declarations for sites-specific operations

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 12th. Apr. 2000
 * Last updated 31st. May. 2000
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

#ifndef S_STRUCT_INCLUDE

#define S_STRUCT_INCLUDE

#include "shapefil.h"

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                                           */
/*                  Constants and MACROS                     */
/*                                                           */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

#define GET_VAL 0
#define SET_VAL 1

#define MAX_DBF_STRING_FIELD_SIZE 64

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                                           */
/*                            Unions                         */
/*                                                           */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


/* Union of field types for DBF records: string/int/double  */

typedef union  {
  char *stringField;
  int intField;
  double doubleField;
} dbfRecElement;


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                                           */
/*                    Structure Prototypes                   */
/*                                                           */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


/* Declare structures */
struct _pntDescriptor;
struct _aggregateDescriptor;
struct _fieldDescriptor;
struct _aggregateList;


/* Declare typedefs on incomplete types */
typedef struct _pntDescriptor pntDescriptor;
typedef struct _aggregateDescriptor aggregateDescriptor;
typedef struct _fieldDescriptor fieldDescriptor;
typedef struct _aggregateList aggregateList;


/* Structure definitions */

struct _pntDescriptor {
  int duff;
  int isnode;
  double xPosn;
  double yPosn;
  double zVal;     
  double mVal;
  pntDescriptor **linkverts;
  int linknum;
  double *linkdirect;
};

struct _aggregateDescriptor {
  int numsites;
  int origID;
  pntDescriptor *sites_group;
};

struct _aggregateList {
  int numaggs;
  aggregateDescriptor *aggs;
};

struct _fieldDescriptor {
  int duff;
  int fldSize;
  int fldDec;
  int nRec;
  DBFFieldType fldType;
  char fldName[12];
  dbfRecElement *fldRecs;
};





/* Function prototype declarations */
int load_mpshape_data( SHPHandle shp0, aggregateList *ag0, 
		     DBFHandle dbf0, fieldDescriptor *fd0, int *nfld );

void dbf_fields_reorder( fieldDescriptor *fd0, const int *nfld );

#endif

