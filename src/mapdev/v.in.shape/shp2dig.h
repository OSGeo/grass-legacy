/******************************************************************************
 * shp2dig.h
 * modules for converting shapefile vector files to
 * GRASS dig (or other topological) format

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 3rd. Feb. 2000
 * Last updated 5th. Mar. 2000
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

#ifndef SHP2DIG_INCLUDE

#define SHP2DIG_INCLUDE

#include "shapefil.h"
#include "gbtree.h"

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                                           */
/*                  Constants and MACROS                     */
/*                                                           */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

#ifdef PI
#define PI_OLD PI
#undef PI
#endif

#define PI 3.14159265358979323846264338327
#define SNAP_RADIUS 0.01
#define HORIZON_WIDTH 0.0000000000000001

#define GET_MT 0
#define SET_MT 1

#define GET_VAL 0
#define SET_VAL 1

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
struct _segmentList;
struct _lineList;
struct _lineDescript;
struct _partDescript;
struct _pntDescript;
struct _pntDictionary;
struct _segmentDescript;
struct _nodeDescript;
struct _areaDescript;
struct _fieldDescript;
struct duff_rec_;
struct duff_recs_t_;


/* Declare typedefs on incomplete types */
typedef struct _segmentList segmentList;
typedef struct _lineList lineList;
typedef struct _lineDescript lineDescript;
typedef struct _partDescript partDescript;
typedef struct _pntDescript pntDescript;
typedef struct _pntDictionary pntDictionary;
typedef struct _segmentDescript segmentDescript;
typedef struct _nodeDescript nodeDescript;
typedef struct _areaDescript areaDescript;
typedef struct _fieldDescript fieldDescript;
typedef struct duff_rec_ duff_rec;
typedef struct duff_recs_t_ duff_recs_t;





/* We start with the geometric structure extracted from the
   shapefile.
*/

/* This structure holds a list of valid segments for inclusion
   in the GRASS database. It records for each segment whether
   it is valid in the context of the list, the pointer to
   the segment and the number of segments.
*/


/* This structure holds the main body of the lines that make up
   a shapefile in its original form.
*/

struct  _lineList {
  int numLines;
  int typeofLine;
  int totalParts;
  int totalValidParts;
  lineDescript *lines;
};


/* This structure breaks the line into its constituent parts
   and provides the overall sign indicator for determining
   the direction of rotation of the line. 
*/

struct _lineDescript {
  int shapeID;   /* recommend 0 if not required */
  double totalindic;
  int numParts;
  int validParts;
  partDescript *parts;
};


/* The description of a polygon part */

struct _partDescript {
  int duff;
  double indic;
  int numPoints;
  int numIntersects;
  double west;    /* The bounding box of the part. More useful than  */
  double east;    /* the global box generated with the shapefile.    */
  double south;   /*                                                 */
  double north;   /*                                                 */
  pntDescript *intersects;
  pntDescript *linepnts;
  areaDescript *centroid;
};


/* Description of an actual point */

struct _pntDescript {
  int duff;
  int isnode;
  double xPosn;
  double yPosn;
  double zVal;     
  double mVal;
  pntDescript **linkverts;  /* Only initialise and use if required        */
  int linknum;		/* links outwards        */
  double *linkdirect;      /* directions of links (math format)          */
};



/* Now build structures to analyse and reconstitute a
   topological map for the data.
*/



struct _segmentList {
  int origID;   /* The original ID from the shapefile. Recommend
		   0 if not required */
  int numSegments;
  segmentDescript *segments;
};


/* The description of a line segment ( that will eventually
   become a dig line in the GRASS database ), including
   its end nodes
*/

struct _segmentDescript {
  int polarity;   /* +1=read forward, -1=read backwards */
  int catID;      /* Principle category (line file) */
  int duff;       /* Currently invalid? */
  int numVertices;
  pntDescript **vertices;
  nodeDescript *startnode; /* ie. at index 0 */
  nodeDescript *endnode;   /* at index N-1   */
};


/* The description of a node */

struct _nodeDescript {
  pntDescript *Vertex;
  segmentDescript **assocSegments;
};


/* A structure for assembling coverage parameters and associated
   data
*/

struct _areaDescript {
  double xcentroid;
  double ycentroid;
  double coverarea;
};


/* Structures for database and category manipulation */

/* Create a structure to hold information about a field and a
   pointer to a record list
*/

struct _fieldDescript {
  int duff;
  int fldSize;
  int fldDec;
  int nRec;
  DBFFieldType fldType;
  char fldName[12];
  dbfRecElement *fldRecs;
};


/* Structures for handling selective extraction */

struct duff_rec_ {

  int rec_no;
  int is_duff;

};


struct duff_recs_t_ {

  int n_recs;
  int alloc_recs;
  
  duff_rec *duff_rec_list;

};




/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                                           */
/*                    Function Prototypes                    */
/*                                                           */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


/* Extract info from shapefiles and DBF files through handles
   defined in shape API, and build line descriptor structure
*/

void linedCreate( lineList *l1, SHPHandle s1, DBFHandle d1,
		  fieldDescript *cat1, BTREE *hBank, int *fcount,
		  duff_recs_t *duff_recs );

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  */
/* This function disposes of all the structures built              */
/* by the linedCreate.                                             */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  */

void linedDispose( lineList *l1, fieldDescript *cat1, int fieldCount );


/* Assemble node dictionary */

void pntAssemble( lineList *l1, pntDictionary *pd0 );


/* Sort an array of point descriptions */

int pntsort( const void *pnt1, const void *pnt2 );
/* qsort_points( struct pntDescript *pntd, int npoints ); */
/* for non-GNU system compatibility */

/* Do two points co-incide? ie. snap */

int pntsCoincide( pntDescript *p1, pntDescript *p2 );


/* Extract the segments of a shape into a segment list and
   initially validate all segments
*/

segmentList *extractSegments( lineDescript *ld0,
				     pntDictionary *pd0 );

/* Function to check if a point is in a given point repository */
/* Returns 0 if not, non-zero if True                          */

int islookup( pntDescript *pnt, pntDictionary *pntDict );


/* Small function for inserting an intermediate non-nodal point in a
   segment that consists of only two points (connecting two nodes).
   GRASS does not deal properly with line segments that have no
   inter-nodal points.

   User _MUST_ ensure that the input is an array to at least two
   point structures - the program will not (cannot) check!
*/

void thirdPoint( pntDescript *pntArray );


/* Fill the fields in a part of an arc to be calculated  */

void partCalcFieldsArc( partDescript *partd );


/* Fill the fields in a part of a polygon to be calculated  */

void partCalcFieldsPolygon( partDescript *partd );


/* Calculate the total area of a polygon shape from parts 
   Might not be useful.
*/

/* void getLineArea( lineDescript *lined ); */

/* How many parts are there in the whole shape file - ie. valid exterior
   perimeters with negative circulation?
*/

void getTotalParts( lineList *L1, duff_recs_t *dr );


/* How many of the parts in a shape are valid in the current context? */

void getValidParts( lineDescript *line1 );


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                                           */
/*                Helper Function Prototypes                 */
/*                                                           */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


/* This function returns the x-coordinate of the two relevant intersects
   in finding the new centroid position
*/

int locateNewCentroid( double *xpos1, double *xpos2, 
			double xcentre, pntDescript *isects, int nIsects );


/* Find a polar angular coordinate (theta), standard circulation as in maths, in range
   [0, 2*PI], given x and y offsets from origin.
*/

double getTheta( double x1, double x2 );


/* Determine if the first ring is an island of the second ring */

int isIslandOf( partDescript *part1, partDescript *part2 );


/* Determine if the centroid of a ring is inside another ring */

int pntInside( partDescript *part1, partDescript *part2, double *maxIsect );


/* Recalculate the centroid of the first ring to be outside the second ring
   which it is assumed to contain.
*/

void recalcCentroid( partDescript *part1, double intsect );

/* Set or retrieve the value of map-type required by various functions remotely */

int procMapType( int iswitch, int *mtype );


/* Set a variable to indicate if we want to reject calculation of the current
   centroid
*/

int proc_reject_centroid( int, int *);


#endif /* SHP2DIG_INCLUDE */
