/******************************************************************************
 * dbutils.c
 * functions for handling processing of internal database
 * relating to topological structures held in memory

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>`
 * 14th. Mar. 2000
 * Last updated 23rd. Apr. 2000
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

#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <setjmp.h>
#include "dbutils.h"
/* #include "gis.h" */
#include <stdio.h> 


int vertRegister( BTREE *hDB, partDescript *part1, int pt_indx ) {

  static void *ptr_old = NULL;
  static int currerror = 0;

  static int num_registered = 0;
  static int db_allocated = 0;

  /* local */
  int i;
  int np;
  int res, res1;
  int result = 0;
  char *pkey;
  float snap;

  double angle0, angle1;
  int linked, lnum;

  pntDescript *pf, *pb, *pc; /* Holders for current and flanking vertices */
  pntDescript *pbl;

  char *keyHolder;
  pntDescript **dataHolder, **tmpdataHolder;
  pntDescript **pntPtrPtr;

  jmp_buf startpnt;


  np = part1->numPoints;

  /* Go on if any point should be invalid */

  if( setjmp(startpnt) ) return 0;

  /* Retrieve snap distance for map */
  if( procSnapDistance( GET_SD, &snap ) ) {
    fprintf(stderr, "Could not set snap distance. Aborting." );
    exit(1);
  }

  /* Assign key value */
  pkey = (char *)malloc( 33 );
  strncpy( pkey, calcKeyValue( &part1->linepnts[pt_indx], snap ), 33 );


  /* Is this point registered in the database? */

  pc = &part1->linepnts[pt_indx];

  pntPtrPtr = (pntDescript **)malloc( sizeof( pntDescript *) );
  keyHolder = (char *)malloc( 33 );
  dataHolder = ( pntDescript ** )malloc( sizeof( pntDescript * ));
  tmpdataHolder = ( pntDescript ** )malloc( sizeof( pntDescript * ));
  
  *pntPtrPtr = pc;

  strncpy(keyHolder, pkey, 33 );
  dataHolder = pntPtrPtr;

  res = btree_find( hDB, keyHolder, &dataHolder );
  if( res == 0 ) btree_update( hDB, keyHolder, 33, dataHolder, 4 );

  if ( res == 1 ) {
    /* Point is already in database. Modify to reflect new links */
    res1 = 0; /* Make this sensible at some point !! */

    /* Get the point */
    /* res1 = hDB->get( hDB, keyHolder, tmpdataHolder, 0 ); */

    if( res1 == -1 ) {
      /* There was an error */
      /* G_fatal_error( "Error retrieving data from database" ); */
    }
    else if( res1 == 1 ) {
      /* No key. This shouldn't happen. Abort! */
      /* G_fatal_error( "Error retrieving data from database" ); */
    }
    else{
      pc = *( (pntDescript **) dataHolder );

      if( pt_indx > 0 ) {
	pb = (pntDescript *) ptr_old;

	/* Is this the same vertex. If so skip */

	if( pc == pb ) longjmp( startpnt, 1);

	/* Are we already linked to this? */
	linked = 0;
	for( i = 0; i < pb->linknum; ++i ) {
	  pbl = pb->linkverts[i];
	  if( pc == pbl )
	    linked = 1;
	}

	/* Determine angle of link to previous vertex */

	angle0 = atan2(( pb->yPosn - pc->yPosn ), ( pb->xPosn - pc->xPosn ) );
	if( angle0 < 0 ) angle0 += 2 * PI ;
	angle1 = angle0 + PI ;
	if( angle1 >= 2 * PI ) angle1 -= 2 * PI ;

	lnum = pc->linknum;

	if( !linked ) {
	  result = 1;
	  if( lnum == 0 ) {
	    pc->linkdirect = (double *)malloc( sizeof( double));
	    pc->linkverts = (pntDescript **)malloc( sizeof (pntDescript *));
	  }
	  else {
	    pc->linkdirect = (double *)realloc( pc->linkdirect, (lnum + 1) * sizeof( double));
	    pc->linkverts = (pntDescript **)realloc( pc->linkverts, (lnum + 1) *
						  sizeof (pntDescript *));
	  }
	  lnum++;
	  pc->linkdirect[lnum-1] = angle0;
	  pc->linkverts[lnum-1] = pb;
	  pc->linknum = lnum;
	}


	  
	/* Now fill in the fields of the previous link */
	lnum = pb->linknum;

	if( !linked ) {
	  if( lnum == 0 ) {
	    pb->linkdirect = (double *)malloc( sizeof( double));
	    pb->linkverts = (pntDescript **)malloc( sizeof (pntDescript *));
	  }
	  else {
	    pb->linkdirect = (double *)realloc( pb->linkdirect, (lnum + 1) * sizeof( double));
	    pb->linkverts = (pntDescript **)realloc( pb->linkverts, (lnum + 1) *
						  sizeof (pntDescript *));
	  }
	  lnum++;
	  pb->linkdirect[lnum-1] = angle1;
	  pb->linkverts[lnum-1] = pc;
	  pb->linknum = lnum;
	}

      }
    }

    ptr_old = pc;
    
  }
  else {
    /* Point is added: reflect new links */
    pc = *( (pntDescript **) dataHolder );
    num_registered++;
    result = 1;

    /* Determine angle of link to previous vertex */
    if( pt_indx > 0 ) {
      pb = (pntDescript *) ptr_old;


      angle0 = atan2(( pb->yPosn - pc->yPosn ), ( pb->xPosn - pc->xPosn ) );
      if( angle0 < 0 ) angle0 += 2 * PI ;
      angle1 = angle0 + PI ;
      if( angle1 >= 2 * PI ) angle1 -= 2 * PI ;

      lnum = 0;

      pc->linkdirect = (double *)malloc( sizeof( double));
      pc->linkverts = (pntDescript **)malloc( sizeof (pntDescript *));
      lnum++;
      pc->linkdirect[lnum-1] = angle0;
      pc->linkverts[lnum-1] = pb;
      pc->linknum = lnum;


	  
      /* Now fill in the fields of the previous link */
      lnum = pb->linknum;

	if( lnum == 0 ) {
	  pb->linkdirect = (double *)malloc( sizeof( double));
	  pb->linkverts = (pntDescript **)malloc( sizeof (pntDescript *));
	}
	else {
	  pb->linkdirect = (double *)realloc( pb->linkdirect, (lnum + 1) * sizeof( double));
	  pb->linkverts = (pntDescript **)realloc( pb->linkverts, (lnum + 1) *
						   sizeof (pntDescript *));
	}
	lnum++;
	pb->linkdirect[lnum-1] = angle1;
	pb->linkverts[lnum-1] = pc;
	pb->linknum = lnum;
    }

    ptr_old = pc;

    
  }

  free( tmpdataHolder );
  return (result);

}


/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

char *calcKeyValue( pntDescript *pnt1, float sr ) {
  /* local */

  double xtmp, ytmp;
  char xbuf[40], ybuf[40];
  char *retbuf;
  int indx;
  char *indx_ptr;

  xtmp = ((int)( pnt1->xPosn / sr )) * sr;
  ytmp = ((int)( pnt1->yPosn / sr )) * sr;

  retbuf = (char *)malloc( 33 );
  
  snprintf( xbuf, 35, "%035.10f", xtmp );
  snprintf( ybuf, 35, "%035.10f", ytmp );

  indx_ptr = strchr( xbuf, '.' );
  strncpy( retbuf, indx_ptr - 13, 13 );
  retbuf[13] = '\0';
  strncat( retbuf, indx_ptr + 1, 3 );
  retbuf[16] = '\0';

  indx_ptr = strchr( ybuf, '.' );
  strncat( retbuf, indx_ptr - 13, 13 );
  retbuf[29] = '\0';
  strncat( retbuf, indx_ptr + 1, 3 );
  retbuf[32] = '\0';

  return retbuf;
}

/* Helper function definitions */

int btree_compare( char *key1, char *key2 ) {
  /* Just compare lexicographically */

  return strncmp( key1, key2, 32 );
}


int procSnapDistance( int iswitch, float *sd ) {
  
  /* Set or get the SNAP_DISTANCE variable */

  static float snap_distance = 0.0;

  if( iswitch == SET_VAL ) {
    if(sd) {
      snap_distance = *sd;
      return 0;
    }
    else return 1;
  }
  else if( iswitch == GET_VAL ) {
    *sd = snap_distance;
    return 0;
  }
  else return 1;
}


int procMinSubtend( int iswitch, float *dphi ) {
  
  /* Set or get the minimum value at which two radii from a node
     are considered to be separate (non-colinear)
  */

  static float minimum_angle = 1.745e-4;

  if( iswitch == SET_VAL ) {
    if(dphi) {
      minimum_angle = *dphi;
      return 0;
    }
    else return 1;
  }
  else if( iswitch == GET_VAL ) {
    *dphi = minimum_angle;
    return 0;
  }
  else return 1;
}

/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
