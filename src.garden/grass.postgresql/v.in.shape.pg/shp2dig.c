/******************************************************************************
 * shp2dig.c
 * modules for converting shapefile vector files to
 * GRASS dig (or other topological) format

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 3rd. Feb. 2000
 * Last updated 11th. May. 2000
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
#include <math.h>
#include <string.h>
#include <assert.h>
#include "shp2dig.h"

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  */
/* This function carries out the transfer of data from the         */
/* shapefile API into the shp2dig structures, and                  */
/* does the initial processing of data, both category and line. It */
/* takes unallocated lineList and unallocated fieldDescript        */
/* objects, initializes them, allocates the substructures and      */
/* calculates essential structure parameters ( through sub-        */
/* functions.                                                      */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  */

void linedCreate(  lineList *l1, SHPHandle s1, DBFHandle d1,
		   fieldDescript *cat1, BTREE *hBank, int *fcount ) {                        

  /* Local */
  int numFields, numPolygons, numRecs0;
  int nums0, newRecsCount;
  int ltype;

  int i0, j0, j1, k0, k1;
  int i2, j2, k2;

  int recCount; /* Keep a running index when assembling records */

  int notAnIsland; /* Count the number of rings that are not islands of
		      another */

  int combinationsExhausted; /* For checking combinations of exterior rings
				with primary islands */

  int dfield; /* Is a particular field defined - loop-check variable */

  int nolinks; /* Count how many vertex links we fail to register */
  int badring; /* Record if current shape part is to be rejected */

  int pnts_diff; /* Flag points that differ in comparing two lines */

  double topIsect;   /* Store maximum intersect value of island ring */

  dbfRecElement *reclist;

  SHPObject *tmpShp;

  DBFFieldType ftype;
  char fname[12];
  int fsize, fdec;

  /* Begin the process of assembling the line descriptor from shape
     file
  */

  l1->numLines = s1->nRecords;
  l1->lines = (lineDescript *)malloc( l1->numLines * sizeof( lineDescript ));
  for( i0 = 0; i0 < l1->numLines; ++i0 ) {
    tmpShp = SHPReadObject( s1, i0 );
    if( i0 == 0 ) {  
      l1->typeofLine = tmpShp->nSHPType;
      ltype = l1->typeofLine;                        /* Just record this once. Assume the file */
    }					             /* has only one type of shape entry.      */
                                                     /* At this point                          */
    
    l1->lines[i0].shapeID = i0 + 1;
    l1->lines[i0].numParts = tmpShp->nParts;
    l1->lines[i0].parts = (partDescript *)malloc( l1->lines[i0].numParts * 
						  sizeof( partDescript ));
    
    j1 = 0;
    for( j0 = 0; j0 < l1->lines[i0].numParts; ++j0 ) {
      int partStart, partEnd, currVertex;

      badring = 0; /* Assume ring is good */

      l1->lines[i0].parts[j0].intersects = NULL;    
      l1->lines[i0].parts[j0].linepnts = NULL;    
      l1->lines[i0].parts[j0].centroid = NULL;    
      
      l1->lines[i0].parts[j0].duff = 0;    

      partStart = tmpShp->panPartStart[j1];

      if( j0 == l1->lines[i0].numParts - 1 ) partEnd = tmpShp->nVertices - 1;
      else partEnd = tmpShp->panPartStart[j1+1] - 1;

      j1++;

      /* Create the vertex list for the part and loop through the list */
      l1->lines[i0].parts[j0].numPoints = partEnd - partStart + 1;
      l1->lines[i0].parts[j0].linepnts =
	(pntDescript *)malloc( l1->lines[i0].parts[j0].numPoints * sizeof( pntDescript ));
      
      nolinks = 0;
      currVertex = 0;
      for( k0 = partStart; k0 <= partEnd; ++k0 ) {

	/* Declare local variables */

	int k1;
	int n1;

	/* Assemble rings from shape parts */

	/* Force-close the ring */
	if( k0 == partEnd && ( ltype == SHPT_POLYGON || ltype == SHPT_POLYGONZ
			       || ltype == SHPT_POLYGONM )) k1 = partStart;
	else k1 = k0;

	l1->lines[i0].parts[j0].linepnts[currVertex].duff = 0;
	l1->lines[i0].parts[j0].linepnts[currVertex].isnode = 0;
	l1->lines[i0].parts[j0].linepnts[currVertex].xPosn = tmpShp->padfX[k1];
	l1->lines[i0].parts[j0].linepnts[currVertex].yPosn = tmpShp->padfY[k1];
	if( ltype == SHPT_POLYGONZ || ltype == SHPT_ARCZ || ltype == SHPT_MULTIPATCH )
	  l1->lines[i0].parts[j0].linepnts[currVertex].zVal = tmpShp->padfZ[k1];
	if( ltype == SHPT_POLYGONZ || ltype == SHPT_ARCZ || ltype == SHPT_MULTIPATCH 
	    || ltype == SHPT_ARCM || ltype == SHPT_POLYGONM )
	  l1->lines[i0].parts[j0].linepnts[currVertex].mVal = tmpShp->padfM[k1];
	l1->lines[i0].parts[j0].linepnts[currVertex].linkverts = NULL;
	l1->lines[i0].parts[j0].linepnts[currVertex].linkdirect = NULL;
	l1->lines[i0].parts[j0].linepnts[currVertex].linknum = 0;

	/* Do the addition to internal database ( point bank ) here */
	/* This is an integral part of the process of dissecting lines
	 to obtain arc segments from shape polygon arcs.
	 
        */
	
	
	if( !vertRegister( hBank, &l1->lines[i0].parts[j0], currVertex ) )
	  nolinks++;

	/* Increment block's internal counter */
	currVertex++;
      }


      /* Fill in the calculated fields of the part descriptor */
      /* This gets everything except the area point, which requires
         information on all the rings of a shape before this (these)
	 can be assessed. However a provisional centroid is
	 created to account for this.
      */

      if( ltype == SHPT_ARC || ltype == SHPT_ARCZ || ltype == SHPT_ARCM )
	partCalcFieldsArc ( &l1->lines[i0].parts[j0] );
      else if( ltype == SHPT_POLYGON || ltype == SHPT_POLYGONZ || ltype == SHPT_POLYGONM )
	partCalcFieldsPolygon ( &l1->lines[i0].parts[j0] );

      /* Check for duplicates. Reject this ring if it is already registered, and
	 moreover is an exterior ring
      */
      if( nolinks == l1->lines[i0].parts[j0].numPoints && 
	l1->lines[i0].parts[j0].indic < 0  ) {
	
	/* Run thru previous listings if this is the case to
	   find the already recorded rings or lines, and
	   determine circulation
	*/
	for( i2 = 0; i2 <= i0; ++i2 ) {
	  for( j2 = 0; j2 < l1->lines[i2].numParts; ++j2 ) {

	    /* Finish  if this is not registered yet */
	    if( i2 == i0 && j2 >= j0 ) break;

	    /* Go on if this is a hole */
	    if( (ltype == SHPT_POLYGON || ltype == SHPT_POLYGONZ || ltype == SHPT_POLYGONM)
		&& l1->lines[i2].parts[j2].indic > 0 )
	      continue;

	    /* Go on if the number of points do not match */
	    if( l1->lines[i0].parts[j0].numPoints != l1->lines[i2].parts[j2].numPoints )
	      continue;

	    /* Go on if bounding box is different */
	    if( fabs(l1->lines[i0].parts[j0].west - l1->lines[i2].parts[j2].west)
		> SNAP_RADIUS 
		&& fabs(l1->lines[i0].parts[j0].east - l1->lines[i2].parts[j2].east)
		> SNAP_RADIUS 
		&& fabs(l1->lines[i0].parts[j0].south - l1->lines[i2].parts[j2].south)
		> SNAP_RADIUS 
		&& fabs(l1->lines[i0].parts[j0].north - l1->lines[i2].parts[j2].north)
		> SNAP_RADIUS 
		) continue;
	    
	    /* Go in if a point is found to be different */
	    pnts_diff = 0;
	    for( k2 = 0; k2 < l1->lines[i0].parts[j0].numPoints; ++k2 ) {
	      if( fabs( l1->lines[i0].parts[j0].linepnts[k2].xPosn -
			l1->lines[i2].parts[j2].linepnts[k2].xPosn ) 
		  > SNAP_RADIUS  &&
		  fabs( l1->lines[i0].parts[j0].linepnts[k2].yPosn -
			l1->lines[i2].parts[j2].linepnts[k2].yPosn ) 
		  > SNAP_RADIUS
		  ) pnts_diff = 1;;
	    }
	    if( pnts_diff ) continue;

	    /* Still here? We have already registered this ring.
	       Rubber it.
	    */
	    
	    badring = 1;
	    break;
	  }
	}
      } /* nolinks */

      if( badring ) {
	l1->lines[i0].numParts--;
	j0--;
	continue;	
      }
    }
    /* Dismiss Shape Object */
    SHPDestroyObject( tmpShp );

    /* Calculate total area of the shape, if appropriate - set to 0.0
       if arc file
    */
    /* getLineArea( &l1->lines[i0] ); */ /* Maybe eventually */

    /* How many of the parts of the line descriptor are valid exterior
       rings?
    */

    getValidParts( &l1->lines[i0] );
  }

  getTotalParts( l1 );

  /* In the case of polygon files . . . */

  if( ltype == SHPT_POLYGON || ltype == SHPT_POLYGONZ || ltype == SHPT_POLYGONM ) {

    /* . . . Now all rings are processed, determine the final area point, ie.
     make sure that it is not in an island
    */

    for( i0 = 0; i0 < l1->numLines; ++i0 ) {
      for( j0 = 0; j0 < l1->lines[i0].numParts; ++j0 ) {

	if( l1->lines[i0].parts[j0].duff )
	  continue;

	combinationsExhausted = 0;
	while( !combinationsExhausted ) {
	  notAnIsland = 0;
	  for( k0 = 0; k0 < l1->lines[i0].numParts; ++k0 ) {
	    if( k0 == j0 || !l1->lines[i0].parts[k0].duff ) {
	      notAnIsland++;
	      continue;
	    }
	  
	    if( isIslandOf( &l1->lines[i0].parts[k0], &l1->lines[i0].parts[j0] ) ) {
	    
	      if( pntInside( &l1->lines[i0].parts[j0], &l1->lines[i0].parts[k0], 
			     &topIsect ) ) {
		recalcCentroid( &l1->lines[i0].parts[j0], topIsect );
	      }
	      else
		notAnIsland++;
	    }
	    else {
	      notAnIsland++;
	    }
	  }

	  if( notAnIsland == l1->lines[i0].numParts )
	    combinationsExhausted = 1;
	}
      }
    }

  }


  /* Now process the database records and add an entry for each valid
     ring
  */
  
  /* How many fields? records (initially)? */
  numFields = DBFGetFieldCount( d1 );
  *fcount = numFields;
  numRecs0 = DBFGetRecordCount( d1 );

  /* Now this should be the same as the record count from the shape file */
  assert( numRecs0 == l1->numLines );

  cat1[0].fldSize = 10;
  cat1[0].fldDec = 0;
  cat1[0].nRec = l1->totalValidParts;
  cat1[0].fldType = FTInteger;
  strcpy( cat1[0].fldName, "XT__ID" );

  cat1[1].fldSize = 10;
  cat1[1].fldDec = 0;
  cat1[1].nRec = l1->totalValidParts;
  cat1[1].fldType = FTInteger;
  strcpy( cat1[1].fldName, "XT__ORIG_ID" );

  cat1[2].fldSize = 18;
  cat1[2].fldDec = 6;
  cat1[2].nRec = l1->totalValidParts;
  cat1[2].fldType = FTDouble;
  strcpy( cat1[2].fldName, "XT__X_LOCN" );
  
  cat1[3].fldSize = 18;
  cat1[3].fldDec = 6;
  cat1[3].nRec = l1->totalValidParts;
  cat1[3].fldType = FTDouble;
  strcpy( cat1[3].fldName, "XT__Y_LOCN" );

  for( i0 = 0; i0 < numFields; ++i0 ) {
    ftype = DBFGetFieldInfo( d1, i0, fname, &fsize, &fdec );
    cat1[i0+4].fldSize = fsize;
    cat1[i0+4].fldDec = fdec;
    cat1[i0+4].nRec = l1->totalValidParts;
    cat1[i0+4].fldType = ftype;
    strcpy( cat1[i0+4].fldName, fname );
  }

  /* Add records to field descriptors */

  for( i0 = 0; i0 < numFields; ++i0 ) {
    reclist = ( dbfRecElement * )malloc( l1->totalValidParts * sizeof( dbfRecElement ));
    recCount = 0;
    switch( cat1[i0+4].fldType ) {
    case 0: 
      for(  j0 = 0; j0 < numRecs0; ++j0 ) {
	for( k0 = 0; k0 < l1->lines[j0].validParts; ++k0 ) {
	  reclist[recCount].stringField = (char *)malloc( cat1[i0+4].fldSize + 1 );
	  strcpy( reclist[recCount++].stringField, DBFReadStringAttribute( d1, j0, i0 ) );
	}
      }
      cat1[i0+4].fldRecs = reclist;
      assert( recCount == l1->totalValidParts );
      break;
    case 1: 
      for(  j0 = 0; j0 < numRecs0; ++j0 ) {
	for( k0 = 0; k0 < l1->lines[j0].validParts; ++k0 ) {
	  reclist[recCount++].intField = DBFReadIntegerAttribute( d1, j0, i0 );
	}
      }
      cat1[i0+4].fldRecs = reclist;
      assert( recCount == l1->totalValidParts );
      break;
    case 2: 
      for(  j0 = 0; j0 < numRecs0; ++j0 ) {
	for( k0 = 0; k0 < l1->lines[j0].validParts; ++k0 ) {
	  reclist[recCount++].doubleField = DBFReadDoubleAttribute( d1, j0, i0 );
	}
      }
      cat1[i0+4].fldRecs = reclist;
      assert( recCount == l1->totalValidParts );
      break;
    default: 
      for(  j0 = 0; j0 < numRecs0; ++j0 ) {
	for( k0 = 0; k0 < l1->lines[j0].validParts; ++k0 ) {
	  reclist[recCount++].intField = 0;
	}
      }
      cat1[i0+4].fldRecs = reclist;
      assert( recCount == l1->totalValidParts );
      break;

    } /* switch */
  }
  
  /* Field 0 is to maintain an index of the entries, starting
     at 1. Field 1 maintains a record of the original entry
     in the input, ie. shapeID.
  */
  reclist = ( dbfRecElement * )malloc( l1->totalValidParts * sizeof( dbfRecElement ));
  recCount = 0;
  for( j0 = 0; j0 < numRecs0; ++j0 ) {
      for( k0 = 0; k0 < l1->lines[j0].validParts; ++k0 ) {
	reclist[recCount].intField = recCount + 1;
	recCount++;
      }
  }
  cat1[0].fldRecs = reclist;
  assert( recCount == l1->totalValidParts );

  reclist = ( dbfRecElement * )malloc( l1->totalValidParts * sizeof( dbfRecElement ));
  recCount = 0;
  for( j0 = 0; j0 < numRecs0; ++j0 ) {
      for( k0 = 0; k0 < l1->lines[j0].validParts; ++k0 ) {
	reclist[recCount++].intField = j0 + 1;
      }
  }
  cat1[1].fldRecs = reclist;
  assert( recCount == l1->totalValidParts );

  /* Field 3 is the x-co-ordinate of the record point.
     Field 4 is the y-co-ordinate of the record point.
  */

  reclist = ( dbfRecElement * )malloc( l1->totalValidParts * sizeof( dbfRecElement ));
  recCount = 0;
  for( j0 = 0; j0 < numRecs0; ++j0 ) {

    k1 = 0;
    for( k0 = 0; k0 < l1->lines[j0].validParts; ++k0 ) {

      /* Find the precise valid rings */
      dfield = 0;
      while( !dfield ) {
	dfield = !l1->lines[j0].parts[k1++].duff;
      }
      reclist[recCount++].doubleField = l1->lines[j0].parts[k1-1].centroid->xcentroid;
    }
  }
  cat1[2].fldRecs = reclist;
  assert( recCount == l1->totalValidParts );
  
  reclist = ( dbfRecElement * )malloc( l1->totalValidParts * sizeof( dbfRecElement ));
  recCount = 0;
  for( j0 = 0; j0 < numRecs0; ++j0 ) {

    k1 = 0;
    for( k0 = 0; k0 < l1->lines[j0].validParts; ++k0 ) {

      /* Find the precise valid rings */
      dfield = 0;
      while( !dfield ) {
	dfield = !l1->lines[j0].parts[k1++].duff;
      }
      reclist[recCount++].doubleField = l1->lines[j0].parts[k1-1].centroid->ycentroid;
    }
  }
  cat1[3].fldRecs = reclist;
  assert( recCount == l1->totalValidParts );
  

}  /* end linedCreate */




/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  */


/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  */
/* This function disposes of all the structures built              */
/* by the linedCreate.                                             */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  */

void linedDispose( lineList *l1, fieldDescript *cat1, int fieldCount ) {
  
  int i0, j0, k0;

  for( i0 = 0; i0 < l1->numLines; ++i0 ) {
    for( j0 = 0; j0 < l1->lines[i0].numParts; ++j0 ) {
      for( k0 = 0; k0 < l1->lines[i0].parts[j0].numPoints; ++k0 ) {
	if( l1->lines[i0].parts[j0].linepnts[k0].linkverts)
	  free( l1->lines[i0].parts[j0].linepnts[k0].linkverts );
	if( l1->lines[i0].parts[j0].linepnts[k0].linkdirect)
	  free( l1->lines[i0].parts[j0].linepnts[k0].linkdirect );
      }

      if( l1->lines[i0].parts[j0].intersects )
	free( l1->lines[i0].parts[j0].intersects );
      if( l1->lines[i0].parts[j0].linepnts )
	free( l1->lines[i0].parts[j0].linepnts );
      if( l1->lines[i0].parts[j0].centroid )
	free( l1->lines[i0].parts[j0].centroid );
    }

    if( l1->lines[i0].parts )
      free( l1->lines[i0].parts );
  }

  if( l1->lines )
    free( l1->lines );

  for( i0 = 0; i0 < fieldCount + 4; ++i0 ) {
    if( cat1[i0].fldType == FTString ) {
      for( j0 = 0; j0 < cat1[i0].nRec; ++j0 ) {
	if( cat1[i0].fldRecs[j0].stringField )
	  free( cat1[i0].fldRecs[j0].stringField );
      }
    }

    if( cat1[i0].fldRecs )
      free( cat1[i0].fldRecs );
  }

  free( cat1 );
}


/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  */
/*                                                                 */
/* This function examines the constructed line descriptor for      */
/* a polygon file and determines the validity and circulation of   */
/* parts, selecting a suitable location on the interior of a       */
/* valid point for registering an area point.                      */
/*                                                                 */
/* A valid part is one that is circumscribed by a closed arc       */
/* with _negative_ circulation (ie. clockwise). A valid area       */
/* point is interior to the delimiting closed arc, but must not    */
/* be within an island. An island delimiter can be determined      */
/* by having _positive_ circulation.                               */
/*                                                                 */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  */

void partCalcFieldsPolygon( partDescript *partd ) {

  /* Local variables declared here */
  
  int i0, j0;

  /* Maximum, minimum an co-ordinate values */
  double Xmin, Xmax, Xnow, Ymin, Ymax, Ynow;

  /* Co-ordinates of centroid, provisional and actual */
  double tmpCentroidx, tmpCentroidy;

  /* Information on current position vectors, relative to centroid */
  double posvecx, posvecy, posvecx_old, posvecy_old;

  /* Length of vector */
  /* We don't need this - do we? 
  double lenvec, lenvec_old;
  */

  /* Temp variables for calculation of partial or sector angles */
  double theta, theta_old, phi, psi, delta;

  /* This flags us to check if a partial intersect is actually
     comleted when the previous point was within SNAP_RADIUS
     of the horizon. Does the line actually cross or does it
     reflect?
  */
  int remindDirect;
  int remindEnd; /* Just for the terminal points */
  int tryAgain = 1; /* Loop flag for determining if the calculation needs to be 
		       aborted and restarted on the second pass (to find
		       centroid  */

  /* How many intersects? */
  int nIntersects;

  /* Parameters of linear eqn for interpolating intersects */
  double alpha, beta;

  /* Loop variable to cumulate partial circulations */
  double totalCirc;

  /* Temporary holding values for position of intersects of ring and 
     horizon.
  */
  double newx1, newx2, newx, newy;

  /* Pointer to pntDescript struct for creating array of intersects */
  pntDescript *pd0;

  /* Shapefile does not provide bounding-box information for a part
     :(

     Must get this first. And choose centre for proposed centroid of
     part.
  */
  Xmin = 1.0e+27;
  Xmax = -1.0e+27;
  Ymin = 1.0e+27;
  Ymax = -1.0e+27;
  for( i0 = 0; i0 < partd->numPoints - 1; ++i0 ) { /* Miss last vertex ( == first ) */
    Xnow = partd->linepnts[i0].xPosn;
    Ynow = partd->linepnts[i0].yPosn;

    if( Xnow < Xmin ) Xmin = Xnow;
    if( Ynow < Ymin ) Ymin = Ynow;
    if( Xnow > Xmax ) Xmax = Xnow;
    if( Ynow > Ymax ) Ymax = Ynow;

  }

  partd->west = Xmin;
  partd->east = Xmax;
  partd->north = Ymax;
  partd->south = Ymin;

  /* Move the centroid a little bit to avoid indeterminacies caused in
     saw-tooth boundary maps
  */
  tmpCentroidx = (( Xmin + Xmax ) / 2.0) - (0.002 * ( Xmax - Xmin ));
  tmpCentroidy = (( Ymin + Ymax ) / 2.0) - (0.002 * ( Ymax - Ymin ));
  
  /* We must create an array of point descriptors to hold the position
     of East-West intersects. These points are eventually located on
     the northing passing through the centre of the ring's bounding
     box. The final centroid will also be located on this (to the
     West if possible, if not, to the East.), except if this is moved because the
     provisional centroid lies on an edge.  This is only temorary so
     size might as well be the number of points, less one, as there are
     really only N-1 points.
  */

  pd0 = NULL;
  pd0 = (pntDescript *)malloc( (partd->numPoints - 1) * sizeof( pntDescript ) );

  /* Also a pointer to an area descriptor for the parameters associated
     with the area point
  */

  partd->centroid = (areaDescript *)malloc( sizeof( areaDescript ) );

  /* Second Pass: Now we determine the circulation of the ring, or if we are
     external to it. Also the position of intersects if we have to move.
  */


  while( tryAgain ) {

    remindEnd = 0;
    remindDirect = 0; /* by default we don't check for incomplete intersects */

    tryAgain = 0;
    totalCirc = 0.0; /* Start with zero measure for circulation */
    nIntersects = 0;

    for( i0 = 0; i0 < partd->numPoints - 1; ++i0 ) { /* Miss last vertex ( == first ) */

      /* Calculate two successive position vectors rel. to centroid */
      posvecx_old = partd->linepnts[i0].xPosn - tmpCentroidx;
      posvecy_old = partd->linepnts[i0].yPosn - tmpCentroidy;
      posvecx = partd->linepnts[i0+1].xPosn - tmpCentroidx;
      posvecy = partd->linepnts[i0+1].yPosn - tmpCentroidy;


      if( fabs(posvecy_old) < HORIZON_WIDTH ||
	  fabs(posvecy) < HORIZON_WIDTH ) {
	/* One of the vertices too close to call. Start again! */
	tryAgain = 1;
	tmpCentroidy -= 0.002 * (Ymax - Ymin);
	break;
      }
      else if( (posvecy_old < 0 && posvecy > 0) ||
	       (posvecy_old > 0 && posvecy < 0)) {
	/* Got an intersect. Calculate it. */

	/* Calculate beta, the y/x slope */
	if( fabs( posvecx - posvecx_old ) < HORIZON_WIDTH ) {
	  /* unless this is undefined */
	  pd0[nIntersects].duff = 0;
	  pd0[nIntersects].isnode = 0;
	  pd0[nIntersects].xPosn = partd->linepnts[i0].xPosn;
	  pd0[nIntersects].yPosn = tmpCentroidy;
	  nIntersects++;
	}
	else {
	  beta = ( posvecy - posvecy_old ) / ( posvecx - posvecx_old );
	  alpha = posvecx_old - ( posvecy_old / beta );

	  pd0[nIntersects].duff = 0;
	  pd0[nIntersects].isnode = 0;
	  pd0[nIntersects].xPosn = alpha + tmpCentroidx;
	  pd0[nIntersects].yPosn = tmpCentroidy;

	  nIntersects++;
	}

      }

      /* Now calculate partial circulation of sector */
      /* THIS WON'T WORK. */
      /* lenvec = sqrt( (posvecx * posvecx) + (posvecy * posvecy) );
	 lenvec_old = sqrt( (posvecx_old * posvecx_old) + (posvecy_old * posvecy_old) );
	 lotalCirc += ( (posvecx_old * posvecy) - (posvecx * posvecy_old) ) /
	 (lenvec * lenvec_old);
      */

      theta_old = getTheta( posvecx_old, posvecy_old );
      theta = getTheta( posvecx, posvecy );
      /*if( (i0 == 1 && theta_old < -1.0) || theta < -1.0 )  {
	tryAgain = 1;
	tmpCentroidy -= 0.002 * ( Ymax - Ymin );
	break;
	} */ /* ? */
	
      delta = theta - theta_old;

      /* Deal with the case of indeterminacy (it's on an edge) */
      if( fabs( fabs( delta ) - PI ) < HORIZON_WIDTH ) {
	tryAgain = 1;
	tmpCentroidy -= 0.002 * ( Ymax - Ymin );
	break;
      }

      /* If not, this delta needs checking for crossing the theta=0 boundary */
      if( delta < -1.0 * PI )
	delta += 2 * PI;
      else if( delta > PI )
	delta -= 2 * PI;

      totalCirc += delta;
    
      /* All done */
    }
    
  }

  newx = tmpCentroidx;
  newy = tmpCentroidy;
  partd->numIntersects = nIntersects;
  partd->intersects = pd0;

  /* Total circulation should be close to 2*PI (6.28...), positive or negative,
     or of negligible value, very close to zero, so, choose [-1, 1] as separator
     region
  */

  if( totalCirc < 1.0 && totalCirc > -1.0 ) {
    /* Ooops! We're outside. must move. */

    /* First determine position of two nearest intersects on west side, if any 
       alternatively on the east side, and check we've found apropriate values;
       can't continue if we haven't.
     */

    locateNewCentroid( &newx1, &newx2, tmpCentroidx, partd->intersects, partd->numIntersects );
    assert( fabs(newx1) < 1.0e10 && fabs(newx2) < 1.0e10 );

    /* Third pass. Have to redetermine the circulation to see if we have an
       exterior ring or an island.
    */

    newx = ( newx1 + newx2 ) / 2.0; newy = tmpCentroidy;

    totalCirc = 0;
    for( i0 = 0; i0 < partd->numPoints - 1; ++i0 ) { /* Miss last vertex ( == first ) */

      /* Calculate two successive position vectors rel. to centroid */
      posvecx_old = partd->linepnts[i0].xPosn - newx;
      posvecy_old = partd->linepnts[i0].yPosn - newy;
      posvecx = partd->linepnts[i0+1].xPosn - newx;
      posvecy = partd->linepnts[i0+1].yPosn - newy;

      /* Now calculate partial circulation of sector */
      /* Don't need to check special cases: this _should_ be a
	 valid centroid of this ring.
      */
      theta_old = getTheta( posvecx_old, posvecy_old );
      theta = getTheta( posvecx, posvecy );
      delta = theta - theta_old;

      if( delta < -1.0 * PI )
	delta += 2 * PI;
      else if( delta > PI )
	delta -= 2 * PI;

      totalCirc += delta;
    }

    /* This should now be positive or negative (non-zero) in value. 
       though some situations seem to arise where it can `escape'.
       We will deal with this eventually for now - flag the parent
       procedure to drop the label
     */
    /* assert( totalCirc > 1.0 || totalCirc < -1.0 ); */
    if(totalCirc < 1.0 && totalCirc > -1.0 ) {
      totalCirc = 0;
    }
  }

  /* Next, dispose situation where circulation is positive ( ie. anti-clockwise, an island
     or hole 
  */
  if( totalCirc > 1.0 ) {
    partd->duff = 1;
    partd->indic = 1.0;
    partd->centroid->xcentroid = newx;
    partd->centroid->ycentroid = newy;
  }

  /* If circulation is negative, we have an exterior ring
  */
  else if( totalCirc < -1.0 ) {
    partd->duff = 0;
    partd->indic = -1.0;
    partd->centroid->xcentroid = newx;
    partd->centroid->ycentroid = newy;
  }

  /* Otherwise we have a dud ring for some reason. Blank it 
   */
  else {
    partd->duff = 1;
    partd->indic = 0.0;
    partd->centroid->xcentroid = 0.0;
    partd->centroid->ycentroid = 0.0;    
  }

  /* THE END */
} /* partCalcFieldsPolygon */


/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  */
/*                                                                 */
/* This function fills the fields of a part descriptor for the     */
/* case of a partial arc in a polyline file.                       */
/*                                                                 */
/* In this case,  the circulation refers to the direction or       */
/* polarity of the line.                                           */
/*                                                                 */
/* +1.0 is forward - ie. first point to last                       */
/*                                                                 */
/* -1.0 is backwards - ie. last to first                           */
/*                                                                 */
/* Since it doesn't matter here. It is always set to +1.0          */
/*                                                                 */
/* The intersects facility also is not activated                   */
/*                                                                 */
/* The line label is attached to the middle point of the arc, or   */
/* the position midway between the middle two points.              */
/*                                                                 */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  */

void partCalcFieldsArc( partDescript *partd ) {
  
  /* Local variables */
  int i0, j0, indx;

  /* Maximum, minimum an co-ordinate values */
  double Xmin, Xmax, Xnow, Ymin, Ymax, Ynow;

  /* Shapefile does not provide bounding-box information for a part
     :(

     Must get this first.
  */
  Xmin = 1.0e27;  Xmax = -1.0e27;
  Ymin = 1.0e27;  Ymax = -1.0e27;
  for( i0 = 0; i0 < partd->numPoints; ++i0 ) {
    Xnow = partd->linepnts[i0].xPosn;
    Ynow = partd->linepnts[i0].yPosn;

    if( Xnow < Xmin ) Xmin = Xnow;
    if( Ynow < Ymin ) Ymin = Ynow;
    if( Xnow > Xmax ) Xmax = Xnow;
    if( Ynow > Ymax ) Ymax = Ynow;
  }

  partd->west = Xmin;
  partd->east = Xmax;
  partd->north = Ymax;
  partd->south = Ymin;

  /* Now determine 'centroid' of the polyline */

  if( partd->numPoints % 2 == 1 ) {
    indx = (partd->numPoints / 2) + 1;
    Xnow = partd->linepnts[indx-1].xPosn;
    Ynow = partd->linepnts[indx-1].yPosn;
  }
  else {
    indx = partd->numPoints / 2;
    Xnow = (partd->linepnts[indx-1].xPosn + partd->linepnts[indx].xPosn) / 2.0;
    Ynow = (partd->linepnts[indx-1].yPosn + partd->linepnts[indx].yPosn) / 2.0;
  }

  partd->duff = 0;
  partd->indic = 1.0;

  /* Allocate space for area descriptor */
  partd->centroid = (areaDescript *)malloc( sizeof( areaDescript ) );

  partd->centroid->xcentroid = Xnow;
  partd->centroid->ycentroid = Ynow;
  
  /* That's all for polyline */

} /* partCalcFieldsArc */


/* How many of the parts in a shape are valid in the current context? */

void getValidParts( lineDescript *line1 ) {
  
  /* Local variables */
  int i0;

  int validTotal = 0;

  for( i0 = 0; i0 < line1->numParts; ++i0 ) {
    if( !line1->parts[i0].duff )
      validTotal++;
  }

  line1->validParts = validTotal;
}


/* How many parts are there in the whole shape file - ie. valid exterior
   perimeters with negative circulation?
*/

void getTotalParts( lineList *L1 ) {
  
  /* Local variables */
  int i0;

  int allTotal = 0;
  int validTotal = 0;

  for( i0 = 0; i0 < L1->numLines; ++i0 ) {
    allTotal += L1->lines[i0].numParts;
    validTotal += L1->lines[i0].validParts;
  }

  L1->totalParts = allTotal;
  L1->totalValidParts = validTotal;
}



/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/*                                                           */
/*                       Helper Functions                    */
/*                                                           */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */


/* Sorting routine for points: needed at some stage for compatibility
   with non-GNU systems

   qsortpoints( struct pntDescript *pntd, int npoints ) {}
*/

/* This is GNU-specific */
int pntsort( const void *pnt1, const void *pnt2 ) {

  /* variable SNAP_RADIUS must be defined in parent function */
  pntDescript *p1;
  pntDescript *p2;

  p1 = (pntDescript *)pnt1;
  p2 = (pntDescript *)pnt2;

  if( pntsCoincide( p1, p2 ) )return 0;
  else {
    if( fabs( p1->yPosn - p2->yPosn ) < 1.0e-15 ) {
      if( p1->xPosn < p2->xPosn ) return -1;
      else return 1;
    }
    else if( p1->yPosn < p2->yPosn ) return 1;
    else return -1;
  }
}

/* Are points the `same' */

int pntsCoincide( pntDescript *p1, pntDescript *p2 ) {
  if( sqrt( ((p1->xPosn - p2->xPosn) * (p1->xPosn - p2->xPosn)) + 
	    (( p1->yPosn - p2->yPosn ) * ( p1->yPosn - p2->yPosn ) ) )
      < SNAP_RADIUS )
    return 0;
  else return 1;
}



/* Helper function for adding a third point to an array of only two
   (nodal) points
*/

void thirdPoint( pntDescript *pntArray ) {
  pntDescript *tmpArray;
  double tmpx, tmpy;

  tmpArray = (pntDescript *)malloc( 3 * sizeof( pntDescript ) );
  tmpArray[1] = pntArray[1];
  tmpArray[3] = pntArray[2];

  tmpx = ( tmpArray[1].xPosn + tmpArray[3].xPosn ) / 2;
  tmpy = ( tmpArray[1].yPosn + tmpArray[3].yPosn ) / 2;

  tmpArray[2].xPosn = tmpx;
  tmpArray[2].yPosn = tmpy;
  tmpArray[2].isnode = 0;
  tmpArray[2].duff = 0;

  free( pntArray );
  pntArray = tmpArray;
}



/* This function gets the x position of significant intersects */

void locateNewCentroid( double *xpos1, double *xpos2, 
			double xcentre, pntDescript *isects, int nIsects ) {

  /* This shouldn't be called if there are less than two intersects */
  /* assert(nIsects > 1); */

  /* Loop variables */
  int i0;

  /* Status Check variable */
  int moretodo;
  
  /* Hold temporary switch (double) value */
  double tmpIsect;

  /* Index in sorted array of a principal index */
  int isectPosn;
  
  /* Create array of x-coordinates */
  double xIsects[nIsects];

  /* Fill this array */
  for( i0 = 0; i0 < nIsects; ++i0 )
    xIsects[i0] = isects[i0].xPosn;

  /* Bubble Sort the array */
  
  moretodo = 1;
  while( moretodo ) {
    moretodo = 0; /* Keep looping till nothing to sort */
    for( i0 = 0; i0 < nIsects - 1; ++i0 ) {
      if( xIsects[i0+1] < xIsects[i0] ) { /* swap values */
	tmpIsect = xIsects[i0+1];
	xIsects[i0+1] = xIsects[i0];
	xIsects[i0] = tmpIsect;
	moretodo++; /* record the fact that a swap has occurred */
      }
    }
  }

  /* Find index of principal intersect */

  if( xIsects[0] > xcentre ) { /* No intersects to west, first and
				  second intersects are the principals
				  on the east side */
    *xpos1 = xIsects[0];
    *xpos2 = xIsects[1];
  }
  else { /* get the first intersect to east and assign _previous_ two
	    as principals */
    for( i0 = 0; i0 < nIsects; ++i0 ) {
      if( i0 == nIsects - 1 && xIsects[i0] < xcentre ) {
	/* reached the end: there are no intersects to east
	   so the principals are the _last_ two in the array */
	*xpos1 = xIsects[nIsects-2];
	*xpos2 = xIsects[nIsects-1];
      }
      else {
	if( xIsects[i0] > xcentre ) {
	  /* Quick check that the west/east intersect populations are
	     even-numbered */
	  assert( i0 % 2 == 0 );
	  *xpos1 = xIsects[i0-2];
	  *xpos2 = xIsects[i0-1];
	  break;
	}
      }
    }

  }
}


/* Find angular co-ordinate given Cartesian cordinates */

double getTheta( double x1, double y1 ) {
  
  /* local */
  double phi;  /* temporary angular value */

  phi = atan2( y1, x1 );

  if( phi < 0 )
    phi = ( 2 * PI ) + phi;

  return phi;
}


/* Determine if the first ring is an island of the second ring */

int isIslandOf( partDescript *part1, partDescript *part2 ) {
  
  /* Local variables */

  int i0;

  int apnt; /* Moveable reference point count */

  double refpntx, refpnty;

  double pvx, pvx_old, pvy, pvy_old;

  double circulation, theta, theta_old, delta;

  int notSure = 1; /* Is the circulation as yet undetermined? */


  /* Check if bounding box conditions allow that Ring 1 might be
     contained in Ring 2
  */

  /* If not terminate, returning 0 */

  if( part1->west < part2->west || part1->east > part2->east ||
      part1->south < part2->south || part1->north > part2->north )
    return 0;

  /* Otherwise continue - now check the circulation of Ring 2
     about Pt 1 of Ring 1, or Pt 2 if this is indeterminate.
  */

  apnt = 0;

  while( notSure ) {
    notSure = 0;
    refpntx = part1->linepnts[apnt].xPosn;
    refpnty = part1->linepnts[apnt].yPosn;
    circulation = 0;
    
    for( i0 = 0; i0 < part2->numPoints - 1; ++i0 ) {
      pvx_old = part2->linepnts[i0].xPosn - refpntx;
      pvy_old = part2->linepnts[i0].yPosn - refpnty;
      pvx = part2->linepnts[i0+1].xPosn - refpntx;
      pvy = part2->linepnts[i0+1].yPosn - refpnty;
      
      theta_old = getTheta( pvx_old, pvy_old );
      theta = getTheta( pvx, pvy );
      
      delta = theta - theta_old;
      
      /* If point on edge, try another point */
      if( fabs( fabs( delta ) - PI) < SNAP_RADIUS ) {
	notSure = 1;
	apnt++;
	break;
      }

      /* Correct for crossing theta=0 boundary */
      if( delta < -1.0 * PI ) delta += 2 * PI;
      else if( delta > PI ) delta -= 2 * PI;

      /* Accumulate circulation */
      circulation += delta;
    }

  }

  /* If circulation is -2 * PI ( << -1.0 ) Ring 1 is contained
     in Ring 2
  */
  if( circulation < -1.0 )
    return 1;
  else return 0;

}


/* Determine if the centroid of a ring is inside another ring */

int pntInside( partDescript *part1, partDescript *part2, double *maxIsect ) {

  int i0;
  
  double intx, intx0; /* Keep track of position of intersects */

  double xpnt, ypnt; /* Position of centroid of Ring 1 */

  double pvx_old, pvx, pvy_old, pvy; /* Position vectors */

  double beta, alpha; /* Linear scaling parameters */

  double theta, theta_old, delta; /* temp angles */

  double circulation; /* Accumulator for circulation */

  /* Assign current registered centroid of Ring 1 */
  xpnt = part1->centroid->xcentroid;
  ypnt = part1->centroid->ycentroid;

  intx = -1.0e27;
  circulation = 0;

  for( i0 = 0; i0 < part2->numPoints - 1; ++i0 ) {
    
    /* Determine position vectors */
    pvx_old = part2->linepnts[i0].xPosn - xpnt;
    pvy_old = part2->linepnts[i0].yPosn - ypnt;
    pvx = part2->linepnts[i0+1].xPosn - xpnt;
    pvy = part2->linepnts[i0+1].yPosn - ypnt;

    if( (pvy < 0 && pvy_old > 0) || (pvy > 0 && pvy_old < 0) ) {
      
      /* Found an intersect - find the x-coordinate */

      beta = ( pvy - pvy_old ) / ( pvx - pvx_old );
      alpha = pvx_old - ( pvy_old / beta );
      intx0 = alpha + xpnt;

      if( intx0 > intx ) intx = intx0;
    }

    /* Find partial circulation. Don't check for point on edge -
       we know it won't be in this case
    */

    theta_old = getTheta( pvx_old, pvy_old );
    theta = getTheta( pvx, pvy );
    delta = theta - theta_old;

    if( delta < -1.0 * PI ) delta += 2 * PI;
    else if( delta > PI ) delta -= 2 * PI;

    circulation += delta;
    
  }

  if( intx < -1.0e20 ) *maxIsect = -1.0e27;
  else *maxIsect = intx;

  if( fabs( circulation ) > 1.0 ) /* it's inside */
    return 1;
  else
    return 0;
}


/* Recalculate the centroid of the first ring to be outside the second ring
   which it is assumed to contain.
*/

void recalcCentroid( partDescript *part1, double intsect ) {
  

  pntDescript *tmpPnt;

  int i0; /* loop variables */

  double princIsect; /* The principal intersect (east of centroid) */

  double currentx; /* x-coordinate of current centroid */

  /* Find the intersect just to the right of the current centroid */
  currentx = part1->centroid->xcentroid;

  princIsect = 1.0e20;

  for( i0 = 0; i0 < part1->numIntersects; ++i0 ) {
    if( part1->intersects[i0].xPosn > currentx &&
	part1->intersects[i0].xPosn < princIsect )
      princIsect = part1->intersects[i0].xPosn;
  }

  /* The principal intersect should be greater than the contained
     intersect imported from the island.
  */
  assert( princIsect > intsect );

  /* The new centroid's x-position is chosen to be half way between the 
     two critical intersects mentioned above
  */

  part1->centroid->xcentroid = ( intsect + princIsect ) / 2.0;
}


int procMapType( int iswitch, int *mtype ) {
  
  static int mt = 0;

  if( iswitch == GET_MT ) {
    *mtype = mt;
    return 0;
  }
  else if( iswitch == SET_MT ) {
    mt = *mtype;
    return 0;
  }
  else return 1;
}
