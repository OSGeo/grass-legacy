/******************************************************************************
 * writelin.c
 * modules for writing vector data to
 * GRASS API

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 8th. Apr. 2000
 * Last updated 9th. Apr. 2000
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
#include "shp2dig.h"
#include "writelin.h"
#include "btree.h"


/* Create a repository of segments for inclusion into GRASS
   database */

int vbase2segd( segmentList *seg0, BTREE *btr0 ) {

  /* local variables */
  int i0, j0, k0, k1, l0;
  int segsize0 = 50;
  int segincr = 20;
  int segID = 0;

  int chainLength = 100;
  int chainIncr = 50;
  int pntID = 0;

  char *tmpKey;
  void *tmpData;

  char *tffile = "/tmp/tree_data.log";
  FILE *datalog;

  int chaining;  /* flag set while tracking a segment is active */
  
  /* Possibly required co-ordinates */
  double xstart, xmid, xend, ystart, ymid, yend;

  pntDescript *pinit, *pcurr, *pnext, *pprev, *pstart, *pnew;

  /* Initialise data holders */
  tmpKey = (char *)malloc( 33 );
  seg0->segments = (segmentDescript *)malloc( segsize0 * sizeof
					      (segmentDescript) );
  

  if( (datalog = fopen( tffile, "w" )) == NULL ) {
    fprintf( stderr, "Could not open file to log data errors" );
    exit(1);
  }
  
  /* Rewind database to root */
  btree_rewind( btr0 );

  /* Start detecting and tracking nodes */

  while( btree_next( btr0, &tmpKey, &tmpData ) != 0 ) {


    pntDescript **data1;

    data1 = (pntDescript **)tmpData;
    pcurr = *data1;

    if( pcurr->linknum == 1 )
      fprintf( datalog, "Single-end node at %.6f, %.6f\n",
	pcurr->xPosn, pcurr->yPosn );
    if( pcurr->linknum == 2 )
      fprintf( datalog, "Vertex interior to polyline. Not starting at %.6f %.6f (%d)\n",
	       pcurr->xPosn, pcurr->yPosn, pcurr->duff );
    if( pcurr->linknum == 0 )
      fprintf( datalog, "Isolated point at %.6f %.6f. Skipping.\n",
	       pcurr->xPosn, pcurr->yPosn );
    if( pcurr->linknum == 2 ||  pcurr->linknum == 0 ) continue;
    else {
      fprintf( datalog, "\nTracking from node at %.6f %.6f\n",
	      pcurr->xPosn, pcurr->yPosn );
      for( i0 = 0; i0 < pcurr->linknum; ++i0 )
	fprintf( datalog, "   Track %d leads to point registered at %X ( %.6f %.6f)\n",
		 i0, pcurr->linkverts[i0], pcurr->linkverts[i0]->xPosn,
		 pcurr->linkverts[i0]->yPosn );
      pinit = pcurr;
      for( i0 = 0; i0 < pcurr->linknum; ++i0 ) {
	/* Initialise the starting node */
	pcurr = pinit;
	/* Get the first link */
	pnext = pcurr->linkverts[i0];
	fprintf( datalog, "Starting track %d:", i0 );
	if( pnext->duff ) fprintf( datalog, "Old track. Not continuing.\n" );
	else fprintf( datalog, "\n" );
	if( pnext->duff ) continue; /* Old track */
	else /* Track it */
	  {
	    if( seg0->numSegments >= segsize0 ) {
	      segsize0 += segincr;
	      seg0->segments = (segmentDescript *)realloc( seg0->segments, 
							   segsize0 * sizeof
							   (segmentDescript) );
	    }
	    seg0->numSegments++;
	    segID = seg0->numSegments;
	    if( pcurr->linknum == 1 )
	      seg0->segments[segID-1].duff = 1;
	    else
	      seg0->segments[segID-1].duff = 0;

	    /* Now initialise the pointer list for vertices */

	    /* Ignore 'node' attributes, but clear them */
	    seg0->segments[segID-1].startnode = NULL;
	    seg0->segments[segID-1].endnode = NULL;
	    
	    seg0->segments[segID-1].vertices = 
	      (pntDescript **)malloc( chainLength * sizeof(pntDescript *) );

	    /* Add the starting node to the list of vertices */
	    seg0->segments[segID-1].vertices[0] = pcurr;
	    seg0->segments[segID-1].numVertices = 1;

	    /* Start tracking */

	    pstart = pcurr;
	    chaining = 1;
	    while( chaining ) {

	      if(seg0->segments[segID-1].numVertices > chainLength) {
		chainLength += chainIncr;
		seg0->segments[segID-1].vertices = 
		  (pntDescript **)realloc( seg0->segments[segID-1].vertices, 
					   chainLength * sizeof(pntDescript *) );		
	      }

	      pprev = pcurr;
	      pcurr = pnext;
	      seg0->segments[segID-1].numVertices++;
	      pntID = seg0->segments[segID-1].numVertices - 1;

	      /* If this is a node we stop */
	      if( pcurr->linknum == 1 || pcurr->linknum > 2 ) {
		chaining = 0;
		pnext = NULL;
	      }
	      /* if not, we get the next link */
	      else
		{
		  for( j0 = 0; j0 < 2; ++j0 ) {
		    if( pcurr->linkverts[j0] == pprev )
		      continue;
		    else
		      {
			pnext = pcurr->linkverts[j0];
			break;
		      }
		  }
		}

	      if( pcurr->linknum == 1 ) seg0->segments[segID].duff = 1;

	      /* Deal with the anomalous case of a single link
		 between two nodes 
	      */
	      if( chaining == 0 && pntID == 1 ) {
		/* Create a new point */
		pnew = (pntDescript *)malloc( sizeof (pntDescript) );
		xstart = pprev->xPosn;
		xend = pcurr->xPosn;
		ystart = pprev->yPosn;
		yend = pcurr->yPosn;
		xmid = ( xstart + xend ) / 2.0;
		ymid = ( ystart + yend ) / 2.0;
		pnew->duff = 0;
		pnew->isnode = 0;
		pnew->xPosn = xmid;
		pnew->yPosn = ymid;
		pnew->zVal = 0.0;
		pnew->mVal = 0.0;
		pnew->linkverts = NULL; /* ignore links */
		pnew->linkdirect = NULL;
		pnew->linknum = 0; 

		fprintf( datalog, "Vertex at %.6f %.6f.\n",
			 pnew->xPosn, pnew->yPosn );
		fprintf( datalog, "Ending node at %.6f %.6f.\n",
			 pcurr->xPosn, pcurr->yPosn );
		seg0->segments[segID-1].vertices[1] = pnew;
		seg0->segments[segID-1].vertices[2] = pcurr;
		seg0->segments[segID-1].numVertices = 3;
		if( pcurr->linknum == 1 )
		  seg0->segments[segID-1].duff = 1;
		continue;
	      }

	      /* Otherwise add the point and blank it */
	      if( chaining == 1 ) pcurr->duff = 1;
	      if( chaining == 0 && pcurr->linknum == 1 )
		seg0->segments[segID-1].duff = 1;
	      if( chaining == 0 ) fprintf( datalog, "Ending node " );
	      else fprintf( datalog, "Vertex " );
	      fprintf( datalog, "at %.6f %.6f.\n",
		       pcurr->xPosn, pcurr->yPosn );
	      seg0->segments[segID-1].vertices[pntID] = pcurr;
	    }
	    fprintf( datalog, "\nAggregate segment %d completed\n\n",
		    segID );
	      
	  }
      }
    }

  }

  return 1;

  /* still have to check for simple islands */
  btree_rewind( btr0 );

  while( btree_next( btr0, &tmpKey, &tmpData ) != 0 ) {
    

    pntDescript **data1;

    data1 = (pntDescript **)tmpData;
    pcurr = *data1;

    if( pcurr->linknum != 2 ) continue;
    else
      {
	/* Get the first link */
	pnext = pcurr->linkverts[0];
	if( pnext->duff ) continue; /* Somehow we've reached an old track */
	else /* start tracking */
	  {
	    if( seg0->numSegments >= segsize0 ) {
	      segsize0 += segincr;
	      seg0->segments = (segmentDescript *)realloc( seg0->segments, 
							   segsize0 * sizeof
							   (segmentDescript) );
	    }
	    seg0->numSegments++;
	    segID = seg0->numSegments;
	    if( pcurr->linknum == 1 ) /* This shouldn't happen, but ... */
	      seg0->segments[segID-1].duff = 1;
	    else
	      seg0->segments[segID-1].duff = 0;
	    
	    /* As before ... */
	    /* Now initialise the pointer list for vertices */

	    /* Ignore 'node' attributes, but clear them */
	    seg0->segments[segID-1].startnode = NULL;
	    seg0->segments[segID-1].endnode = NULL;
	    
	    seg0->segments[segID-1].vertices = 
	      (pntDescript **)malloc( chainLength * sizeof(pntDescript *) );

	    /* Add the starting node to the list of vertices */
	    seg0->segments[segID-1].vertices[0] = pcurr;
	    seg0->segments[segID-1].numVertices = 1;
	  }
	    pstart = pcurr;
	    chaining = 1;
	    while( chaining ) {
	      
	      if(seg0->segments[segID-1].numVertices > chainLength) {
		chainLength += chainIncr;
		seg0->segments[segID-1].vertices = 
		  (pntDescript **)realloc( seg0->segments[segID-1].vertices, 
					   chainLength * sizeof(pntDescript *) );		
	      }

	      pprev = pcurr;
	      pcurr = pnext;
	      seg0->segments[segID].numVertices++;
	      pntID = seg0->segments[segID-1].numVertices - 1;

	      /* If we're back to the start, stop */

	      if( pcurr == pstart ) {
		chaining = 0;
		pnext = NULL;
	      }
	      /* if not, we get the next link */
	      else
		{
		  for( j0 = 0; j0 < 2; ++j0 ) {
		    if( pcurr->linkverts[j0] == pprev )
		      continue;
		    else
		      {
			pnext = pcurr->linkverts[j0];
			break;
		      }
		  }
		}

	      /* Add vertex */
	      pcurr->duff = 1;
	      seg0->segments[segID-1].vertices[pntID] = pcurr;

	    }
      }

  }
}
