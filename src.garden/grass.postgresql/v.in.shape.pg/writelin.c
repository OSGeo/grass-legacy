/******************************************************************************
 * writelin.c
 * modules for writing vector data to
 * GRASS API

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 8th. Apr. 2000
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
#include <assert.h>
#include "shp2dig.h"
#include "writelin.h"
#include "dbutils.h"
#include "gbtree.h"


/* Create a repository of segments for inclusion into GRASS
   database */

int vbase2segd( segmentList *seg0, BTREE *btr0 ) {

  /* local variables */
  int i0, j0, k0, k1, l0;
  int segsize0 = 50;
  int segincr = 20;
  int segID = 0;
  int maptype;
  float minangle;
  int deferred = 0;
  double deferval;
  int bindx;

  int chainLength = 100;
  int chainIncr = 50;
  int pntID = 0;

  double phi; /* Temp angle var */
  double xlen0, xlen1, ylen0, ylen1;

  char *tmpKey;
  void *tmpData;

  int chaining;  /* flag set while tracking a segment is active */
  int isdone;    /* flag to avoid extracting further vertices */
  
  /* Possibly required co-ordinates */
  double xstart, xmid, xend, ystart, ymid, yend;
  double len0, len1;

  pntDescript *pinit, *pcurr, *pnext = NULL, *pprev, *pstart, *pnew;
  pntDescript *pdfr0, *pdfr1, *pdfr2, *pmidd, *poffs;

  /* Initialise data holders */
  tmpKey = (char *)malloc( 33 );
  seg0->segments = (segmentDescript *)malloc( segsize0 * sizeof
					      (segmentDescript) );

  if( procMapType( GET_VAL, &maptype ) != 0 ) {
    fprintf( stderr, "Could not retrieve map type. Defaulting to LINE.\n" );
    maptype = 1;
  }

  if( procMinSubtend( GET_VAL, &minangle ) != 0 ) {
    fprintf( stderr, "Could not retrieve minimum angle at vertex. Setting default\n" );
    minangle = 1.745e-4;
  }
  

  /* Rewind database to root */
  btree_rewind( btr0 );

  /* Start detecting and tracking nodes */

  while( btree_next( btr0, &tmpKey, &tmpData ) != 0 ) {


    pntDescript **data1;

    data1 = (pntDescript **)tmpData;
    pcurr = *data1;

    /* Defer snapback nodes till later. ie. if two polylines track
       from this node along an initial colinear track, but the first
       vertex of each is at a different position.
    */

    deferred = 0;
    if( pcurr->linknum > 2 ) {
      for( i0 = 0; i0 < pcurr->linknum; ++i0 ) {
	if(deferred) continue;
	for( j0 = i0 + 1; j0 < pcurr->linknum; ++j0 ) {
	  if(deferred) continue;
	  if( fabs( pcurr->linkdirect[i0] - pcurr->linkdirect[j0] ) < minangle ) {
	    deferred = 1;
	    deferval = pcurr->linkdirect[i0];
	  }
	}
      }
    }
    /* Now deal with regular polylines */

    if( pcurr->linknum == 2 ||  pcurr->linknum == 0 ) continue;
    else {
      pinit = pcurr;
      for( i0 = 0; i0 < pinit->linknum; ++i0 ) {
	/* Initialise the starting node */
	pcurr = pinit;
	/* Check this track is not deferred */
	if( deferred && fabs( pcurr->linkdirect[i0] - deferval ) < minangle ) {
	  /* if it is deferred, do we have to deal with it as a special
	     case because it has a snap-back at either end?
	  */
	  if( check_terminal_snapback( pcurr, pcurr->linkverts[i0], &pmidd ) )
	    build_half_lines( pmidd, seg0 );
	  else
	    continue;
	}
	/* Get the first link */
	pnext = pcurr->linkverts[i0];
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
	    if( pcurr->linknum == 1 && maptype == 2 )
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
	    isdone = 0;
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

	      if( pcurr->linknum == 1 && maptype == 2 ) seg0->segments[segID-1].duff = 1;

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
		seg0->segments[segID-1].vertices[1] = pnew;
		seg0->segments[segID-1].vertices[2] = pcurr;
		seg0->segments[segID-1].numVertices = 3;
		if( pcurr->linknum == 1 && maptype == 2 )
		  seg0->segments[segID-1].duff = 1;
		/* Does this link overwrite a colinear track from another segment? */
		for( k0 = 0; k0 < pprev->linknum; ++k0 ) {
		  if( i0 == k0 ) continue;
		  if( fabs(pprev->linkdirect[i0] - pprev->linkdirect[k0]) 
		      < 1.745e-4 ) {  /* 1% of 1 degree (in radians) */
		    seg0->segments[segID-1].duff = 1;
		    break;
		  }
		}
		if( pprev->linknum == 1 ) {
		  for( k0 = 0; k0 < pcurr->linknum; ++k0 ) {
		    phi = reverse_angle( pcurr->linkdirect[k0] );
		    if( fabs( phi - pprev->linkdirect[i0] ) < 1.745e-4 )
		      seg0->segments[segID-1].duff = 1;
		  }
		}
		continue;
	      }

	      /* Otherwise add the point and blank it */

	      /* if we have reached the end, check the snapback situation */

	      if( chaining == 0 ) {
		/* Get back index of previous link */

		for( k0 = 0; k0 < pcurr->linknum; k0++ ) {
		  if( pcurr->linkverts[k0] == pprev )
		    bindx = k0;
		}

		/* Are there outsets snapped to this one? */
		poffs = NULL;
		for( k0 = 0; k0 < pcurr->linknum; ++k0 ) {
		  if( k0 == bindx ) continue;
		  if( fabs(pcurr->linkdirect[k0] - pcurr->linkdirect[bindx]) < minangle ) {
		    poffs = pcurr->linkverts[k0];
		    break;
		  }
		}

		/* Now deal with snap-backs */
		if(poffs) {
		  xlen0 = pcurr->xPosn - pprev->xPosn;
		  xlen1 = pcurr->xPosn - poffs->xPosn;
		  ylen0 = pcurr->yPosn - pprev->yPosn;
		  ylen1 = pcurr->yPosn - poffs->yPosn;
		  len0 = sqrt( (xlen0 * xlen0) + (ylen0 * ylen0) );
		  len1 = sqrt( (xlen1 * xlen1) + (ylen1 * ylen1) );

		  if( len0 > len1 ) {
		    /* We have drawn the long straw this time */
		    /* Go on, but set the terminal point to the first
		       offset on the secondary track
		    */
		    pcurr = poffs;
		  }
		  else {
		    /* We are dealing with the short line */
		    isdone = 1;

		    /* Don't add this, so decrement the vertex-count again */
		    seg0->segments[segID-1].numVertices--;

		    /* Add a new segment to the list, for the terminal part */
		    extract_simple_link( seg0, pprev, pcurr );
		  }
		}

		
	      }

	      if( chaining == 1 ) pcurr->duff = 1;
	      /* if( chaining == 0 && pcurr->linknum == 1 )
		 seg0->segments[segID-1].duff = 1; */
	      if(!isdone) seg0->segments[segID-1].vertices[pntID] = pcurr;
	    }
	      
	  }
      }
    }

  }

  /* still have to check for simple islands */
  btree_rewind( btr0 );

  while( btree_next( btr0, &tmpKey, &tmpData ) != 0 ) {
    

    pntDescript **data1;

    data1 = (pntDescript **)tmpData;
    pcurr = *data1;

    /* Go on if this is not a simple link in a chain */
    if( pcurr->linknum != 2 ) continue;

    /* And go on if this is a link that has already been encountered */
    if( pcurr->duff ) continue;

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
	      seg0->segments[segID-1].numVertices++;
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

/* Function to dispose of segmentList cleanly */

int segLDispose( segmentList *seg0 ) {
  
  /* Local vars */
  int i0, j0, k0;

  for( i0 = 0; i0 < seg0->numSegments; ++i0 ) {
    for( j0 = 0; j0 < seg0->segments[i0].numVertices; ++j0 ) {
      if( seg0->segments[i0].vertices[j0] )
	seg0->segments[i0].vertices[j0] = NULL;
    }

    if( seg0->segments[i0].vertices )
      free( seg0->segments[i0].vertices );
  }

  if( seg0->segments )
    free( seg0->segments );

  if( seg0 ) free( seg0 );

  return(1);
}


double reverse_angle( double phi0 ) {
  
  /* local */
  double psi;

  psi = phi0 + PI ;
  if( psi >= 2 * PI )
    psi -= 2 * PI ;

  return psi;
}


int check_terminal_snapback( pntDescript *ppnt1, pntDescript *ppnt2, pntDescript **pmiddle ) {
  /* Track a segment without writing to find if there is a snapback at the other end */

  /* local variables */
  int nvert, midvert;
  int indx;
  int is_snapback = 0;

  float minphi;

  int j; /* loop variable */

  pntDescript *ptmp0, *ptmp1, *ptmp2, *ptmp3, *ptmp4;

  /* Get the minimum angle subtended at node */
  if(procMinSubtend( GET_VAL, &minphi ) != 0)
    minphi = 1.745e-4; /* If unable to retrieve, set to default value */

  /* Initiallise the track monitoring variables */
  nvert = 2;
  midvert = 0;

  ptmp0 = ppnt1;
  ptmp1 = ppnt2;

  ptmp3 = ppnt1; /* Set midpoint to position n/2 */
  ptmp4 = ptmp0; /* Previous link for slow tracking */

  /* Start tracking */
  while(ptmp1->linknum == 2) {
    /* Go on tracking */

    if(ptmp3 == NULL) {
      /* Bad line. Probably will not be labelled. Abort. */
      *pmiddle = NULL;
      return(0);
    }

    nvert++;
    midvert = nvert / 2;

    ptmp2 = ptmp0; /* Set previous link */
    ptmp0 = ptmp1;

    /* Get next link */
    if(ptmp1->linkverts[0] == ptmp2)
      ptmp1 = ptmp1->linkverts[1];
    else
      ptmp1 = ptmp1->linkverts[0];

    /* Get next link for midpoint */
    if(nvert % 2 == 0) {
      if(ptmp3->linkverts[0] == ptmp4) {
	ptmp4 = ptmp3;
	ptmp3 = ptmp3->linkverts[1];
      }
      else {
	ptmp4 = ptmp3;
	ptmp3 = ptmp3->linkverts[0];
      }
    }
  }

  /* Don't want a two-vertex link. Scrub this - deal with it if it happens */
  /* AAARGH! It does - frequently! */
  /* assert( nvert != 2 ); */

  /* Find `back-index' leading to current vertex */

  for( j = 0; j < ptmp1->linknum; ++j ) {
    if(ptmp1->linkverts[j] == ptmp0) {
      indx = j;
      break;
    }
  }

  /* Now determine if the link out is a snap-back. */

  for( j = 0; j < ptmp1->linknum; ++j ) {
    if( j == indx ) continue;

    if( fabs( ptmp1->linkdirect[j] - ptmp1->linkdirect[indx]) < minphi ) {
      /* We are dealing with  a terminal snapback */

      is_snapback = 1;
      break;
    }
  }

  /* Set return conditions and exit */

  *pmiddle = ptmp3;

  return is_snapback;

}


void build_half_lines( pntDescript *ppntx, segmentList *segl ) {
  
  /* local */

  int i0, j0, k0;
  int segnum;
  pntDescript *ppnt0, *ppnt1, *ppnt2, *ppnts, *pnew0;
  float ma;

  int segsize1 = 50;
  int segincrement = 20;
  int arcsize0 = 100;
  int arcsize1 = 50;

  double ydelta0, ydelta1, xdelta0, xdelta1;
  double track_length0, track_length1;
  int sec_index;
  int iseg;

  int out[2];

  /* What is the snapback-angle at nodes? */
  if( procMinSubtend( GET_VAL, &ma ) != 0 ) {
    fprintf( stderr, "Could not retrieve snap angle at nodes\n" );
    exit(1);
  }

  /* Initialise tracking details */

  segnum = segl->numSegments;

  /* Ah don't know how big this is. Realloc anyway!! */
  while( segnum >= segsize1 )
    segsize1 += segincrement;
  segl->segments = (segmentDescript *)realloc( segl->segments, segsize1 * sizeof
					       (segmentDescript) );
  /* assert(ppntx->linknum == 2); */ /* Should be in the centre of an arc (2 links) */
  ppntx->duff = 1; /* Blank the starting point */

  if( ppntx->linknum > 2 ) {
    /* This is just a 1-way simple link, deal with it and return */

    ppntx->duff = 1; /* Don't repeat this in the reverse direction */

    /* This is most likely a triangular wedge, with this the long edge.
       Whatever it is we're probably better without it. Return.
    */

    return;

  }

  for( i0 = 0; i0 < 2; ++i0 ) {
    /* Track each way */
    ppnt0 = ppntx;
    ppnt1 = ppnt0->linkverts[i0];
    if(ppnt1->linknum != 2) {
      /* Simple link. Deal with this as a terminal snapback */

      /* Is this short or long? */

      /* Find length of primary track */
      ydelta0 = ppnt0->yPosn - ppnt1->yPosn;
      xdelta0 = ppnt0->xPosn - ppnt1->xPosn;
      track_length0 = sqrt( (ydelta0 * ydelta0) + (xdelta0 * xdelta0) );

      /* Find length of secondary track */
      /* Get back-index of starting point */
      sec_index = -1;
      for( j0 = 0; j0 < ppnt1->linknum; ++j0 ) {
	if( ppnt1->linkverts[j0] = ppnt0 ) {
	  sec_index = j0;
	  break;
	}
      }

      /* Find the first link in the secondary track */
      ppnts = NULL;
      if(sec_index >= 0) {
	for( j0 = 0; j0 < ppnt1->linknum; ++j0 ) {
	  if( j0 == sec_index ) continue;
	  if( fabs(ppnt1->linkdirect[j0] - ppnt1->linkdirect[sec_index]) < ma ) {
	    ppnts = ppnt1->linkverts[j0];
	    break;
	  }
	}

	if(ppnts == NULL) break; {

	/* Unknown situation. Write the line and break. */
	  
	  extract_simple_link( segl, ppnt0, ppnt1 );
	  break;
	}

      /* Now calculate length */
      ydelta1 = ppnt1->yPosn - ppnts->yPosn;
      xdelta1 = ppnt1->xPosn - ppnts->xPosn;
      track_length1 = sqrt( (ydelta1 * ydelta1) + (xdelta1 * xdelta1) );
      }

      if( sec_index >= 0 && track_length0 > track_length1 ) {
	/* We have the long stick. Set the secondary track's first
	   offset as the terminal vertex (node)
	*/
	ppnt1 = ppnts;
      }
      /* else: this is the short stick. Do nothing */ 

    }

    /* Set first and second points [ actually stick in a third
       vertex in the middle to get round potential `third point'
       problems ]
    */
    iseg = segnum++;

    segl->segments[iseg-1].numVertices = 3;
    segl->segments[iseg-1].vertices = (pntDescript **)malloc( arcsize0 * 
							      sizeof(pntDescript *));
							      
    if(segl->segments[iseg-1].numVertices > arcsize0) {
      arcsize0 += arcsize1;
      segl->segments[iseg-1].vertices = 
	(pntDescript **)realloc( segl->segments[iseg-1].vertices, 
				 arcsize0 * sizeof(pntDescript *) );		
    }

    segl->segments[iseg-1].vertices[0] = ppnt0;
    segl->segments[iseg-1].vertices[2] = ppnt1;
    /* ppnt1->duff = 1; */

    /* Deal with the middle point */
    pnew0 = (pntDescript *)malloc( sizeof(pntDescript));
    
    /* fill in appropriate fields. Ignore unnecessary */
    pnew0->duff = 0;
    pnew0->isnode = 0;
    pnew0->xPosn = (ppnt0->xPosn + ppnt1->xPosn) / 2.0;
    pnew0->yPosn = (ppnt0->yPosn + ppnt1->yPosn) / 2.0;
    pnew0->zVal = 0.0;
    pnew0->mVal = 0.0;
    pnew0->linkverts = NULL;
    pnew0->linknum = 0;
    pnew0->linkdirect =  NULL;

    /* Register the new vertex */
    segl->segments[iseg-1].vertices[1] = pnew0;
    

    /* Now track on to further vertices if present */
    while( ppnt1->linknum == 2) { /* While the `next' vertex is not a node */
      /* Get the next link */

      if(ppnt1->linkverts[0] == ppnt0) {
	ppnt2 = ppnt0;
	ppnt0 = ppnt1;
	ppnt1 = ppnt1->linkverts[1];
      }
      else
	{
	  ppnt2 = ppnt0;
	  ppnt0 = ppnt1;
	  ppnt1 = ppnt1->linkverts[0];
	}

      /* If this is not a node we register the point and go on */
      if(ppnt1->linknum == 2) {
	segl->segments[iseg-1].numVertices++;
	if(segl->segments[iseg-1].numVertices > arcsize0) {
	  arcsize0 += arcsize1;
	  segl->segments[iseg-1].vertices = 
	    (pntDescript **)realloc( segl->segments[iseg-1].vertices, 
				     arcsize0 * sizeof(pntDescript *) );		
	}
	segl->segments[iseg-1].vertices[segl->segments[iseg-1].numVertices-1] = ppnt1;
	ppnt1->duff = 1;
      }
      else {
	/* The next link is a node, so we must deal with the snapback issue */

	/* Is this short or long? */

	/* Find length of primary track */
	ydelta0 = ppnt0->yPosn - ppnt1->yPosn;
	xdelta0 = ppnt0->xPosn - ppnt1->xPosn;
	track_length0 = sqrt( (ydelta0 * ydelta0) + (xdelta0 * xdelta0) );

	/* Find length of secondary track */
	/* Get back-index of starting point */
	for( j0 = 0; j0 < ppnt1->linknum; ++j0 ) {
	  if( ppnt1->linkverts[j0] = ppnt0 ) {
	    sec_index = j0;
	    break;
	  }
	}

	/* Find the first link in the secondary track */
	ppnts = NULL;
	for( j0 = 0; j0 < ppnt1->linknum; ++j0 ) {
	  if( j0 == sec_index ) continue;
	  if( fabs(ppnt1->linkdirect[j0] - ppnt1->linkdirect[sec_index]) < ma ) {
	    ppnts = ppnt1->linkverts[j0];
	    break;
	  }
	}

	/* Now calculate length */
	if(ppnts != NULL) {
	  ydelta1 = ppnt1->yPosn - ppnts->yPosn;
	  xdelta1 = ppnt1->xPosn - ppnts->xPosn;
	  track_length1 = sqrt( (ydelta1 * ydelta1) + (xdelta1 * xdelta1) );
	}

	if( ppnts != NULL && track_length0 > track_length1 ) {
	  /* We have the long stick. Set the secondary track's first
	     offset as the terminal vertex (node)
	  */
	  ppnt1 = ppnts;
	}
	else if( ppnts == NULL ) {
	  /* There is no snap-back (??), just go on */
	}
	else {
	  /* This is the short stick. Build a new link from the end-point
	     to the secondary offset, and then break, as we have already
	     registered the last point in this link. It is now ppnt0
	  */

	  /* Build linking arc */
	  extract_simple_link(segl, ppnt0, ppnt1);

	  /* and break */
	  break;
	  
	}

	/* Register the point */
	segl->segments[iseg-1].numVertices++;
	if(segl->segments[iseg-1].numVertices > arcsize0) {
	  arcsize0 += arcsize1;
	  segl->segments[iseg-1].vertices = 
	    (pntDescript **)realloc( segl->segments[iseg-1].vertices, 
				     arcsize0 * sizeof(pntDescript *) );		
	}
	segl->segments[iseg-1].vertices[segl->segments[iseg-1].numVertices-1] = ppnt1;	

	/* This is the last vertex in this arc */
	break; /* We have relinked the vertex, but not resynced it's fields, so won't
		  necessarily break automatically */

      }
	
    
    }
    
  }
  
}



void extract_simple_link( segmentList *seg1, pntDescript *pt1, pntDescript *pt2 ) {

  /* Extract a simple link from a v-base and add it to a segment list,
     adding a third intermediate point
  */

  /* local variables */

  int arcs0 = 50, arcs1 = 20; /* Initial and incremental sizes of allocation for list */
  pntDescript *pnew;
  int segid;

  
  /* Create a new segment in the list */
  seg1->numSegments++;
  while( seg1->numSegments >= arcs0 )
    arcs0 += arcs1;
  seg1->segments = (segmentDescript *)realloc( seg1->segments, arcs0 * sizeof
					       (segmentDescript) );

  segid = seg1->numSegments - 1;

  seg1->segments[segid].numVertices = 3;
  seg1->segments[segid].vertices = (pntDescript **)malloc( 3 * sizeof(pntDescript *) );

  /* Add the endpoints */

  seg1->segments[segid].vertices[0] = pt1;
  seg1->segments[segid].vertices[2] = pt2;

  /* Add the third point */

  pnew = (pntDescript *)malloc( sizeof(pntDescript));
  pnew->duff = 1;
  pnew->isnode = 0;
  pnew->xPosn = (pt1->xPosn + pt2->xPosn) / 2.0;
  pnew->yPosn = (pt1->yPosn + pt2->yPosn) / 2.0;
  pnew->zVal = 0.0;
  pnew->mVal = 0.0;
  pnew->linkverts = NULL;
  pnew->linknum = 0;
  pnew->linkdirect = NULL;

  seg1->segments[segid].vertices[1] = pnew;
  

  
}
