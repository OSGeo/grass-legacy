/******************************************************************************
 * cpoly.c [v.in.mif]
 * Build control structures for complex/compound polygon
 * shapes

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 9th. Jan. 2001
 * Last updated 9th. Jan. 2001
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
#include <math.h>
#include <assert.h>
#include "gis.h"
#include "Vect.h"
#include "cpoly.h"
#include "store.h"


/* Given a set of lines find the rings in these and the relationships
   between them and build this into a cpolygon structure
*/

int
G_build_cpolygons_struct(cpolygon **cp0, line_set *ls) {

  cpolygon *cp;

  int num_simple_lines;
  int i;       /* loop */

  double sn;  /* the snap distance */
  int circ;   /* Indicator of direction of circulation */

  num_simple_lines = ls->n_lines;

  cp = (cpolygon *)G_malloc( sizeof(cpolygon) );
  cp->hulls = (sring *)G_malloc( num_simple_lines * sizeof(sring) );
  cp->iset = (mring *)G_malloc( num_simple_lines * sizeof(mring) );
  cp->alloc_hulls = num_simple_lines;
  cp->holes = (sring *)G_malloc( num_simple_lines * sizeof(sring) );
  cp->alloc_holes = num_simple_lines;

  G_process_snap_distance( GET_VAL, &sn );

  cp->n_hulls = 0;
  cp->n_holes = 0;

  for( i = 0; i < num_simple_lines; i++ )
    memset(&cp->iset[i], 0, sizeof(mring) );

  for( i = 0; i < num_simple_lines; i++ ) {

    if(!G__verify_closed( &ls->lines[i], sn )) {
      fprintf(stderr, "Dataset contains unclosed ring. Aborting.\n");
      return -1;
    }

    circ = G__get_intrinsic_circulation(&ls->lines[i]);

    switch(circ) {

    case 0:
      {
	/* Improperly formed ring. Ignore it */
	break;
      }

    case 1:
      {
	/* Ring is circulated counter-clockwise. Is a hole */
	memcpy(&cp->holes[cp->n_holes++], &ls->lines[i], sizeof(sring));
      }

    case -1:
      {
	/* Ring is circulated clockwise. Is a hull */
	memcpy(&cp->hulls[cp->n_hulls++], &ls->lines[i], sizeof(sring));
      }
    }  /* END switch(circ) */
  }

  /* Condense arrays to elliminate blank slots */

  cp->holes = (sring *)G_realloc(cp->holes, cp->n_holes * sizeof(sring) );
  cp->alloc_holes = cp->n_holes;
  cp->hulls = (sring *)G_realloc(cp->hulls, cp->n_hulls * sizeof(sring) );
  cp->iset = (mring *)G_realloc(cp->iset, cp->n_hulls * sizeof(mring) );
  cp->alloc_hulls = cp->n_hulls;

  G__assign_isles(cp);

  *cp0 = cp;
  return 0;

}


/* Destroy a cpolygon */

void 
G_destroy_cpolygons_struct(cpolygon *cp1) {

  int i;

  if(cp1) {

    if(cp1->iset) {
      for( i = 0; i < cp1->n_hulls; i++ )
	if(cp1->iset[i].links) free(cp1->iset[i].links);
      free(cp1->iset);
    }

    if(cp1->hulls) free(cp1->hulls);
    if(cp1->holes) free(cp1->holes);

    free(cp1);
  }
  
}




/* Find an area point for the polygons */

int
G__mpolygon_centroid(cpolygon *cp, const int indx, double *x, double *y) {

  /* loop */
  int i;

  /* Set up polygon */

  double xa, ya;

  struct line_pnts **line0;
  struct line_pnts *line1;

  /* Structure to hold islands */
  line0 = (struct line_pnts **)G_malloc(cp->iset[indx].n_links 
					* sizeof(struct line_pnts *) );

  for( i = 0; i < cp->iset[indx].n_links; i++ )
    line0[i] = cp->holes[cp->iset[indx].links[i]].points;

  if( Vect_get_point_in_poly_isl(cp->hulls[indx].points, line0, 
				 cp->iset[indx].n_links, &xa, &ya) < 0 ) {
    G_free(line0);
    return -1;
  }

  if(x) 
    *x = xa;
  else {
    G_free(line0);
    return -1;
  }

  if(y)
    *y = ya;
  else {
    G_free(line0);
    return -1;
  }

  G_free(line0);
  return 0;

}

int
G__verify_closed(sring *sr, const double s1) {

  struct line_pnts *line1;
  double xa, xb, ya, yb;

  line1 = sr->points;

  xa = line1->x[0];
  ya = line1->y[0];
  xb = line1->x[line1->n_points - 1];
  yb = line1->y[line1->n_points - 1];

  if( hypot( xb - xa, yb - ya ) <= s1 ) {
    /* line is to be considered closed - but force close it */
    line1->x[line1->n_points - 1] = line1->x[0];
    line1->y[line1->n_points - 1] = line1->y[0];
    return 1;
  }
  
  else return 0;  /* Not closed */
}


int
G__get_intrinsic_circulation(sring *sr) {

  int i;

  struct line_pnts *line1;
  double s1;

  double delta, alpha, basex, basey;
  double phi0, phi1;

  line1 = sr->points;
  G_process_snap_distance( GET_VAL, &s1 );

  alpha = 0.0;
  basex = (line1->x[0] + line1->x[1]) / 2.0;
  basey = (line1->y[0] + line1->y[1]) / 2.0;
  phi0 = atan2(line1->y[1] - basey, line1->x[1] - basex);

  for( i = 2; i < line1->n_points; i++ ) {

    /* Skip if points are coincident, or almost so */
    if( hypot( line1->x[i] - line1->x[i - 1], line1->y[i] - line1->y[i - 1] ) <= s1 )
      continue;

    phi1 = atan2( line1->y[i] - basey, line1->x[i] - basex );
    delta = phi1 - phi0;

    if( delta < - M_PI ) delta += 2 * M_PI;
    else if( delta > M_PI ) delta -= 2 * M_PI;
    else if( fabs(delta) == M_PI ) return 0; /* Ring is snapped back. Abort. */

    alpha += delta;
    phi0 = phi1;
  }

  /* Value should be close to pi */

  if( fabs( fabs(alpha) - M_PI ) > 0.001 ) {
    /* Problem with ring. Circulation is an unusual value */
    return 0;
  }

  else if( alpha > 1.0 ) return 1; /* Positive circulation (counter-clockwise) */
  else return -1;                  /* Negative circulation (clockwise)         */
}


int
G__get_extrinsic_circulation(sring *sr, const double xpos, const double ypos) {

  /* Get the circulation of a ring about a given reference point */

  /* NOTE: Assume angles in radians are accurate to at least 2 dp. Ditto
     for radial (snap) distances
  */

  int i;

  struct line_pnts *line1;
  double s1;

  double delta, alpha;
  double phi0, phi1;

  line1 = sr->points;
  G_process_snap_distance( GET_VAL, &s1 );

  alpha = 0.0;
  phi0 = atan2(line1->y[0] - ypos, line1->x[0] - xpos);

  for( i = 1; i < line1->n_points; i++ ) {

    /* Return 2 if point coincides with reference point */

    if( hypot( line1->x[i] - xpos, line1->y[i] - ypos ) <= s1 )
      return 2;

    phi1 = atan2( line1->y[i] - ypos, line1->x[i] - xpos );
    delta = phi1 - phi0;

    if( fabs(fabs(delta) - M_PI) < 0.01 ) return 2; /* Too close to call */
    else if( delta < - M_PI ) delta += 2 * M_PI;
    else if( delta > M_PI ) delta -= 2 * M_PI;

    alpha += delta;
    phi0 = phi1;
  }

  /* Is this a full circulation about a central point? */

  if( fabs(fabs(alpha) - 2 * M_PI) < 0.01 ) {
    if(alpha > 0) return 1;
    else return -1;
  }

  /* ... or a full circulation about an exterior point? */

  if( fabs(alpha) < 0.01 )
    return 0;

  /* ... or something else */
  
  return 2;
}


int
G__assign_isles( cpolygon *cp1 ) {

  int i, j, k;  /* loop */
  int n_inclusions;  /* Count number of hulls included within another */
  twint *tw = NULL, tmp_tw;
  sring *h1, *h2;
  mring *currset;

  int do_continue;

  /* Check first if any hulls are contained within any other */

  if(cp1->n_hulls > 1) {
    n_inclusions = 0;
    for( i = 0; i < cp1->n_hulls; i++ ) {
      for( j = i + 1; j < cp1->n_hulls; j++ ) {
	h1 = &cp1->hulls[i]; 
	h2 = &cp1->hulls[j];
	if(G__bb1_contains_bb2(h1, h2)) {
	  if(G__ring1_contains_ring2(h1, h2)) {
	    tmp_tw.a = j;
	    tmp_tw.b = i;
	    if(n_inclusions == 0)
	      tw = (twint *)G_malloc( sizeof(twint) );
	    else
	      tw = (twint *)G_realloc( tw, (n_inclusions + 1) * sizeof(twint) );
	    memcpy(&tw[n_inclusions], &tmp_tw, sizeof(twint));
	    n_inclusions++;
	  }
	}
	else if(G__bb1_contains_bb2(h2, h1)) {
	  if(G__ring1_contains_ring2(h2, h1)) {
	    tmp_tw.a = i;
	    tmp_tw.b = j;
	    if(n_inclusions == 0)
	      tw = (twint *)G_malloc( sizeof(twint) );
	    else
	      tw = (twint *)G_realloc( tw, (n_inclusions + 1) * sizeof(twint) );
	    memcpy(&tw[n_inclusions], &tmp_tw, sizeof(twint));
	    n_inclusions++;
	  }
	}
      }
    }
  }

  /* loop through holes */

  for( i = 0; i < cp1->n_holes; i++ ) {

    /* Find which hull this is contained in */

    for( j = 0; j < cp1->n_hulls; j++ ) {
      
      if(G__bb1_contains_bb2(&cp1->hulls[j], &cp1->holes[i])) {
	if(G__ring1_contains_ring2(&cp1->hulls[j], &cp1->holes[i])) {
	  /* Hole is contained, but is this the immediate parent cell? */
	  if(cp1->n_hulls > 1 && n_inclusions > 0) {
	    do_continue = 0;
	    for( k = 0; k < n_inclusions; k++ ) {
	      if(tw[k].b == j) {
		if(G__bb1_contains_bb2(&cp1->hulls[tw[k].a], &cp1->holes[i])) {
		  if(G__ring1_contains_ring2(&cp1->hulls[tw[k].a], &cp1->holes[i])) {
		    do_continue =1; /* Inner hull is
				       a more immediate parent 
				       of current hole
				    */
		  }
		}
	      }
	    }
	  }

	  if( do_continue == 1 ) {
	    do_continue = 0;
	    continue;
	  }

	  /* OK! Hole is properly contained by hull which is immediate parent */
	  /* Add this hole to the hull's iset */

	  currset = &cp1->iset[j];

	  /* Make sure we have space to add the new link */
	  if(currset->alloc_links == 0) {
	    currset->links = (int *)G_malloc( 128 * sizeof(int) );
	    currset->alloc_links = 128;
	  }
	  else if( currset->n_links + 1 > currset->alloc_links ) {
	    currset->links = (int *)G_realloc(currset->links, (currset->n_links + 128)
					      * sizeof(int) );
	    currset->alloc_links += 128;
	  }

	  /* Add link */

	  currset->links[currset->n_links++] = i;
	}
      }
    }
  }

  if(tw) G_free(tw);
  return 0;
}


int
G__bb1_contains_bb2(sring *sr1, sring *sr2) {

  if( sr1->bbox.n > sr2->bbox.n && sr1->bbox.e > sr2->bbox.e &&
      sr1->bbox.s < sr2->bbox.s && sr1->bbox.w < sr2->bbox.w )
    return 1;
  else return 0;
}

int
G__ring1_contains_ring2(sring *sr1, sring *sr2) {

  /* It is assumed that the rings do not cross, and do not intersect,
     except possibly at discrete points, where both
     rings will have a vertex.
  */

  double basex, basey;
  int circ_chk;
  int pnt_cnt, incomplete;

  /* Get regerence point for inner ring. Pseudonode point of ring will do
     ie. first vertex
  */

  pnt_cnt = 0;

  while(pnt_cnt < sr2->points->n_points) {

    basex = sr2->points->x[pnt_cnt];
    basey = sr2->points->y[pnt_cnt++];

    circ_chk = G__get_extrinsic_circulation(sr1, basex, basey);

    switch(circ_chk) {

    case 2:
      {
	/* This is the indeterminate case, we must check
	   another point
	*/
	break;
      }
    case 0:
      {
	/* Point is outside. Ring1 does not contain ring2. Return 0. */
	return 0;
      }
    case -1:
    case 1:
      {
	/* Point is inside ring. Return 1 */
	return 1;
      }
    }

  }   /* End while */

  return 0; /* Shouldn't get here, but if we do, say no containment */
}
