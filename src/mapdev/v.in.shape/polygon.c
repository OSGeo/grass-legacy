/******************************************************************************
 * polygon.c
 * Definitions structure for holding polygon

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 27th Jan 2002
 * Last updated 9th Mar 2002
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

/*

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "polygon.h"
#include "category.h"
#include "vmap_import.h"
#include "gis.h"
#include "Vect.h"

#define REAL_EPSILON 1.0e-6
#define POLYGON_RING_BLOCKSIZE 40
#define POLYGON_RING_INCR_BLOCKSIZE 10

/* To set an angle to the range ( -pi < THETA <= +pi )  */
#define Correct_Angle(A) while((A)>=M_PI||(A)<(-M_PI)) {if((A)>=M_PI){(A)-=2*M_PI;} \
else if((A)<(-M_PI)){(A)+=2*M_PI;}}

static double phi_old = 0.0;
static int check_paranoid = 0;

static int polygon_init_alloc_blocksize(int ix);
static int polygon_next_alloc_blocksize(int ix, int jx);

polygon_ctrl *polygon_ctrl_init(void) {

  polygon_ctrl *pgc1;

  pgc1 = (polygon_ctrl *)malloc( sizeof(polygon_ctrl) );
  if(pgc1 == NULL)
    return NULL;

  pgc1->layer_0 = 0;
  pgc1->n_hulls = 0;
  pgc1->alloc_hulls = 0;
  pgc1->n_holes = 0;
  pgc1->alloc_holes = 0;
  pgc1->n_tmp_holes = 0;
  pgc1->alloc_tmp_holes = 0;

  pgc1->centroid_x = 1.0e+99;
  pgc1->centroid_y = 1.0e+99;

  pgc1->categories_0 = NULL;
  pgc1->struct_status = POLY_INITIALISED;

  return pgc1;
}

void polygon_ctrl_destroy(polygon_ctrl * pgc0) {

  struct line_pnts *lp1;
  int ja; /* loop */

  if(pgc0->alloc_hulls > 0) {
    free(pgc0->holes_per_hull);
    for(ja = 0; ja < pgc0->n_hulls; ja++) {
      if(pgc0->hulls[ja].alloc_points > 0) {
	free(pgc0->hulls[ja].x);
	free(pgc0->hulls[ja].y);
      }
    }
    free(pgc0->hulls);
  }

  if(pgc0->alloc_holes > 0) {
    free(pgc0->hole_idxs);
    for(ja = 0; ja < pgc0->n_holes; ja++) {
      if(pgc0->holes[ja].alloc_points > 0) {
	free(pgc0->holes[ja].x);
	free(pgc0->holes[ja].y);
      }
    }
    free(pgc0->holes);
  }

  if(pgc0->alloc_tmp_holes > 0) {
    for(ja = 0; ja < pgc0->n_tmp_holes; ja++) {
      if(pgc0->tmp_holes[ja].alloc_points > 0) {
	free(pgc0->tmp_holes[ja].x);
	free(pgc0->tmp_holes[ja].y);
      }
    }
    free(pgc0->tmp_holes);
  }

  if(pgc0->struct_status & POLY_WITH_CATS)
    category_ctrl_destroy(pgc0->categories_0);

  free(pgc0);
}


int polygon_ctrl_append_ring(polygon_ctrl *pgc0, struct line_pnts *lpt0) {

  /* Return values */
  /* 1  : Success, added
     -1 : Some unspecified error
     -2 : Ring has degenerate line
     -3 : Ring not closed
  */

  int icirc1, res, res1, circ_dummy, k, ma;
  int ia;  /* loop */

  double *x_copy, *y_copy;
  
  if(! ring_is_closed(lpt0))
    return(-1);

  /* What sort of ring is it */

  res = get_ring_intrinsic_circulation(lpt0, &icirc1);

  if(res < 0)  /* Ring bad in some way */
    return (res);

  if(icirc1 == -1) {
    /* Hull */
    polygon_ctrl_allocate_hulls(pgc0, pgc0->n_hulls + 1);
    memcpy(&pgc0->hulls[pgc0->n_hulls++], lpt0, sizeof(struct line_pnts));
    x_copy = (double *) malloc( sizeof(double) *
				pgc0->hulls[pgc0->n_hulls - 1].alloc_points );
    if(x_copy == NULL)
      return (-1);
    memcpy(x_copy, pgc0->hulls[pgc0->n_hulls - 1].x,
	   pgc0->hulls[pgc0->n_hulls - 1].n_points * sizeof(double));
    pgc0->hulls[pgc0->n_hulls - 1].x = x_copy;
    y_copy = (double *) malloc( sizeof(double) *
				pgc0->hulls[pgc0->n_hulls - 1].alloc_points );
    if(y_copy == NULL)
      return (-1);
    memcpy(y_copy, pgc0->hulls[pgc0->n_hulls - 1].y,
	   pgc0->hulls[pgc0->n_hulls - 1].n_points * sizeof(double));
    pgc0->hulls[pgc0->n_hulls - 1].y = y_copy;

    pgc0->holes_per_hull[pgc0->n_hulls - 1] = 0;

    /* If there are dangling holes, can they now be assigned to this hull? */

    if(pgc0->n_tmp_holes > 0) {
      k = 0;
      ma = pgc0->n_tmp_holes;
      for(ia = 0; ia < ma; ia++) {
	res1 = ring_point_is_inside(&pgc0->hulls[pgc0->n_hulls - 1],
				    pgc0->tmp_holes[k].x[1],
				    pgc0->tmp_holes[k].y[1],
				    &circ_dummy);
	if(res1) {
	  if(res1 < 0) {
	    return(-1);
	  }
	  else {
	    /* Move the hole to the real holes' location
	       and assign to the current hull
	    */

	    polygon_ctrl_allocate_holes(pgc0, pgc0->n_holes + 1);
	    memcpy(&pgc0->holes[pgc0->n_holes++], &pgc0->tmp_holes[k],
		   sizeof(struct line_pnts));
	    pgc0->hole_idxs[pgc0->n_holes - 1] = pgc0->n_hulls - 1;
	    pgc0->holes_per_hull[pgc0->n_hulls - 1]++;

	    /* Move the remaining holes down one. */
	    if(ia < ma - 1) {
	      memmove(&pgc0->tmp_holes[k], &pgc0->tmp_holes[k + 1],
		      sizeof(struct line_pnts) * (ma - ia - 1));
	    }
	    pgc0->n_tmp_holes--;
	  }
	}

	else k++;
      } /* for ia ... */
    }
  }

  else if (icirc1 == 1) {
    /* Hole */

    /* Check each existing hull: if there are none or the hull is not
       the parent, add to the temp holes' list
    */

    int parent = 0;
    int parent_idx = 0;

    if(pgc0->n_hulls > 0) {
      for(ia = 0; ia < pgc0->n_hulls; ia++) {
	res1 = ring_point_is_inside(&pgc0->hulls[ia], lpt0->x[1],
				    lpt0->y[1], &circ_dummy);

	if(res1 < 0) {
	  return(-1);
	}

	else if(res1 == 1) {
	  parent = 1;
	  parent_idx = ia;
	  break;
	}
      }
    }

    if(parent) {
      polygon_ctrl_allocate_holes(pgc0, pgc0->n_holes + 1);
      memcpy(&pgc0->holes[pgc0->n_holes++], lpt0, sizeof(struct line_pnts));
      x_copy = (double *) malloc( sizeof(double) *
				  pgc0->holes[pgc0->n_holes - 1].alloc_points );
      if(x_copy == NULL)
	return (-1);
      memcpy(x_copy, pgc0->holes[pgc0->n_holes - 1].x,
	     pgc0->holes[pgc0->n_holes - 1].n_points * sizeof(double));
      pgc0->holes[pgc0->n_holes - 1].x = x_copy;
      y_copy = (double *) malloc( sizeof(double) *
				  pgc0->holes[pgc0->n_holes - 1].alloc_points );
      if(y_copy == NULL)
	return (-1);
      memcpy(y_copy, pgc0->holes[pgc0->n_holes - 1].y,
	     pgc0->holes[pgc0->n_holes - 1].n_points * sizeof(double));
      pgc0->holes[pgc0->n_holes - 1].y = y_copy;

      pgc0->holes_per_hull[parent_idx]++;
      pgc0->hole_idxs[pgc0->n_holes - 1] = parent_idx;
    }

    else {
      polygon_ctrl_allocate_tmp_holes(pgc0, pgc0->n_tmp_holes + 1);
      memcpy(&pgc0->tmp_holes[pgc0->n_tmp_holes++], lpt0, sizeof(struct line_pnts));

      x_copy = (double *) malloc( sizeof(double) *
				  pgc0->tmp_holes[pgc0->n_tmp_holes - 1].alloc_points );
      if(x_copy == NULL)
	return (-1);
      memcpy(x_copy, pgc0->tmp_holes[pgc0->n_tmp_holes - 1].x,
	     pgc0->tmp_holes[pgc0->n_tmp_holes - 1].n_points * sizeof(double));
      pgc0->tmp_holes[pgc0->n_tmp_holes - 1].x = x_copy;
      y_copy = (double *) malloc( sizeof(double) *
				  pgc0->tmp_holes[pgc0->n_tmp_holes - 1].alloc_points );
      if(y_copy == NULL)
	return (-1);
      memcpy(y_copy, pgc0->tmp_holes[pgc0->n_tmp_holes - 1].y,
	     pgc0->tmp_holes[pgc0->n_tmp_holes - 1].n_points * sizeof(double));
      pgc0->tmp_holes[pgc0->n_tmp_holes - 1].y = y_copy;

    }
  }

  pgc0->struct_status |= POLY_WITH_VERTICES;
  if(pgc0->n_tmp_holes > 0) {
    pgc0->struct_status |= POLY_INCONSISTENT;
  }
  else {
    pgc0->struct_status &= ~(POLY_INCONSISTENT);
  }

  return(1);
}

int polygon_ctrl_allocate_hulls(polygon_ctrl * pgc0, const int nh) {

  int num1;
  struct line_pnts *lpt1;
  int *iptr = NULL;

  if(pgc0->alloc_hulls == 0) {
    /* Allocate for the first time */

    num1 = polygon_init_alloc_blocksize(nh);

    lpt1 = (struct line_pnts *) malloc( num1 * sizeof(struct line_pnts) );
    iptr = (int *) malloc( num1 * sizeof(int) );

  }

  else {
    
    num1 = polygon_next_alloc_blocksize(nh, pgc0->alloc_hulls);
    
    lpt1 = (struct line_pnts *) realloc( pgc0->hulls, num1 * sizeof(struct line_pnts) );
    iptr = (int *) realloc(pgc0->holes_per_hull, num1 * sizeof(int) );

  }

  if(lpt1 == NULL || iptr == NULL)
    return (-1);

  pgc0->hulls = lpt1;
  pgc0->holes_per_hull = iptr;
  pgc0->alloc_hulls = num1;

  return (0);


    
}

int polygon_ctrl_allocate_holes(polygon_ctrl * pgc0, const int nh) {

  int num1;
  struct line_pnts *lpt1;
  int *iptr;

  if(pgc0->alloc_holes == 0) {
    /* Allocate for the first time */

    num1 = polygon_init_alloc_blocksize(nh);

    lpt1 = (struct line_pnts *) malloc( num1 * sizeof(struct line_pnts) );
    iptr = (int *) malloc( num1 * sizeof(int) );

  }

  else {
    
    num1 = polygon_next_alloc_blocksize(nh, pgc0->alloc_holes);
    
    lpt1 = (struct line_pnts *) realloc( pgc0->holes, num1 * sizeof(struct line_pnts) );
    iptr = (int *) realloc(pgc0->hole_idxs, num1 * sizeof(int) );

  }

  if(lpt1 == NULL  || iptr == NULL)
    return (-1);

  pgc0->holes = lpt1;
  pgc0->hole_idxs = iptr;
  pgc0->alloc_holes = num1;

  return (0);

    
}


int polygon_ctrl_allocate_tmp_holes(polygon_ctrl * pgc0, const int nh) {

  int num1;
  struct line_pnts *lpt1;

  if(pgc0->alloc_tmp_holes == 0) {
    /* Allocate for the first time */

    num1 = polygon_init_alloc_blocksize(nh);

    lpt1 = (struct line_pnts *) malloc( num1 * sizeof(struct line_pnts) );

  }

  else {
    
    num1 = polygon_next_alloc_blocksize(nh, pgc0->alloc_tmp_holes);
    
    lpt1 = (struct line_pnts *) realloc( pgc0->tmp_holes, num1 * sizeof(struct line_pnts) );

  }

  if(lpt1 == NULL)
    return (-1);

  pgc0->tmp_holes = lpt1;
  pgc0->alloc_tmp_holes = num1;

  return (0);

    
}

int ring_mbr1_in_mbr2(struct line_pnts *lpt0a, struct line_pnts *lpt0b) {

  /* Note: it is not efficient ot use this for constructing polygons
     because the `contains' attribute will usually be true anyway
     in most coverages, so the test for `point-in-poly' would have
     to be applied - thus this is just unnecessary over-head.
  */
  
  double xa1, xa2, xb1, xb2, ya1, ya2, yb1, yb2;

  get_ring_mbr(lpt0a, &xa1, &xb1, &ya1, &yb1);
  get_ring_mbr(lpt0b, &xa2, &xb2, &ya2, &yb2);

  if(xa1 >= xa2 && xb1 <= xb2 && ya1 >= ya2 && yb1 <= yb2)
    return(1);
  else
    return(0);
  
}


int ring_point_is_inside(struct line_pnts *lpt0, const double x,
			 const double y, int *circ0) {

  /* Return 1 if point is inside, and 0 if point is outside 
     Return -1 if value cannot be interpreted properly.
     In event of a return of 1, also put the circulation
     +/-1 in the field `circ0'
  */

  int res = get_ring_circulation(lpt0, x, y);

  if(res == 0)
    return(0);
  else if(res == 1) {
    *circ0 = 1;
    return(1);
  }
  else if(res == -1) {
    *circ0 = -1;
    return(1);
  }
  else return(-1);
}


int ring_is_closed(struct line_pnts *lpt0) {

  if(lpt0->x[0] == lpt0->x[lpt0->n_points - 1] &&
     lpt0->y[0] == lpt0->y[lpt0->n_points - 1])
    return(1);
  else
    return(0);
}

double ring_angle_at_segment(struct line_pnts *lpt0, const double x,
			     const double y, const int k) {

  double xa, xb, ya, yb, phi1, phi2, delta;

  if(k < 0 || k > lpt0->n_points - 2) 
    return(-9.0);

  if(k == 0) {
    xa = lpt0->x[k];
    ya = lpt0->y[k];
  }

  xb = lpt0->x[k + 1];
  yb = lpt0->y[k + 1];

  if(k == 0) {
    phi1 = atan2(ya - y, xa - x);
  }

  else phi1 = phi_old;

  phi2 = atan2(yb - y, xb - x);

  phi_old = phi2;
  delta = phi2 - phi1;
  Correct_Angle(delta);
  return(delta);
}


int get_ring_circulation(struct line_pnts *lpt0, const double xa, const double ya) {

  /* return -1, 0 or +1 for successful value,
     return -2 if the ring is not closed (to fp precision)
     return -3 if value is wierd
  */

  /* loop */
  int ia;
  double alpha = 0.0;

  if(! ring_is_closed(lpt0))
    return (-2);

  for(ia = 0; ia < lpt0->n_points - 1; ia++)
    alpha += ring_angle_at_segment(lpt0, xa, ya, ia);

  if(fabs(alpha - 2 * M_PI) < REAL_EPSILON)
    return(1);
  else if(fabs(alpha + 2 * M_PI) < REAL_EPSILON)
    return(-1);
  else if(fabs(alpha) < REAL_EPSILON)
    return 0;
  else return(-3);
}


void get_ring_mbr(struct line_pnts *lpt0, double *west, double *east,
		  double *south, double *north) {

  double wa = 1.0e+99, ea = 1.0e-99, sa = 1.0e+99, na = 1.0e-99;

  /* loop */
  int ia;

  for(ia = 0; ia < lpt0->n_points; ia++) { /* We don't care if the line is open for this */
    if(lpt0->x[ia] < wa) wa = lpt0->x[ia];
    if(lpt0->x[ia] > ea) ea = lpt0->x[ia];
    if(lpt0->y[ia] < sa) sa = lpt0->y[ia];
    if(lpt0->y[ia] > na) na = lpt0->y[ia];
  }

  *west = wa; *east = ea; *south = sa; *north = na;

}


int get_ring_intrinsic_circulation(struct line_pnts *lpt0, int *icirc0) {

  int ia; /* loop */

  double xs, ys;

  double alpha = 0.0;
  
  if(! ring_is_closed(lpt0)) 
    return (-1);

  if(lpt0->n_points == 1)
    return (-3);  /* weird value */

  else if(lpt0->n_points == 3)
    return(-2);   /* degenerate */

  else if(lpt0->n_points > 3)  {

    xs = (lpt0->x[0] + lpt0->x[1]) / 2.0;
    ys = (lpt0->y[0] + lpt0->y[1]) / 2.0;

    phi_old = atan2(lpt0->y[1] - ys, lpt0->x[1] - xs);

    for(ia = 1; ia < lpt0->n_points - 1; ia++) {
      alpha += ring_angle_at_segment(lpt0, xs, ys, ia);
    }

    if(fabs(alpha - M_PI) < REAL_EPSILON) {
      *icirc0 = 1;
      return(1);
    }
    else if(fabs(alpha + M_PI) < REAL_EPSILON) {
      *icirc0 = -1;
      return(1);
    }

    /* Something went wrong */
    return (-1);
  }

  else /* ? */
    return(-1);

}
  
ringset *polygon_holes_to_ringset(polygon_ctrl *pgc0, const int hull_idx, int *err_code0) {

  /* Put the list of holes for a given hull into a ringset */

  int num_holes, hole_cnt = 0;
  int ia;   /* loop */

  ringset *rs1;

  if(hull_idx < 0 || hull_idx > pgc0->n_hulls - 1) {
    /* Couldn't create ringset because the hull index is out of range */

    *err_code0 = 1;
    return (NULL);
  }
  
  num_holes = pgc0->holes_per_hull[hull_idx];

  if(num_holes == 0) {
    /* Couldn't create ringset because hull has no holes */
    *err_code0 = 0;
    return (NULL);
  }

  else {

    rs1 = ringset_init(num_holes);
    if(rs1 == NULL) {
      /* Memory allocation error */
      *err_code0 = -1;
      return (NULL);
    }
  }

  for(ia = 0; ia < pgc0->n_holes; ia++) {

    if(pgc0->hole_idxs[ia] == hull_idx) {
      
      if(++hole_cnt > num_holes) {
	/* Format error in polygon controller. Error code 2 */
	*err_code0 = 2;
	return (NULL);
      }

      rs1->rings[hole_cnt - 1] = &pgc0->holes[ia];
    }

  }

  if(hole_cnt != num_holes) {
    *err_code0 = 2;
    ringset_destroy(rs1);
    return (NULL);
  }

  else {

    *err_code0 = 0;  /* Success */
    return rs1;
  }
}

void polygon_ctrl_dump(FILE *lf0, polygon_ctrl *pgc0, const int label0) {

  /* loop */
  int ia, ib;

  int hole_idx1;

  fprintf(lf0, "\n++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n");

  fprintf(lf0, "      POLYGON (SHAPE) NUMBER  %d\n\n", label0);
  fprintf(lf0, "  Polygon contains %d outer rings and a total of %d holes.\n",
	  pgc0->n_hulls, pgc0->n_holes);
  if(pgc0->struct_status & POLY_INCONSISTENT)
    fprintf(lf0, "  The structure is in an inconsistent state with %d dangling holes.\n",
	    pgc0->n_tmp_holes);
  if(pgc0->struct_status & POLY_WITH_CATS)
    fprintf(lf0, "  Polygon has attributes provided.\n");
  if(pgc0->struct_status & POLY_WITH_CENTROID)
    fprintf(lf0, "  An area point is supplied at %16.6lf | %16.6lf.\n",
	    pgc0->centroid_x, pgc0->centroid_y);
  else 
    fprintf(lf0, "  Area point is not provided.\n",
	    pgc0->centroid_x, pgc0->centroid_y);
  if(pgc0->struct_status & POLY_WITH_Z)
    fprintf(lf0, "  Height co-ordinates are supplied for the vertices.\n",
	    pgc0->centroid_x, pgc0->centroid_y);
  else 
    fprintf(lf0, "  No height co-ordinates are supplied for the vertices.\n",
	    pgc0->centroid_x, pgc0->centroid_y);

  for(ia = 0; ia < pgc0->n_hulls; ia++) {
    fprintf(lf0, "\n    Main ring %d has %d holes ", ia + 1, pgc0->holes_per_hull[ia]);
    if(pgc0->holes_per_hull[ia] > 0)
      fprintf(lf0, "which are (not necessarily corresponding to Parts of shapefile):\n      ");
    for(ib = 0; ib < pgc0->n_holes; ib++) {
      hole_idx1 = pgc0->hole_idxs[ib];
      if(hole_idx1 == ia) {
	fprintf(lf0, "%d ", ib + 1);
      }
    }
    fprintf(lf0, "\n");
  }

  fprintf(lf0, "\n++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n");
}


/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

ringset *ringset_init(int nr0) {

  ringset *rs1;

  if(nr0 < 0)
    return (NULL);

  rs1 = (ringset *) malloc( sizeof(ringset) );
  if(rs1 == NULL)
    return (NULL);

  if( nr0 == 0 ) {
    rs1->alloc_rings = 0;
    rs1->n_rings = 0;
  }

  else {

    rs1->rings = (struct line_pnts **) malloc( nr0 * sizeof(struct line_pnts *) );
    if(rs1->rings == NULL) {
      free(rs1);
      return (NULL);
    }

    rs1->alloc_rings = rs1->n_rings = nr0;
    return rs1;
  }
}

void ringset_destroy(ringset *rs0) {

  if(rs0->alloc_rings > 0) 
    free(rs0->rings);

  free(rs0);

}


/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/* Static Routines  */

static int polygon_init_alloc_blocksize(int ix) {

  int iz;

  if(ix <= POLYGON_RING_BLOCKSIZE)
    return (POLYGON_RING_BLOCKSIZE);

  else {

    iz = ix / POLYGON_RING_BLOCKSIZE ;
    return ((iz + 1) * POLYGON_RING_BLOCKSIZE);
  }
    
}

static int polygon_next_alloc_blocksize(int ix, int jx) {

  int iz;

  if(jx >= ix)
    return jx;

  else {
    iz = ix / POLYGON_RING_INCR_BLOCKSIZE ;
    return ((iz + 1) * POLYGON_RING_INCR_BLOCKSIZE);
  }
    
}
