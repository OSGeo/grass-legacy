/******************************************************************************
 * polygon.h
 * Declarations structure for holding polygon

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 27th of January 2002
 * Last updated 19th of February 2002
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

Some Relevant Topological Definitions
_____________________________________


1) The Universal Set of a Coverage is the set of points contained within a
   rectangular interval of the Cartesian Set R x R, where each co-ordinate is
   a projection of one dimension onto the real line. The Coverage is a closed
   subset of R x R in this sense, and the projected intervals may be separately
   unlimited, determined by the projection, or set by other arbitrary means.

2) The Coverage is then disconnected in the following sense: 

   a) the points of the Coverage are distributed among a class of component
      subsets, such that each point is an interior point of a component subset,
      or is a boundary point of such a subset, thus a member of the Boundary Set,
      where

   b) a boundary point is a point, associated with a given component subset, 
      all of whose neighbourhoods contain both points that are interior points
      of the given component, and points that are members of its complement
      (relative to the Coverage).

   c) a component subset of the Interior is understood as a `component' in the strict
      topological sense that 

      i) it is a connected subset of the Coverage;

      ii) it is maximal in the sense that, given any interior point of the
          Component, any connected subset that contains that point
	  (neighbourhood of the point) is necessarily a subset of the Component.
	  If it is not a proper subset of the Component, it is equal 
	  to the Component. Each such Component is the union of its interior
	  and boundary, and thus is closed;

      iii) the class of Components is finite;

      iv) there exists no Component with empty interior.

   d) The union of the Components is equal to the Coverage. The intersection
      of the Components is equal to the Boundary Set.

3) The union of the boundary points associated with a Component, constitutes the
   Boundary of the Component. A Boundary consists of the union of one or more
   disjoint subsets of the Boundary Set.

4) A Component formed by means of the above-described disconnection of the
   Coverage, constitutes a Polygon.

   The above are definitions assuming the Coverage to be a general topological 
   space, apart from the specification that it is a subset of R x R. However, we
   must specify the space more precisely now to expand on the above, so we now
   assume the general properties of a 2-dimensional Euclidian space in Cartesian
   co-ordinates as implicit in what follows:

5) The Edge Set for a given Polygon may be defined as the the subset of the 
   Boundary Set, whose points are associated with the Polygon, such that each
   point that is contained in the Edge Set has neighbourhoods, each of which
   contains points that are interior points of the Polygon.

6) Earlier, the disjoint subsets of a Polygon Boundary were referred to
   without being further described. We delimit these subsets now as follows:

   a) The subsets of the Coverage properly contained in the complement of
      the Coverage are union'd to form the Polygon Exterior. Alternatively
      the Boundary can be thought of as generating a new and simpler disconnection
      of the Coverage into the Exterior and Interior relative to a particular 
      polygon.

   b) For a point B(0) in the Boundary, choose a neighbourhood which contains
      points that are interior points of two different Component subsets of
      the Coverage. (A small enough neighbourhood can always be chosen so that
      this is so.)

   c) For each such neighbourhood choose points P(1) and P(2), such that each is
      contained in an open connected subset of the enclosing Component.

   d) Form a series of open subsets, K(1) to K(N), which are the Interiors of the
      components of the disconnection induced by the polygon in isolation.

   e) For each of P(1) and P(2), define C(1) and C(2) respectively, the containing
      Components. For each of C(1) and C(2), form the intersection with each of the 
      K(j). The result of each intersect is either the empty set, or the C(i) will be equal
      to the K(j). For if C(i) intersected K(j), but was not equal to it, then there would
      be points in the C(i) not in the K(j), vice versa or both. It is easy to see that in all
      cases this violates the assumption that the C's and K's are maximal open sets
      within their respective disconnections.

   f) As each edge is the boundary between two components of the polygon-induced
      disconnection, it is easy to see that one C(i) (and K(j)) must be the Interior, and
      the other one of the components of the Exterior. For if the Interior is not
      represented this violates the assumption that the disconnection is induced
      by the polygon. 

   g) Form a partition of the Edge Set whose members are subsets associated with
      one of the possible index pairs (j,k) described in (f) above. Now assign B(0) to
      the appropriate subset of the Edge Set L[j,k).  It is clear the possible index
      pairs are those for which one value is fixed, corresponding to the Interior
      Component, and the other takes on each of the other values, corresponding to 
      1 or more components of the Exterior. So, there are N-1 subsets. Each subset
      is called an Edge.

   
      */

#ifndef POLYGON_H_
#define POLYGON_H_

#include "gis.h"
#include "Vect.h"
#include "category.h"

#define POLY_INITIALISED 1
#define POLY_WITH_VERTICES 2
#define POLY_WITH_POLARITY 4
#define POLY_WITH_CENTROID 8
#define POLY_WITH_Z 16
#define POLY_WITH_CATS 32
#define POLY_INCONSISTENT 64

typedef struct polygon_struct_ {

  char struct_status;

  int layer_0;

  int n_hulls;
  int alloc_hulls;
  int n_holes;
  int alloc_holes;
  int n_tmp_holes;
  int alloc_tmp_holes;

  struct line_pnts *hulls;
  struct line_pnts *holes;

  int *hole_idxs;
  int *holes_per_hull;

  double centroid_x;
  double centroid_y;

  struct line_pnts *tmp_holes;

  category_ctrl  *categories_0;
} polygon_ctrl;


typedef struct simple_ring_set_ {

  int n_rings;
  int alloc_rings;
  struct line_pnts **rings;

} ringset;



/* Prototypes */

polygon_ctrl *polygon_ctrl_init(void);
void polygon_ctrl_destroy(polygon_ctrl *);
int polygon_ctrl_append_ring(polygon_ctrl *, struct line_pnts *);
int polygon_ctrl_allocate_hulls(polygon_ctrl *, const int);
int polygon_ctrl_allocate_holes(polygon_ctrl *, const int);
int polygon_ctrl_allocate_tmp_holes(polygon_ctrl *, const int);
ringset *polygon_holes_to_ringset(polygon_ctrl *, const int, int *);
int ring_mbr1_in_mbr2(struct line_pnts *, struct line_pnts *);
int ring_point_is_inside(struct line_pnts *, const double,
			 const double, int *);
int ring_is_closed(struct line_pnts *);
double ring_angle_at_segment(struct line_pnts *, const double,
			     const double, const int);
int get_ring_circulation(struct line_pnts *, const double, const double);
void get_ring_mbr(struct line_pnts *, double *, double *, double *, double *);
int get_ring_intrinsic_circulation(struct line_pnts *, int *);
void polygon_ctrl_dump(FILE *, polygon_ctrl *, const int);
ringset *ringset_init(int);
void ringset_destroy(ringset *);

#endif /* POLYGON_H_  */

