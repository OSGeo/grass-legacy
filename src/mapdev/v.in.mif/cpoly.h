#ifndef CPOLY_H_
#define CPOLY_H_

#include "gis.h"
#include "Vect.h"

struct _cpolygon;
struct _line_set;
struct _sring;
struct _mring;
struct _region;
struct _twint;

typedef struct _cpolygon cpolygon;
typedef struct _line_set line_set;
typedef struct _sring sring;
typedef struct _mring mring;
typedef struct _region region;
typedef struct _twint twint;


struct _cpolygon {

  int n_hulls;
  int alloc_hulls;
  sring *hulls;
  mring *iset;
  
  int n_holes;
  int alloc_holes;
  sring *holes;
};


struct _line_set {

  int n_lines;
  int alloc_lines;
  sring *lines;
};

struct _region {

  double w, n, e, s;
};

struct _sring {

  int final; /* Boolean variable: is this a final ring (no islands)? */
  int parent[2];  /* A reference for the outer ring containing this:
		     may contain up to two values
		  */
  struct line_pnts *points;
  region bbox;
};


struct _mring {

  int n_links;
  int alloc_links;
  int *links;
};



struct _twint {
  int a, b;
};

/* cpoly.c */

int G_build_cpolygons_struct(cpolygon **, line_set *);
void G_destroy_cpolygons_struct(cpolygon *);
int G__mpolygon_centroid(cpolygon *, const int, double *, double *);
int G__bb1_contains_bb2(sring *, sring *);
int G__ring1_contains_ring2(sring *, sring *);
int G__verify_closed(sring *, const double);
int G__get_intrinsic_circulation(sring *);
int G__get_extrinsic_circulation(sring *, const double, const double);
int G__assign_isles(cpolygon *);


#endif  /* CPOLY_H_ */


