#include "gis.h"
#include "optri.h"
#include "voroSupport.h"

/*--------------------------------------------------------------------------*/

static int
compareSlope (x0, y0, x1, y1, x2, y2)

     double x0, y0, x1, y1, x2, y2;

{
  double tmp;

  tmp = (y1 - y0) * (x2 - x1) - (y2 - y1) * (x1 - x0);

  if (tmp == 0) return 0;
  if (tmp < 0) return -1;
  else return 1;
}

/*--------------------------------------------------------------------------*/

static void
computeLineBariCenter (x1, y1, x2, y2, xBari, yBari)

     double x1, y1, x2, y2;
     double *xBari, *yBari;

{
  *xBari = (x1 + x2) / 2.0;
  *yBari = (y1 + y2) / 2.0;
}

/*--------------------------------------------------------------------------*/

static void
computePerpBisect (xIn1, yIn1, xIn2, yIn2, xOut1, yOut1, xOut2, yOut2)

     double xIn1, yIn1, xIn2, yIn2;
     double *xOut1, *yOut1, *xOut2, *yOut2;

{
  computeLineBariCenter (xIn1, yIn1, xIn2, yIn2, xOut1, yOut1);
  *xOut2 = *xOut1 + (*yOut1 - yIn1);
  *yOut2 = *yOut1 - (*xOut1 - xIn1);
}  

/*--------------------------------------------------------------------------*/

static int
computeSlope (x1, y1, x2, y2, slope)

     double x1, y1, x2, y2;
     double *slope;

{
  if (x2 -x1 != 0) {
    *slope = (y2 - y1) / (x2 - x1);
    return 0;
  }

  return 1;
}

/*--------------------------------------------------------------------------*/

static void
computeOffset (x, y, slope, slopeInf, offset)

     double x, y;
     double slope;
     int slopeInf;
     double *offset;

{
  if (! slopeInf)
    *offset = y - x * slope;
  else
    *offset = x;
}

/*--------------------------------------------------------------------------*/

static void
computeLineIntersect (slope1, slopeInf1, offset1, 
		      slope2, slopeInf2, offset2, xInter, yInter)

     double slope1, offset1, slope2, offset2;
     int slopeInf1, slopeInf2;
     double *xInter, *yInter;

{
  if (! (slopeInf1 || slopeInf2)) {
    *xInter = (offset2 - offset1) / (slope1 - slope2); /* can't be inf */
    *yInter = (*xInter) * slope1 + offset1;
  } else 
    if (slopeInf1) 
      if (! slopeInf2) {
	*xInter = offset1;
	*yInter = (*xInter) * slope2 + offset2;
      } else 
	G_fatal_error ("in computation of Voronoi vertex\n");
    else {
      *xInter = offset2;
      *yInter = (*xInter) * slope1 + offset1;
    }	  
}

/*--------------------------------------------------------------------------*/

static int
testCocircular (q, qe)

     void *q;
     indexType qe;

{
  return ((! qeISCHedge (q, qeQETOE (qe))) &&
	  sosrCocircular (q, qeDST (q, qeONEXT (q, qe)),
			  qeORG (q, qe), qeDST (q, qe),
			  qeDST (q, qeOPREV (q, qe))));
}

/*--------------------------------------------------------------------------*/

static void
sortIndices (i0, i1, i2)

     indexType *i0, *i1, *i2;

{
  indexType tmp;

  if (*i0 < *i1)
    if (*i0 < *i2) {
      tmp = *i1; *i1 = *i0; *i0 = *i2; *i2 = tmp;
    } else {
      tmp = *i1; *i1 = *i2; *i2 = *i0; *i0 = tmp;
    }
  else
    if (*i2 < *i1) {
      tmp = *i1; *i1 = *i2; *i2 = *i0; *i0 = tmp; 
    }
  
  if (*i0 < *i2) {
    tmp = *i0; *i0 = *i2; *i2 = tmp;
  }

  /* now: i1 < i2 < i0 */
}

/*--------------------------------------------------------------------------*/

static int
compareIndices (q, qe1, qe2)

     void *q;
     indexType qe1, qe2;

{
  indexType s0, s1, s2, r0, r1, r2;

  r0 = qeORG (q, qe1); r1 = qeDST (q, qe1); r2 = qeDST (q, qeONEXT (q, qe1));
  s0 = qeORG (q, qe2); s1 = qeDST (q, qe2); s2 = qeDST (q, qeONEXT (q, qe2));
  
  sortIndices (&r0, &r1, &r2);
  sortIndices (&s0, &s1, &s2);

  return ((r1 < s1) || ((r1 == s1) && ((r2 < s2) || 
				       ((r2 == s2) && (r0 < s0)))));
}

/*--------------------------------------------------------------------------*/

static indexType 
findEdge (q, qe)

     void *q;
     indexType qe;

{
  indexType e, f;

  e = qe;

  /* the recusrion is a bit of an overkill; but then it doesn't cost a */
  /* lot. */

  if (testCocircular (q, qeONEXT (q, qe)))
    if (compareIndices (q, e, f = findEdge (q, qeONEXT (q, qe))))
      e = f;
  if (testCocircular (q, qeSYM (qeOPREV (q, qeSYM (qe)))))
    if (compareIndices (q, e, f = findEdge (q, 
					    qeSYM (qeOPREV (q, qeSYM (qe))))))
      e = f;

  return e;
}

/*--------------------------------------------------------------------------*/

static int
computeVoronoiVertex (q, s, qe, x, y)

     void *q, *s;
     indexType qe;
     double *x, *y;

{
  indexType tmp, e, s0, s1, s2;
  double x0, y0, x1, y1, x2, y2, x3, y3;
  double slope0, offset0, slope1, offset1;
  int slopeInf0, slopeInf1;

  s0 = qeORG (q, qe); s1 = qeDST (q, qe); s2 = qeDST (q, qeONEXT (q, qe));

  if (! sosrCollinear (s, s0, s1, s2)) {

    e = findEdge (q, qe);

    s0 = qeORG (q, e); s1 = qeDST (q, e); s2 = qeDST (q, qeONEXT (q, e));
    sortIndices (&s0, &s1, &s2);

    computePerpBisect (siSITEX (s, s0), siSITEY (s, s0), 
		       siSITEX (s, s1), siSITEY (s, s1),
		       &x0, &y0, &x1, &y1);
    slopeInf0 = computeSlope (x0, y0, x1, y1, &slope0);
    computeOffset (x0, y0, slope0, slopeInf0, &offset0);
    
    computePerpBisect (siSITEX (s, s2), siSITEY (s, s2), 
		       siSITEX (s, s1), siSITEY (s, s1),
		       &x2, &y2, &x3, &y3);
    slopeInf1 = computeSlope (x2, y2, x3, y3, &slope1);
    computeOffset (x2, y2, slope1, slopeInf1, &offset1);
    
    computeLineIntersect (slope0, slopeInf0, offset0, 
			  slope1, slopeInf1, offset1, x, y);
    return 1;
  } else 
    return 0;
}

/*--------------------------------------------------------------------------*/

static void
extendRayToBoundary (s, xFrom, yFrom, xTo, yTo, xBoundary, yBoundary)

     void * s;
     double xFrom, yFrom, xTo, yTo;
     double *xBoundary, *yBoundary;

{
  double rat;

  if (xFrom != xTo) {
    
    if (xFrom < xTo) {
      *xBoundary = EAST;
      rat = (*xBoundary - xFrom) / (xTo - xFrom);
      *yBoundary = yFrom + rat * (yTo - yFrom);
      Vect_append_point (vRIGHT, *xBoundary, *yBoundary);
    } else {
      *xBoundary = WEST;
      rat = (*xBoundary - xFrom) / (xTo - xFrom);
      *yBoundary = yFrom + rat * (yTo - yFrom);
      Vect_append_point (vLEFT, *xBoundary, *yBoundary);
    }

    if (yFrom != yTo)
      if (*yBoundary > NORTH) {
	*yBoundary = NORTH;
	rat = (*yBoundary - yFrom) / (yTo - yFrom);
	*xBoundary = xFrom + rat * (xTo - xFrom);
	Vect_append_point (vTOP, *xBoundary, *yBoundary);
      } else 
	if (*yBoundary < SOUTH) {
	  *yBoundary = SOUTH;
	  rat = (*yBoundary - yFrom) / (yTo - yFrom);
	  *xBoundary = xFrom + rat * (xTo - xFrom);
	  Vect_append_point (vBOTTOM, *xBoundary, *yBoundary);
	}

  } else {

    *xBoundary = xFrom;
    if (yFrom < yTo) {
      *yBoundary = NORTH;
      Vect_append_point (vTOP, *xBoundary, *yBoundary);
    } else {
      *yBoundary = SOUTH;
      Vect_append_point (vBOTTOM, *xBoundary, *yBoundary);
    }
  }
}

/*--------------------------------------------------------------------------*/

static void
truncateRay (s, xFrom, yFrom, xTo, yTo)

     void *s;
     double xFrom, yFrom, *xTo, *yTo;

{
  double rat;

  if (*xTo != xFrom) {
    if (*xTo > EAST) {
      rat = (EAST - xFrom) / (*xTo - xFrom);
      *xTo = EAST;
      *yTo = yFrom + rat * (*yTo - yFrom);
      Vect_append_point (vRIGHT, *xTo, *yTo);
    } else 
      if (*xTo < WEST) {
	rat = (WEST - xFrom) / (*xTo - xFrom);
	*xTo = WEST;
	*yTo = yFrom + rat * (*yTo - yFrom);
	Vect_append_point (vLEFT, *xTo, *yTo);
      }
  }

  if (*yTo != yFrom) {
    if (*yTo > NORTH) {
      rat = (NORTH - yFrom) / (*yTo - yFrom);
      *yTo = NORTH;
      *xTo = xFrom + rat * (*xTo - xFrom);
      Vect_append_point (vTOP, *xTo, *yTo);
    } else
      if (*yTo < SOUTH) {
	rat = (SOUTH - yFrom) / (*yTo - yFrom);
	*yTo = SOUTH;
	*xTo = xFrom + rat * (*xTo - xFrom);
	Vect_append_point (vBOTTOM, *xTo, *yTo);
      }
  }

  if (*xTo != xFrom) {
    if (*xTo > EAST) {
      rat = (EAST - xFrom) / (*xTo - xFrom);
      *xTo = EAST;
      *yTo = yFrom + rat * (*yTo - yFrom);
      Vect_append_point (vRIGHT, *xTo, *yTo);
    } else 
      if (*xTo < WEST) {
	rat = (WEST - xFrom) / (*xTo - xFrom);
	*xTo = WEST;
	*yTo = yFrom + rat * (*yTo - yFrom);
	Vect_append_point (vLEFT, *xTo, *yTo);
      }
  }
}
    
/*--------------------------------------------------------------------------*/

static void
truncateEdge (s, xFrom, yFrom, xTo, yTo)

     void *s;
     double *xFrom, *yFrom, *xTo, *yTo;

{
  truncateRay (s, *xFrom, *yFrom, xTo, yTo);
  truncateRay (s, *xTo, *yTo, xFrom, yFrom);
}

/*--------------------------------------------------------------------------*/

static int
vertexInsideRange (s, x, y)

     void * s;
     double x, y;

{
  return ((x < EAST) && (x > WEST) &&
	  (y < NORTH) && (y > SOUTH));
}

/*--------------------------------------------------------------------------*/

static int
computeVoronoiEdgeFinite (q, s, edge, xFrom, yFrom, xTo, yTo)

     void *q, *s;
     indexType edge;
     double *xFrom, *yFrom, *xTo, *yTo;

{
  int finite1, finite2;
  indexType qe;

  qe = qeMAKEQE (edge);

  if (testCocircular (q, qe)) return 0;

  finite1 = computeVoronoiVertex (q, s, qe, xFrom, yFrom);
  finite2 = computeVoronoiVertex (q, s, qeSYM (qe), xTo, yTo);
  if (! (finite1 && finite2))
    G_fatal_error ("Voronoi vertex not finite.\n");

  truncateEdge (s, xFrom, yFrom, xTo, yTo);
}

/*--------------------------------------------------------------------------*/

static int
computeVoronoiEdgeInfinite (q, s, edge, xFrom, yFrom, xTo, yTo)

     void *q, *s;
     indexType edge;
     double *xFrom, *yFrom, *xTo, *yTo;
     
{
  int finite, leftTurn;
  double xBari, yBari, xo, yo, xd, yd, xTmp, yTmp;
  indexType qe;
  
  qe = qeMAKEQE (edge);
  
  if (qeDST (q, qeONEXT (q, qe)) != qeDST (q, qeOPREV (q, qeSYM (qe))))
    qe = qeSYM (qe);
  
  finite = computeVoronoiVertex (q, s, qe, xFrom, yFrom);
  
  if (! finite)
    G_fatal_error ("Voronoi vertex not finite (boundary).\n");

  if (! vertexInsideRange (s, *xFrom, *yFrom))
    return 0;

  leftTurn = compareSlope (xo = siSITEX (s, qeORG (q, qe)),
			   yo = siSITEY (s, qeORG (q, qe)),
			   xd = siSITEX (s, qeDST (q, qe)),
			   yd = siSITEY (s, qeDST (q, qe)),
			   *xFrom, *yFrom);
  
  if (leftTurn != 0) {
    computeLineBariCenter (xo, yo, xd, yd, &xBari, &yBari);
    if (leftTurn == 1) {
      xTmp = 2 * (*xFrom) - xBari;
      yTmp = 2 * (*yFrom) - yBari;
    } else {
      xTmp = xBari;
      yTmp = yBari;
    } 
  } else {
    xTmp = *xFrom + (yd - yo);
    yTmp = *yFrom - (xd - xo);
  }
  
  extendRayToBoundary (s, *xFrom, *yFrom, xTmp, yTmp, xTo, yTo);
  truncateRay (s, *xTo, *yTo, xFrom, yFrom);

  return 1;
}

/*--------------------------------------------------------------------------*/

int
computeVoronoiEdge (q, s, edge, xFrom, yFrom, xTo, yTo)

     void *q, *s;
     indexType edge;
     double *xFrom, *yFrom, *xTo, *yTo;

{
  if (qeISCHedge (q, edge)) 
    return computeVoronoiEdgeInfinite (q, s, edge, xFrom, yFrom, xTo, yTo);
  else 
    return computeVoronoiEdgeFinite (q, s, edge, xFrom, yFrom, xTo, yTo);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
