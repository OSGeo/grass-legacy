
#include "internoptri.h"
#include "geom/lia.h"
#include "geom/sos.h"

/*-------------------------------------------------------------------------*/

#define DET21(i,j,a) sos_minor2 ((int) i + 1, (int) j + 1, (int) a, (int) 0)
#define DET31(i,j,k,a,b) \
     sos_minor3 ((int) i + 1, (int) j + 1, (int) k + 1, \
                 (int) a, (int) b, (int) 0)
#define DET41(i,j,k,l,a,b,c) \
     sos_minor4 ((int) i + 1, (int) j + 1, (int) k + 1, (int) l + 1, \
                 (int) a, (int) b, (int) c, (int) 0)
#define LIAGETTMP(tmp) (tmp = lia_pushf (LIA_NULL))

/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/

static int 
leftTurnMath (indexType i, indexType j, indexType k)

/* Returns:
 *           | x(i)  y(i)  1 |
 *           | x(j)  y(j)  1 | > 0
 *           | x(k)  y(k)  1 |
 */
{
  int det, soscase;

  if (det = lia_sign (DET31 (i, j, k, 1, 2)))
    soscase = 0;
  else if (det = lia_sign (DET21 (k, j, 1)))
    soscase = 1;
  else if (det = lia_sign (DET21 (j, k, 2)))
    soscase = 2;
  else if (det = lia_sign (DET21 (i, k, 1)))
    soscase = 3;
  else {
    det = 1;
    soscase = 4;
  }

  return (det > 0);
}

/*-------------------------------------------------------------------------*/

static int 
leftTurnCoords (indexType s0, indexType s1, indexType s2)

{
  int result;

  if (s0 < s1)
    if (s1 < s2)
      result = leftTurnMath (s0, s1, s2);
    else
      if (s0 < s2)
	result = ! leftTurnMath (s0, s2, s1);
      else
	result = leftTurnMath (s2, s0, s1);
  else
    if (s0 < s2)
      result = ! leftTurnMath (s1, s0, s2);
    else
      if (s1 < s2)
	result = leftTurnMath (s1, s2, s0);
      else
	result = ! leftTurnMath (s2, s1, s0);

  return result;
}

/*-------------------------------------------------------------------------*/

int sosrLeftTurn (graphType *g, indexType s0, indexType s1, indexType s2)
/* double *detdbl) */

{
/*fprintf (stdout,"leftTurn %d %d %d %d\n", s0, s1, s2, leftTurnCoords (s0, s1, s2));*/
  return leftTurnCoords (s0, s1, s2);
}

/*-------------------------------------------------------------------------*/

int 
sosrCollinear (graphType *g, indexType s0, indexType s1, indexType s2)

{
  if ((s0 == s1) || (s1 == s2) || (s0 == s2)) return 1;

  return (lia_sign (DET31 ((int) s0, (int) s1, (int) s2, 
			   (int) 1, (int) 2)) == 0);
}

/*-------------------------------------------------------------------------*/

static int 
regularityMath (indexType i, indexType j, indexType k, indexType l, int delaunay)

/* Returns:				   
 *    | x(i)  y(i)  (x(i)^2 + y(i)^2 - z(i))  1 |	
 *    | x(j)  y(j)  (x(j)^2 + y(j)^2 - z(j))  1 |	
 *    | x(k)  y(k)  (x(k)^2 + y(k)^2 - z(k))  1 | > 0   
 *    | x(l)  y(l)  (x(l)^2 + y(l)^2 - z(l))  1 |	
 *
 *   s1, s2, s3 and s4 assumed to be in counter-clockwise order in
 * a circle.
 */
{
  int det, soscase, w;

  w = (delaunay ? 4 : 5);

  if (det = lia_sign (DET41 (i, j, k, l, 1, 2, w)))
    soscase = 0;
  else if (det =   lia_sign (DET31 (j, k, l, 1, 2)))
    soscase = 1;
  else if (det =   lia_sign (lia_neg (DET31 (j, k, l, 1, w))))
    soscase = 2;
  else if (det =   lia_sign (DET31 (j, k, l, 2, w)))
    soscase = 3;
  else if (det =   lia_sign (lia_neg (DET31 (i, k, l, 1, 2))))
    soscase = 4;
  else if (det =   lia_sign (DET21 (k, l, 1)))
    soscase = 5;
  else if (det =   lia_sign (DET21 (l, k, 2)))
    soscase = 6;
  else if (det =   lia_sign (DET31 (i, k, l, 1, w)))
    soscase = 7;
  else if (det =   lia_sign (DET21 (k, l, w)))
    soscase = 8;
  else if (det =   lia_sign (lia_neg (DET31 (i, k, l, 2, w))))
    soscase = 9;
  else if (det =   lia_sign (DET31 (i, j, l, 1, 2)))
    soscase = 10;
  else if (det =   lia_sign (DET21 (l, j, 1)))
    soscase = 11;
  else if (det =   lia_sign (DET21 (j, l, 2)))
    soscase = 12;
  else if (det =   lia_sign (DET21 (i, l, 1)))
    soscase = 13;
  else {
    det = 1;
    soscase = 14;
  }

/*fprintf (stdout,"regularityMath (%d %d %d %d, %d) ==> %d, %d\n", 
	i, j, k, l, det, soscase);*/


  return (det > 0);
}

/*-------------------------------------------------------------------------*/

static int 
sosRegularTest (graphType *g, indexType s0, indexType s1, indexType s2, indexType s3, int doDelaunay)

{
  static indexType site[4];
  int i, j, tmp, result, nswaps = 0;

  site[0] = s0; site[1] = s1; 
  site[2] = s2; site[3] = s3;

  for (i = 3; i > 0; i--) {
    for (j = 0; j < i; j++) {
      if (site[j] > site[j+1]) {
	tmp = site[j]; site[j] = site[j+1]; site[j+1] = tmp;
	nswaps++;
      }
    }
  }

  result = regularityMath (site[0], site[1], site[2], site[3], doDelaunay);

  if (nswaps & 1) 
    result = ! result;

  return result;
}

/*-------------------------------------------------------------------------*/

int 
sosrInCircle (graphType *g, indexType s0, indexType s1, indexType s2, indexType s3, double *detdbl)

{
/*fprintf (stdout,"inCircle %d %d %d %d %d\n", s0, s1, s2, s3, sosRegularTest (g, s0, s1, s2, s3, 1));*/
  return sosRegularTest (g, s0, s1, s2, s3, 1);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

/*
  P_-2           P_-1
    *<------------*
     \           /
      \    0    /
       \       /
        \     /
         \   /
          \ /
           * P_-3
*/

/* infP [i] corresponds to P_-i */

static int infP [4][3] = {{0, 0, 0},
                      {1, 1, 1 * 1 + 1 * 1},
		      {-1, 1, (-1) * (-1) + 1 * 1},
                      {0, -1, 0 * 0 + (-1) * (-1)}}; 	    

static Lia_ptr liatmp1, liatmp2, liatmp3, liatmp4, liaInfP [4][3];
static int inCircle3InfArr [4][4][4], leftTurn3InfArr [4][4][4],
  leftTurn2InfArr [4][4];

/*--------------------------------------------------------------------------*/

void 
sosrInit (void)

{
  int i, j, k;
  static int firstTimeInfiniteSos = 1;

  if (! firstTimeInfiniteSos) return;

  firstTimeInfiniteSos = 0;

  LIAGETTMP (liatmp1); LIAGETTMP (liatmp2); LIAGETTMP (liatmp3); 
  LIAGETTMP (liatmp4);
  
  for (i = 1; i < 4; i++)
    for (j = 0; j < 3; j++) {
      LIAGETTMP (liaInfP [i][j]);
      lia_load (liaInfP [i][j], infP [i][j]);
    }

  i = 3; j = 2; k = 1;

  if (! (inCircle3InfArr [i][j][k] = 
	 (infP[i][2] *
	  (infP[j][0] * infP[k][1] - infP[j][1] * infP[k][0]) -
	  infP[j][2] *
	  (infP[i][0] * infP[k][1] - infP[i][1] * infP[k][0]) +
	  infP[k][2] *
	  (infP[i][0] * infP[j][1] - infP[i][1] * infP[j][0])))) {
    fprintf (stdout,"ERROR: sosrInit: 3 infinite points: result == 0!\n");
    exit (1);
  }
  
  if (! (leftTurn3InfArr [i][j][k] = 
	 infP[i][0] * (infP[j][1] - infP[k][1]) -
	 infP[i][1] * (infP[j][0] - infP[k][0]) +
	 (infP[j][0] * infP[k][1] - infP[k][0] * infP[j][1]))) {
    fprintf (stdout,"ERROR: sosrInit: 3 infinite points: result == 0!\n");
    exit (1);
  }

  for (j = 1; j < 4; j++)
    for (i = j + 1; i < 4; i++) {
      leftTurn2InfArr [i][j] = 
	infP[i][0] * infP[j][1] - infP[j][0] * infP[i][1];
      if (! leftTurn2InfArr [i][j])
	if (! (leftTurn2InfArr [i][j] = - (infP[i][0] - infP[j][0])))
	  if (! (leftTurn2InfArr [i][j] = infP[i][1] - infP[j][1])) {
	    fprintf (stdout,"sosrInit: 2 infinite points: result == 0 !\n");
	    exit (1);
	  }
    }
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static int 
inCircle3Inf (graphType *g, indexType i, indexType j, indexType k, indexType l)

{
  return inCircle3InfArr [i][j][k] > 0;
}

/*--------------------------------------------------------------------------*/

static int 
inCircle2Inf (graphType *g, indexType i, indexType j, indexType k, indexType l)

{
  int result, tmp1, tmp2;

  lia_assign (liatmp1, DET21 (k, l, 2));
  lia_load (liatmp2, tmp1 = infP[i][2] * infP[j][0] - infP[j][2] * infP[i][0]);
  lia_mul (liatmp3, liatmp1, liatmp2);

  lia_assign (liatmp1, DET21 (k, l, 1));
  lia_load (liatmp2, tmp2 = infP[i][2] * infP[j][1] - infP[j][2] * infP[i][1]);
  lia_mul (liatmp4, liatmp1, liatmp2);

  lia_sub (liatmp1, liatmp3, liatmp4);
  result = lia_sign (liatmp1);

  if (! result)
    if (! (result = tmp1))
      if (! (result = - tmp2)) {
	fprintf (stdout,"ERROR: incircleInf: 2 infinite points: result == 0 !\n");
	exit (1);
      }
  
  return result > 0;
}
	
/*--------------------------------------------------------------------------*/

static int 
inCircle1Inf (graphType *g, indexType i, indexType j, indexType k, indexType l)

{
  return leftTurnMath (j, k, l);
}

/*--------------------------------------------------------------------------*/

int 
sosrInCircleInf (graphType *g, indexType s0, indexType s1, indexType s2, indexType s3, int doDelaunay, double *detdbl)

{
  indexType site[4];
  int i, j, tmp, result, nswaps = 0;

  if ((s0 >= 0) && (s1 >= 0) && (s2 >= 0) && (s3 >= 0)) 
    return sosRegularTest (g, s0, s1, s2, s3, doDelaunay);
  
  sosrInit ();

  site[0] = s0; site[1] = s1; 
  site[2] = s2; site[3] = s3;

  for (i = 3; i > 0; i--) {
    for (j = 0; j < i; j++) {
      if (site[j] > site[j+1]) {
	tmp = site[j]; site[j] = site[j+1]; site[j+1] = tmp;
	nswaps++;
      }
    }
  }

  /* now the infinite points are on top of the list */

  if (site[2] < 0) 
    /* first case: 3 infinite points */
    result = inCircle3Inf (g, - site[0], - site[1], - site[2], site[3]);
  else
    if (site[1] < 0)
      /* second case: 2 infinite points */
      result = inCircle2Inf (g, - site[0], - site[1], site[2], site[3]);
    else
      /* third case: 1 infinite point */
      result = inCircle1Inf (g, - site[0], site[1], site[2], site[3]);

  if (nswaps & 1) 
    result = ! result;

/*fprintf (stdout,"inCircleInf %d %d %d %d %d\n", s0, s1, s2, s3, result);*/

  return result;
}

/*-------------------------------------------------------------------------*/

int 
sosrCocircular (graphType *g, indexType s0, indexType s1, indexType s2, indexType s3)

{
  return (lia_sign (DET41 ((int) s0, (int) s1, (int) s2, (int) s3,
                           (int) 1, (int) 2, (int) 4)) == 0);
}

/*--------------------------------------------------------------------------*/

static int 
leftTurn3Inf (graphType *g, indexType i, indexType j, indexType k)

{
  return leftTurn3InfArr [i][j][k] > 0;
}

/*--------------------------------------------------------------------------*/

static int 
leftTurn2Inf (graphType *g, indexType i, indexType j, indexType k)

{
  return leftTurn2InfArr [i][j] > 0;
}

/*--------------------------------------------------------------------------*/

static int 
leftTurn1Inf (graphType *g, indexType i, indexType j, indexType k)

{
  int result;

  lia_assign (liatmp1, DET21 (j, k, 2));
  lia_mul (liatmp2, liaInfP [i][0], liatmp1);
  
  lia_assign (liatmp1, DET21 (j, k, 1));
  lia_mul (liatmp3, liaInfP [i][1], liatmp1);

  lia_sub (liatmp1, liatmp2, liatmp3);
  result = lia_sign (liatmp1);

  if (! result)
    if (! (result = infP[i][0]))
      if (! (result = - infP[i][1])) {
        fprintf (stdout,"ERROR: incircleInf: 1 infinite point: result == 0 !\n");
        exit (1);
      }

  return result > 0;
}

/*--------------------------------------------------------------------------*/

int 
sosrLeftTurnInf (graphType *g, indexType s0, indexType s1, indexType s2, double *detdbl)

{
  indexType site[3];
  int i, j, tmp, result, nswaps = 0;

  if ((s0 >= 0) && (s1 >= 0) && (s2 >= 0))
    return sosrLeftTurn (g, s0, s1, s2); /* detdbl);

  sosrInit ();

  site[0] = s0; site[1] = s1; 
  site[2] = s2;

  for (i = 2; i > 0; i--) {
    for (j = 0; j < i; j++) {
      if (site[j] > site[j+1]) {
	tmp = site[j]; site[j] = site[j+1]; site[j+1] = tmp;
	nswaps++;
      }
    }
  }

  /* now the infinite points are on top of the list */

  if (site[2] < 0) 
    /* first case: 3 infinite points */
    result = leftTurn3Inf (g, - site[0], - site[1], - site[2]);
  else
    if (site[1] < 0)
      /* second case: 2 infinite points */
      result = leftTurn2Inf (g, - site[0], - site[1], site[2]);
    else
      /* third case: 1 infinite point */
      result = leftTurn1Inf (g, - site[0], site[1], site[2]);

  if (nswaps & 1) {
    result = ! result;
    *detdbl = - (*detdbl);
  }

/*fprintf (stdout,"leftTurnInf %d %d %d %d\n", s0, s1, s2, result);*/

  return result;
}

/*-------------------------------------------------------------------------*/

int 
sosrLeftOf (graphType *g, indexType s0, indexType s1, double *detdbl)

{
  int leftOf;

  if (s0 == s1) return 0;

  if (leftOf = lia_sign (DET21 (s1, s0, 1)))
    return leftOf > 0;

  return s0 > s1;
}

/*-------------------------------------------------------------------------*/

int 
sosrBelow (graphType *g, indexType s0, indexType s1, double *detdbl)

{
  int below;

  if (s0 == s1) return 0;

  if (below = lia_sign (DET21 (s1, s0, 2)))
    return below > 0;

  return s0 > s1;
}

/*-------------------------------------------------------------------------*/

int 
sosrIdenticalX (graphType *g, indexType s0, indexType s1)

{
  if (s0 == s1) return 1;

  return (lia_sign (DET21 (s1, s0, 1)) == 0);
}

/*-------------------------------------------------------------------------*/

int 
sosrIdenticalY (graphType *g, indexType s0, indexType s1)

{
  if (s0 == s1) return 1;

  return (lia_sign (DET21 (s1, s0, 2)) == 0);
}

/*-------------------------------------------------------------------------*/

int 
sosrIdenticalXY (graphType *g, indexType s0, indexType s1)

{
  if (s0 == s1) return 1;

  return ((lia_sign (DET21 (s1, s0, 1)) == 0) &&
	  (lia_sign (DET21 (s1, s0, 2)) == 0));
}

/*-------------------------------------------------------------------------*/

int 
sosrLexoCompare (graphType *g, indexType s0, indexType s1, double *detdbl)

{
  int leftOf;

  if (s0 == s1) return 0;

  if (leftOf = lia_sign (DET21 (s0, s1, 1)))
    return leftOf;
  if (leftOf = lia_sign (DET21 (s0, s1, 2)))
    return leftOf;
  return 0;
}

/*--------------------------------------------------------------------------*/

int 
sosrSlopeCompare (graphType *g, indexType i, indexType j, indexType k, indexType l)

{
  sosrInit ();

  lia_assign (liatmp1, DET21 (i, j, 2));
  lia_assign (liatmp2, DET21 (k, l, 1));
  lia_mul (liatmp3, liatmp1, liatmp2);

  lia_assign (liatmp1, DET21 (k, l, 2));
  lia_assign (liatmp2, DET21 (i, j, 1));
  lia_mul (liatmp4, liatmp1, liatmp2);

  lia_sub (liatmp1, liatmp3, liatmp4);
  return lia_sign (liatmp1);
}
	
/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/
