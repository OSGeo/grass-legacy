
#include "internoptri.h"
#include <math.h>
#include "lia.h"
#include "sos.h"


/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

#define DET21(i,j,a) sos_minor2 ((int) i + 1, (int) j + 1, (int) a, (int) 0)
#define ISPOSITIVE(lia) (lia_sign (lia) >= 0)
#define ISNEGATIVE(lia) (lia_sign (lia) < 0)
#define ISZERO(lia) (lia_sign (lia) == 0)
#define RATCOMPARE(lia1num,lia1den,lia2num,lia2den) \
  (lia_det2 (lia1num, lia2num, lia1den, lia2den, lia_pushf (LIA_NULL)), \
   lia_sign (lia_popf ()))
#define DET22(lia1num,lia1den,lia2num,lia2den,dest) \
  (lia_det2 (lia1num, lia2num, lia1den, lia2den, dest))
#define LIAGETTMP(tmp) (tmp = lia_pushf (LIA_NULL))

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

#define CASHSIZE 9
#define NOVERTEX -999999
#define CASH1 0
#define CASH2 1

typedef struct cashTypeDummy {
  indexType o, f, t;
  Lia_ptr   n, d;
  int       q;
  struct cashTypeDummy * next, * prev;
} cashType;

static cashType cash[2][CASHSIZE];
static cashType *cashTop[2];
static Lia_ptr tmp1, tmp2, tmp3, dFrom[2], dTo[2];

/*--------------------------------------------------------------------------*/

static int 
haAngleCashGet (cashName, origin, from, to, num, den, quadrant)

     int cashName;
     indexType origin, from, to;
     Lia_ptr *num, *den;
     int *quadrant;

{
  int i, name;
  register cashType *c, *cTop;
  static int firstTime = 1;

  if (firstTime) {
    LIAGETTMP (tmp1); LIAGETTMP (tmp2); LIAGETTMP (tmp3);
    LIAGETTMP (dFrom[0]); LIAGETTMP (dFrom[1]);
    LIAGETTMP (dTo[0]); LIAGETTMP (dTo[1]);

    for (name = 0; name <= 1; name++) {
      for (i = 0; i < CASHSIZE; i++) {
	LIAGETTMP (cash[name][i].n);
	LIAGETTMP (cash[name][i].d);
	cash[name][i].o = NOVERTEX;
	cash[name][i].next = &(cash[name][(i + 1) % CASHSIZE]);
	cash[name][i].prev = &(cash[name][(i + CASHSIZE - 1) % CASHSIZE]);
      }
      cashTop[name] = &(cash[name][0]);
    }

    firstTime = 0;
  }

  cTop = c = cashTop[cashName];
  if ((c->o == origin) && (c->f == from) && (c->t == to)) {
    *num = c->n;
    *den = c->d;
    *quadrant = c->q;
    return 1;
  }

  for (c = c->next;
       (((c->o != origin) || (c->f != from) || (c->t != to)) && 
	(c != cTop));
       c = c->next)
    {}

  if (c != cTop) {
    *num = c->n;
    *den = c->d;
    *quadrant = c->q;
    cashTop[cashName] = c;
    return 1;
  } else {
    cashTop[cashName] = c = c->prev;
    *num = c->n;
    *den = c->d;
    c->o = origin;
    c->f = from;
    c->t = to;
    return 0;
  }
}

/*--------------------------------------------------------------------------*/

static
haAngleCashPutQuadrant (cashName, quadrant)

     int cashName, quadrant;

{
  cashTop[cashName]->q = quadrant;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static int
haQuadrant (dx, dy)

     Lia_ptr dx, dy;

{
  if (ISPOSITIVE(dx))
    if (ISPOSITIVE(dy))
      if (ISZERO(dx))
        return 3;
      else
        return 0;
    else
      return 1;
  else
    if (ISPOSITIVE(dy))
      if (ISZERO(dy))
        return 2;
      else
        return 3;
    else
      return 2;
}

/*--------------------------------------------------------------------------*/

static
haRotateVertex (d, i, NumberOfRotations)

     Lia_ptr d[2];
     int *i;
     int       NumberOfRotations;

{
#define DX d[*i]
#define DY d[1 - *i]

  if (NumberOfRotations > 0) {
    if ((NumberOfRotations % 2) == 1) 
      *i = 1 - *i;
    if (NumberOfRotations <= 2) 
      if(! ISZERO (DX))
	lia_chs (DX);
    if (NumberOfRotations >= 2) 
      if(! ISZERO (DY))
	lia_chs (DY);
  }

#undef DY
#undef DX
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

#define DXF dFrom[f]
#define DYF dFrom[1-f]
#define DXT dTo[t]
#define DYT dTo[1-t]

/*--------------------------------------------------------------------------*/

static
haComputeAngle (origin, from, to, num, den, angleQuadrant)

     indexType origin, from, to;
     Lia_ptr *num, *den;
     int *angleQuadrant;

{
  int q, f, t;
  
  if (haAngleCashGet (CASH1, origin, from, to, num, den, angleQuadrant))
    return;

  f = t = 0;
  
  if (from != origin) {
    lia_assign (DXF, DET21 (from, origin, 1)); 
    lia_assign (DYF, DET21 (from, origin, 2));
  } else {
    lia_load (DXF, (int) 0);
    lia_load (DYF, (int) 0);
  }
  
  if (to != origin) {
    lia_assign (DXT, DET21 (to, origin, 1)); 
    lia_assign (DYT, DET21 (to, origin, 2));
  } else {
    lia_load (DXT, (int) 0);
    lia_load (DYT, (int) 0);
  }
  
  q = haQuadrant (DXF, DYF);
  haRotateVertex (dFrom, &f, q);
  haRotateVertex (dTo, &t, q);
  
  q = haQuadrant (DXT, DYT);
  haRotateVertex (dTo, &t, q);
  
  if (from == to) 
    *angleQuadrant = 4;
  else {
    lia_mul (tmp1, DYF, DXT);
    lia_mul (tmp2, DXF, DYT);
    
    if (lia_le (tmp1, tmp2)) {
      haRotateVertex (dTo, &t, 3);
      *angleQuadrant = (q + 3) % 4;
    } else 
      *angleQuadrant = q;
  }
    
  DET22 (DXT, DXF, DYT, DYF, *num); /* == dxt * dyf - dxf * dyt */
  lia_chs (DYF);
  DET22 (DXT, DYF, DYT, DXF, *den); /* == dxt * dxf + dyt * dyf */

  haAngleCashPutQuadrant (CASH1, *angleQuadrant);
}

/*--------------------------------------------------------------------------*/

static
haComputeAngleNumOnly (origin, from, to, num, angleQuadrant)

     indexType origin, from, to;
     Lia_ptr *num;
     int *angleQuadrant;

{
  Lia_ptr dummy;
  int q, f, t;

  if (haAngleCashGet (CASH2, origin, from, to, num, &dummy, angleQuadrant))
    return;

  f = t = 0;
  
  if (from != origin) {
    lia_assign (DXF, DET21 (from, origin, 1)); 
    lia_assign (DYF, DET21 (from, origin, 2));
  } else {
    lia_load (DXF, (int) 0);
    lia_load (DYF, (int) 0);
  }
  
  if (to != origin) {
    lia_assign (DXT, DET21 (to, origin, 1)); 
    lia_assign (DYT, DET21 (to, origin, 2));
  } else {
    lia_load (DXT, (int) 0);
    lia_load (DYT, (int) 0);
  }
  
  q = haQuadrant (DXF, DYF);
  haRotateVertex (dFrom, &f, q);
  haRotateVertex (dTo, &t, q);
  
  q = haQuadrant (DXT, DYT);
  haRotateVertex (dTo, &t, q);
  
  if (from == to) 
    *angleQuadrant = 4;
  else {
    lia_mul (tmp1, DYF, DXT);
    lia_mul (tmp2, DXF, DYT);
    
    if (lia_le (tmp1, tmp2)) {
      haRotateVertex (dTo, &t, 3);
      *angleQuadrant = (q + 3) % 4;
    } else 
      *angleQuadrant = q;
  }
    
  DET22 (DXT, DXF, DYT, DYF, *num); /* == dxt * dyf - dxf * dyt */

  haAngleCashPutQuadrant (CASH2, *angleQuadrant);
}

/*--------------------------------------------------------------------------*/

int
haComputeAngleQuadrantOnly (g, origin, from, to)

     graphType *g;
     indexType origin, from, to;

{
  Lia_ptr dummy;
  int angleQuadrant;

  haComputeAngleNumOnly (origin, from, to, &dummy, &angleQuadrant);

  return angleQuadrant;
}

/*--------------------------------------------------------------------------*/

int
haAngleGT180 (g, origin, from, to)

     graphType *g;
     indexType origin, from, to;

{
  Lia_ptr num;
  int quadrant;

  haComputeAngleNumOnly (origin, from, to, &num, &quadrant);

  if (quadrant > 2) return 1;
  if (quadrant < 2) return 0;

  if(! ISZERO (num)) return 1;

  if (from > to) return 1;
  if (to > from) return 0;

  return 0;
}

/*--------------------------------------------------------------------------*/

int
haAngleGT (g, origin1, from1, to1, origin2, from2, to2)

     graphType *g;
     indexType origin1, from1, to1, origin2, from2, to2;

{
  Lia_ptr num1, den1, num2, den2;
  int quadrant1, quadrant2;
  int sign;

  haComputeAngle (origin1, from1, to1, &num1, &den1, &quadrant1);
  haComputeAngle (origin2, from2, to2, &num2, &den2, &quadrant2);

  if (quadrant1 > quadrant2) return 1;
  if (quadrant1 < quadrant2) return 0;

  lia_mul (tmp1, num1, den2);
  lia_mul (tmp2, den1, num2);
  
  if (lia_le (tmp2, tmp1)) return 1;
  if (lia_le (tmp1, tmp2)) return 0;

  if (origin1 > origin2) return 1;
  if (origin2 > origin1) return 0;
  if (from1 > from2) return 1;
  if (from2 > from1) return 0;
  if (to1 > to2) return 1;
  if (to2 > to1) return 0;
  
  printf("ERROR: Two Angles have the same size!!??\n");
  printf("%d %d %d %d %d %d\n", origin1, from1, to1, origin2, from2, to2);
  exit (1);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------*/

void
haPrintAngle()

{

}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
