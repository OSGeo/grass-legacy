
#include "internoptri.h"
#include <math.h>
#include "geom/lia.h"
#include "geom/sos.h"

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

#define AREA_IS_FINITE 1
#define AREA_IS_MINUS_INFINITE 0

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

#define DET21(i,j,a) sos_minor2 ((int) i + 1, (int) j + 1, (int) a, (int) 0)
#define DET31(i,j,k,a,b) \
     sos_minor3 ((int) i + 1, (int) j + 1, (int) k + 1, \
                 (int) a, (int) b, (int) 0)
#define ISPOSITIVE(lia) (lia_sign (lia) >= 0)
#define ISNEGATIVE(lia) (lia_sign (lia) < 0)
#define ISZERO(lia) (lia_sign (lia) == 0)
#define RATCOMPARE(lia1num,lia1den,lia2num,lia2den) \
  (lia_det2 (lia1num, lia2num, lia1den, lia2den, lia_pushf (LIA_NULL)), \
   lia_sign (lia_popf ()))
#define DET22(lia1num,lia1den,lia2num,lia2den,dest) \
  (lia_det2 (lia1num, lia2num, lia1den, lia2den, dest))
#define LIAGETTMP(tmp) (tmp = lia_pushf (LIA_NULL))
#define LIAFREETMP() ((void) lia_popf ())
#define LIAFREETMP2() (LIAFREETMP(), LIAFREETMP())
#define LIAFREETMP4() (LIAFREETMP2(), LIAFREETMP2())
#define LIAFREETMP6() (LIAFREETMP4(), LIAFREETMP2())

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

#define CASHSIZE 9
#define NOVERTEX -999999

typedef struct cashTypeDummy {
  indexType o, f, t;
  Lia_ptr   a, l;
  struct cashTypeDummy * next, * prev;
} cashType;

static cashType cash[CASHSIZE];
static cashType *cashTop;
static Lia_ptr tmp1, tmp2, tmp3;

/*--------------------------------------------------------------------------*/

static int 
hhHeightCashGet (indexType origin, indexType from, indexType to, Lia_ptr *area, Lia_ptr *length)

{
  int i;
  register cashType *c;
  static int firstTime = 1;

  if (firstTime) {
    LIAGETTMP (tmp1); LIAGETTMP (tmp2); LIAGETTMP (tmp3);

    for (i = 0; i < CASHSIZE; i++) {
      LIAGETTMP (cash[i].a);
      LIAGETTMP (cash[i].l);
      cash[i].o = NOVERTEX;
      cash[i].next = &(cash[(i + 1) % CASHSIZE]);
      cash[i].prev = &(cash[(i + CASHSIZE - 1) % CASHSIZE]);
    }

    cashTop = &(cash[0]);
    firstTime = 0;
  }

  c = cashTop;
  if ((c->o == origin) && (c->f == from) && (c->t == to)) {
    *area = c->a;
    *length = c->l;
    return 1;
  }

  for (c = c->next;
       (((c->o != origin) || (c->f != from) || (c->t != to)) && 
	(c != cashTop));
       c = c->next)
    {}

  if (c != cashTop) {
    *area = c->a;
    *length = c->l;
    cashTop = c;
    return 1;
  } else {
    cashTop = c = c->prev;
    *area = c->a;
    *length = c->l;
    c->o = origin;
    c->f = from;
    c->t = to;
    return 0;
  }
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

static void
hhComputeHeight (indexType origin, indexType from, indexType to, Lia_ptr *area, Lia_ptr *length, int *status)

{
  if (from == to) {
    *status = AREA_IS_MINUS_INFINITE;
    return;
  }

  *status = AREA_IS_FINITE;
  
  if (hhHeightCashGet (origin, from, to, area, length)) 
    return;

  if ((origin == to) || (origin == from)) 
    lia_load (*area, (int) 0);
  else {
    lia_assign (tmp1, DET31 (origin, to, from, 1, 2));
    lia_mul (*area, tmp1, tmp1);
    if (ISNEGATIVE (tmp1))
      lia_chs (*area);
  }

  lia_assign (tmp1, DET21 (to, from, 1));
  lia_mul (tmp2, tmp1, tmp1);
  lia_assign (tmp1, DET21 (to, from, 2));
  lia_mul (tmp3, tmp1, tmp1);
  lia_add (*length, tmp2, tmp3);
}

/*---------------------------------------------------------------------------*/

static void
hhComputeHeightAreaOnly (indexType origin, indexType from, indexType to, Lia_ptr area, int *status)

{
  if (from == to) {
    *status = AREA_IS_MINUS_INFINITE;
    return;
  }

  *status = AREA_IS_FINITE;
  
  if ((origin == to) || (origin == from)) {
    lia_load (area, (int) 0);
  } else {
    lia_assign (tmp1, DET31 (origin, to, from, 1, 2));
    lia_mul (area, tmp1, tmp1);
    if (ISNEGATIVE (tmp1))
      lia_chs (area);
  }
}

/*---------------------------------------------------------------------------*/

#define AREA tmp3

int 
hhHeightLT0 (graphType *g, indexType origin, indexType from, indexType to)

{
  int status;

  hhComputeHeightAreaOnly (origin, from, to, AREA, &status);

  if (ISNEGATIVE (AREA) || (status == AREA_IS_MINUS_INFINITE)) return 1;

  if (! ISZERO (AREA)) return 0;

  if (from > to) return 1;
  if (to > from) return 0;

  return 1;
}

#undef AREA

/*---------------------------------------------------------------------------*/

int 
hhHeightLT (graphType *g, indexType origin1, indexType from1, indexType to1, indexType origin2, indexType from2, indexType to2)

{
  Lia_ptr area1, length1, area2, length2;
  int status1, status2, quadrant1, quadrant2;

  hhComputeHeight (origin1, from1, to1, &area1, &length1, &status1);

  hhComputeHeight (origin2, from2, to2, &area2, &length2, &status2);

  if ((status1 != AREA_IS_MINUS_INFINITE) ||
      (status2 != AREA_IS_MINUS_INFINITE)) {

    if (status1 == AREA_IS_MINUS_INFINITE) return 1;
    if (status2 == AREA_IS_MINUS_INFINITE) return 0;

    lia_mul (tmp1, area1, length2);
    lia_mul (tmp2, length1, area2);

    if (lia_le (tmp2, tmp1)) return 0;    
    if (lia_le (tmp1, tmp2)) return 1;
  }

  if (ISZERO (area1) && ISZERO (area2)) {
    quadrant1 = haComputeAngleQuadrantOnly (g, origin1, from1, to1);
    quadrant2 = haComputeAngleQuadrantOnly (g, origin2, from2, to2);
    if ((quadrant1 > 0) && (quadrant2 == 0)) return 1;
    if ((quadrant2 > 0) && (quadrant1 == 0)) return 0;
  }

  if (origin1 > origin2) return 1;
  if (origin2 > origin1) return 0;
  if (from1 > from2) return 1;
  if (from2 > from1) return 0;
  if (to1 > to2) return 1;
  if (to2 > to1) return 0;

  fprintf (stdout,"ERROR: Two Heights have the same size!!??");
  fprintf (stdout,"%d %d %d %d %d %d\n", origin1, from1, to1, origin2, from2, to2);
  exit (1);

  return 1;
}

/*---------------------------------------------------------------------------*/

void 
hhPrintHeight (FILE *fp, indexType oi, coordType ox, coordType oy, coordType oz, indexType fi, coordType fx, coordType fy, coordType fz, indexType ti, coordType tx, coordType ty, coordType tz)

{
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/



