#include <math.h>
#include "internoptri.h"
#include "geom/lia.h"
#include "geom/sos.h"

#define MYMAX(a,b) (a > b ? a : b)

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

/*-------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------*/

#define CASHSIZE 9
#define NOVERTEX -999999

typedef struct cashTypeDummy {
  indexType o, f, t;
  Lia_ptr   a, b, c, ab;
  indexType anchor;
  struct cashTypeDummy * next, * prev;
} cashType;

static cashType cash[CASHSIZE];
static cashType *cashTop;
static Lia_ptr tmp1, tmp2, tmp3, tmp4;


/*-------------------------------------------------------------------------*/

static int 
hsSlopeCashGet (origin, from, to, A, B, C, AplusB)

     indexType origin, from, to;
     Lia_ptr *A, *B, *C, *AplusB;

{
  int i;
  register cashType *c;
  static int firstTime = 1;

  if (firstTime) {
    LIAGETTMP (tmp1); LIAGETTMP (tmp2); LIAGETTMP (tmp3); LIAGETTMP (tmp4); 

    for (i = 0; i < CASHSIZE; i++) {
      LIAGETTMP (cash[i].a);
      LIAGETTMP (cash[i].b);
      LIAGETTMP (cash[i].c);
      LIAGETTMP (cash[i].ab);
      cash[i].o = NOVERTEX;
      cash[i].next = &(cash[(i + 1) % CASHSIZE]);
      cash[i].prev = &(cash[(i + CASHSIZE - 1) % CASHSIZE]);
    }

    cashTop = &(cash[0]);
    firstTime = 0;
  }

  c = cashTop;
  if ((c->o == origin) && (c->f == from) && (c->t == to)) {
    *A = c->a;
    *B = c->b;
    *C = c->c;
    *AplusB = c->ab;
    return 1;
  }

  for (c = c->next;
       (((c->o != origin) || (c->f != from) || (c->t != to)) && 
	(c != cashTop));
       c = c->next)
    {}

  if (c != cashTop) {
    *A = c->a;
    *B = c->b;
    *C = c->c;
    *AplusB = c->ab;
    cashTop = c;
    return 1;
  } else {
    cashTop = c = c->prev;
    *A = c->a;
    *B = c->b;
    *C = c->c;
    *AplusB = c->ab;
    c->anchor = NOVERTEX;
    c->o = origin;
    c->f = from;
    c->t = to;
    return 0;
  }
}

/*--------------------------------------------------------------------------*/

static int hsSlopeCashGetAnchor ( indexType *anchor)

{
  return ((*anchor = cashTop->anchor) != NOVERTEX);
}

/*--------------------------------------------------------------------------*/

static int hsSlopeCashPutAnchor ( indexType anchor)

{
  return cashTop->anchor = anchor;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static void hsComputeSlope (

     indexType origin,indexType from,indexType to,
     Lia_ptr *A,Lia_ptr *B,Lia_ptr *C,Lia_ptr *AplusB)

{
  if (hsSlopeCashGet (origin, from, to, A, B, C, AplusB)) 
    return;

  if ((origin == from) || (origin == to) || (from == to)) {
    lia_load (*A, (int) 0);
    lia_load (*B, (int) 0);
    lia_load (*C, (int) 0);
    lia_load (*AplusB, (int) 0);
  } else {
    lia_assign (*A, DET31 (origin, to, from, 2, 3));
/*    lia_mul (tmp1, *A, *A);*/

    lia_push (*A); 
    lia_pushtop ();
    lia_times ();
    lia_pop (tmp1);

    lia_assign (*B, DET31 (origin, to, from, 3, 1));
/*    lia_mul (tmp2, *B, *B);*/

    lia_push (*B); 
    lia_pushtop ();
    lia_times ();
    lia_pop (tmp2);

    lia_add (*AplusB, tmp1, tmp2);
    lia_assign (tmp1, DET31 (origin, to, from, 1, 2));
/*    lia_mul (*C, tmp1, tmp1);*/

    lia_push (tmp1);
    lia_pushtop ();
    lia_times ();
    lia_pop (*C);

  }
}

/*--------------------------------------------------------------------------*/

static int hsTestIfAnchor (

     Lia_ptr A,Lia_ptr B,
     indexType candidate,indexType v1,indexType v2)

{
  if (v1 == candidate) return (candidate >= v2);

  lia_assign (tmp1, DET21 (v1, candidate, 1));
  lia_mul (tmp2, tmp1, B);
  lia_assign (tmp1, DET21 (v1, candidate, 2));
  lia_mul (tmp3, tmp1, A);
  lia_sub (tmp1, tmp3, tmp2);

  if (ISZERO (tmp1)) return (candidate >= MYMAX (v1, v2));

  if (v2 == candidate) return (candidate >= v1);

  lia_assign (tmp4, DET21 (v2, candidate, 1));
  lia_mul (tmp2, tmp4, B);
  lia_assign (tmp4, DET21 (v2, candidate, 2));
  lia_mul (tmp3, tmp4, A);
  lia_sub (tmp4, tmp3, tmp2);

  if (ISZERO (tmp4)) return (candidate >= MYMAX (v1, v2));

  return (ISPOSITIVE (tmp4) != ISPOSITIVE (tmp1));
}

/*--------------------------------------------------------------------------*/

static int hsComputeAnchor (

     Lia_ptr A,Lia_ptr B,
     indexType origin,indexType from,indexType to)

{
  indexType tmp;

  if (hsSlopeCashGetAnchor (&tmp))
    return tmp;

  if (hsTestIfAnchor (A, B, origin, from, to)) {
    hsSlopeCashPutAnchor (origin);
    return origin;
  }

  if (hsTestIfAnchor (A, B, from, origin, to)) {
    hsSlopeCashPutAnchor (from);
    return from;
  }

  hsSlopeCashPutAnchor (to);
  return to;
}

/*--------------------------------------------------------------------------*/

int hsSlopeRightTurn (

     graphType *g,
     indexType origin,indexType from,indexType to)

{
  return haAngleGT180 (g, origin, from, to);
}

/*--------------------------------------------------------------------------*/

int hsSlopeGT (

     graphType *g,
     indexType origin1,indexType from1,indexType to1,
     indexType origin2,indexType from2,indexType to2)

{
  Lia_ptr A1, B1, C1, AplusB1, A2, B2, C2, AplusB2;
  indexType anchor;

  hsComputeSlope (origin1, from1, to1, &A1, &B1, &C1, &AplusB1);

  if (((origin1 == from2) && (from1 == to2) && (to1 == origin2)) ||
      ((origin2 == from1) && (from2 == to1) && (to2 == origin1))) {
    
    anchor = hsComputeAnchor (A1, B1, origin1, from1, to1);

    if (anchor == origin1) return 1;
    if (anchor == origin2) return 0;
    if (origin1 > origin2) return 1;
    if (origin2 > origin1) return 0;
    return (from1 > from2);
  } 

  hsComputeSlope (origin2, from2, to2, &A2, &B2, &C2, &AplusB2);
  
  if (! (ISZERO (AplusB1) && ISZERO (C1) && ISZERO (AplusB2) && ISZERO (C2))) {

    if (ISZERO (AplusB1) && ISZERO (C1)) return 0;
    if (ISZERO (AplusB2) && ISZERO (C2)) return 1;

    lia_mul (tmp1, AplusB1, C2);
    lia_mul (tmp2, AplusB2, C1);

    if (lia_le (tmp2, tmp1)) return 1;
    if (lia_le (tmp1, tmp2)) return 0;
  }

  if (origin1 > origin2) return 1;
  if (origin2 > origin1) return 0;
  if (from1 > from2) return 1;
  if (from2 > from1) return 0;
  if (to1 > to2) return 1;
  if (to2 > to1) return 0;

  fprintf (stdout,"ERROR: Two Slopes have the same size!!??\n");
  fprintf (stdout,"%d %d %d %d %d %d\n", origin1, from1, to1, origin2, from2, to2);
  exit (1);

  return 1;
}

/*--------------------------------------------------------------------------*/

void
hsPrintSlope (fp, oi, ox, oy, oz, fi, fx, fy, fz, ti, tx, ty, tz)

     FILE      *fp;
     coordType ox, oy, oz, fx, fy, fz, tx, ty, tz;
     indexType oi, fi, ti;

{
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/



