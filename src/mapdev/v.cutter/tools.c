/**** tools.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"

#define lessor(a,b) ((a) < (b) ? (a) : (b))

seg_bboxes_cross (A, B)
    struct line_t *A, *B;
{
    double Ax1,Ax2,Ay1,Ay2;
    double Bx1,Bx2,By1,By2;

    /* first check if bboxes line cross */
    Ax1 = lessor (A->p1.x, A->p2.x);
	Ax2 = Ax1 == A->p2.x ? A->p1.x : A->p2.x;
    Ay1 = lessor (A->p1.y, A->p2.y);
	Ay2 = Ay1 == A->p2.y ? A->p1.y : A->p2.y;
    Bx1 = lessor (B->p1.x, B->p2.x);
	Bx2 = Bx1 == B->p2.x ? B->p1.x : B->p2.x;
    By1 = lessor (B->p1.y, B->p2.y);
	By2 = By1 == B->p2.y ? B->p1.y : B->p2.y;

    if (Ax1 > Bx2 || Ax2 < Bx1 || Ay1 > By2 || Ay2 < By1)
	return 0;

    return 1;
}


vertices_touch (A, B)
  struct line_t *A, *B;
{
  int ret = 0;

  if (A->p1.x == B->p1.x && A->p1.y == B->p1.y)  ret |= A1B1;
  if (A->p2.x == B->p2.x && A->p2.y == B->p2.y)  ret |= A2B2;

  if (ret) return ret;

  if (A->p2.x == B->p1.x && A->p2.y == B->p1.y)  ret |= A2B1;
  if (A->p1.x == B->p2.x && A->p1.y == B->p2.y)  ret |= A1B2;

  return ret;
}
