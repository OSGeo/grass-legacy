/**** plane.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"

point_to_right_of_segment (A, p)
  struct line_t *A;
  struct point_t *p;
{
  double a, b, t;             /* constants in line equation */


  t = (A->p2.x - A->p1.x);

  if (t == 0.)
  {
    fprintf (stderr, "divisor = 0\n");
    return 0;
  }

  a = (A->p2.y - A->p1.y) / t;
  b = a * (-A->p2.x) + A->p2.y;

  /* decide which half point is in & assign triangle */
  return (a * p->x - p->y + b) > 0;
}
