/**** sort.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"

/* use static externals to pass extra sort criteria to compare routine */
static struct poly_t *Poly;
static int Code;		/* Active code */

/*
**  Code to sort intersections around a polygon 
**
** Note that a, b have nothing to do with map A, map B.  In fact both 
**  intersections will be on same map, same poly.
**
** Sort Criteria:
**   - Same intersection?  sort OUT->IN order
**   - increasing line number
**     - increasing segment number
**       - increasing distance from beginning of line segment
**
*/
static int 
inter_compare (a, b)		/* ==ISLE== */
  struct t_data **a;
  struct t_data **b;
{
  int line;
  int segment;
  int pos;
  double x1dist, y1dist;
  double x2dist, y2dist;
  double adist, bdist;
  struct sub_poly *P;

  /* if same intersection, then sort  OUT -> IN  order */
  if ((*a)->inter == (*b)->inter)
  {
/*DEBUG*/  if ((*a)->in_out == (*b)->in_out)
/*DEBUG*/    fprintf (stderr, "inter_compare  a->in_out == b->in_out (%d,%d,%d) (%d,%d,%d)  %d\n", (*a)->i[Code].poly, (*a)->i[Code].line, (*a)->i[Code].segment, (*b)->i[Code].poly, (*b)->i[Code].line, (*b)->i[Code].segment, (*a)->in_out);


    if (Code == A_CODE)
    {
	if ((*a)->in_out == OUT)
	  return -1;
	else 
	  return 1;
    }
    else		/* for B_CODE have to reverse in_out flag */
    {
	if ((*a)->in_out == IN)
	  return -1;
	else 
	  return 1;
    }
  }

  /* sort by higher subpoly number */
  if ((*a)->i[Code].subpoly != (*b)->i[Code].subpoly)
    return (*a)->i[Code].subpoly - (*b)->i[Code].subpoly;

  /* sort by higher line number */
  if ((*a)->i[Code].line != (*b)->i[Code].line)
    return (*a)->i[Code].line - (*b)->i[Code].line;
  

  /* else line_a == line_b */
  /* sort by higher segment w/in line */
  if ((*a)->i[Code].segment != (*b)->i[Code].segment)
    return (*a)->i[Code].segment - (*b)->i[Code].segment;

  /* else line_a == line_b && segment_a == segment_b */

  /* sort by distance from vertex w/in segment */
  line    = (*a)->i[Code].line;
  segment = (*a)->i[Code].segment;
  
  P = &(Poly->spoly[(*a)->i[Code].subpoly]);


  /*   segment is 1 -> N */
  pos     = sub_poly_line_start_pos (P, line) + segment-1;
  x1dist  = (*a)->x - P->Points->x[pos];
  y1dist  = (*a)->y - P->Points->y[pos];
  x2dist  = (*b)->x - P->Points->x[pos];
  y2dist  = (*b)->y - P->Points->y[pos];

  adist   = x1dist * x1dist + y1dist * y1dist;
  bdist   = x2dist * x2dist + y2dist * y2dist;

#ifdef FOO
/*DEBUG*/ 
if (Code == A_CODE)
{
    if ((*a)->i[Code].line == 0 && (*a)->i[Code].segment == 2)
    {
	fprintf (stderr, "Sort: line %d  seg %d  poly_pos %d  pos %d adist %f bdist %f\n", line, segment, sub_poly_line_start_pos (P, line), pos, adist, bdist);

	dump_poly (Poly);
    }
}
#endif

/*DEBUG*/ 
  if (adist == bdist)
  {
/*DEBUG*/    debugf ("inter_compare Points are the same (%d,%d,%d) (%d,%d,%d)\n", (*a)->i[Code].poly, (*a)->i[Code].line, (*a)->i[Code].segment, (*b)->i[Code].poly, (*b)->i[Code].line, (*b)->i[Code].segment);
    return 0;
  }

  if (adist > bdist)
    return 1;
  else
    return -1;
}

sort_intersections_on_poly (active, P, code)
  struct array_p *active;
  struct poly_t *P;
  int code;
{
  Poly = P;
  Code = code;

  qsort (active->data, active->num, active->size, inter_compare);
}

