/**** lsort.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"

/* use static externals to pass extra sort criteria to compare routine */
static int Code;		/* Active code */

/*
**  Code to sort intersections along a line 
**
** Note that a, b have nothing to do with map A, map B.  In fact both 
**  intersections will be on same map, same line.
**
** Sort Criteria:
**   - Same intersection?  sort OUT->IN order
**   - increasing line number
**     - increasing segment number
**       - increasing distance from beginning of line segment
**
*/
static int 
inter_compare (a, b)
  struct t_data **a;
  struct t_data **b;
{
  double x1dist, y1dist;
  double x2dist, y2dist;
  double adist, bdist;

  /* if same intersection, then sort  OUT -> IN  order */
  if ((*a)->inter == (*b)->inter)
  {
/*DEBUG*/  if ((*a)->in_out == (*b)->in_out)
/*DEBUG*/    fprintf (stderr, "inter_compare  a->in_out == b->in_out (%d,%d,%d) (%d,%d,%d)  %d\n", (*a)->i[Code].poly, (*a)->i[Code].line, (*a)->i[Code].segment, (*b)->i[Code].poly, (*b)->i[Code].line, (*b)->i[Code].segment, (*a)->in_out);


    {
	if ((*a)->in_out == OUT)
	  return -1;
	else 
	  return 1;
    }
  }

  /* sort by higher line number */
  if ((*a)->i[Code].line != (*b)->i[Code].line)
    return (*a)->i[Code].line - (*b)->i[Code].line;
  

  /* else line_a == line_b */
  /* sort by higher segment w/in line */
  if ((*a)->i[Code].segment != (*b)->i[Code].segment)
    return (*a)->i[Code].segment - (*b)->i[Code].segment;

  /* else line_a == line_b && segment_a == segment_b */


  /* convert address of subpoly to float * to extract dist data */
  adist   = *((float *) &((*a)->i[Code].subpoly));
  bdist   = *((float *) &((*b)->i[Code].subpoly));


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

/* ==LINE== */
sort_intersections_on_line (active, code)
  struct array_p *active;
  int code;
{
  Code = code;

  qsort (active->data, active->num, active->size, inter_compare);
}
