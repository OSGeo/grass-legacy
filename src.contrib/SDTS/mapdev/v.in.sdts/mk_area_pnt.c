#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "sdts_in.h"
#include "gis.h"
#include "Vect.h"
#include "stc123.h"
#include  "sdts_globals.h"

static short first_time;
static int max_poly_num = 0;

struct poly_point {
  char id[16];
  double sectY;
  struct line_pnts *inter;
} *poly_list = NULL;

struct pnts {
  double x;
  double y;
};

void
init_areapoint ()
{
  int i;

  if (max_poly_num)
    for ( i = 0; i < max_poly_num && poly_list[i].inter ; i++)
      {
	poly_list[i].id[0] = '\0';
	poly_list[i].sectY = 0;
	Vect_destroy_line_struct (poly_list[i].inter);
	poly_list[i].inter = 0;
      }
}


int
compute_areapoint ( le, cnt  )
     struct Sdts_LE *le;
     int cnt;
{
  register double *px, *py;
  double hi, lo;
  struct poly_point *ppl, *ppr;
  int i;

  if (max_poly_num < le->pidl || max_poly_num < le->pidr)
    {
      int tmp;
      
      tmp = le->pidl>le->pidr ? le->pidl: le->pidr;
      if (max_poly_num == 0)
	{
	  poly_list=(struct poly_point *)dig__falloc(tmp, sizeof(struct poly_point));
	  if (poly_list == NULL)
	    {
	      fprintf (stderr, "Out of memory when computing area points.\n");
	      exit (0);
	    }
	}
      else
	{
	  poly_list=(struct poly_point *)dig__frealloc(poly_list, tmp, sizeof(struct poly_point), max_poly_num);
	  if (poly_list == NULL)
	    {
	      fprintf (stderr, "Out of memory when computing area points.\n");
	      exit (0);
	    }
	}
      max_poly_num = tmp;
    }

  if (le->pidl == le->pidr)
    return 0;

  ppl = le->pidl > 0 ? &poly_list [le->pidl-1] : 0;
  ppr = le->pidr > 0 ? &poly_list [le->pidr-1] : 0;

  if (ppl && !ppl->id[0])
    sprintf (ppl->id, "%s_%d", le->pidl_modn, le->pidl); 

  if (ppr && !ppr->id[0])
    sprintf (ppr->id, "%s_%d", le->pidr_modn, le->pidr); 

  px = le->X;
  py = le->Y;

  if ((ppl && !ppl->sectY) || (ppr && !ppr->sectY))
    {
      /* In fact the following code is not really necessary, all I need
	 here is a horizontal line with at least one insection which is
	 not among the boundary points.  So I can just pick up a non-horzontal
	 line segment of this polygon line, and use its midpoint as the
	 intersection point.  The following code just tries to make this
	 point as central as possible.
	 */

      double mid;

      for (i = 1, lo = py[0], hi = py[0]; i < cnt ; i++ )
	{
	  if (lo > py[i])
	    lo = py[i];
	  else
	    if (hi < py[i])
	      hi = py[i];
	}

      if (lo == hi)
	return 0;

      mid = (lo + hi) / 2.0;

      for (i = 0 ; i < cnt ; i++ )
	{
	  if ((py[i] < mid) && ((mid - py[i]) < (mid - lo)))
	    lo = py[i];

	  if ((py[i] >= mid) && ((py[i] - mid) < (hi - mid)))
	    hi = py[i];
        }

      if (ppl && !ppl->sectY)
	ppl->sectY = (lo + hi) / 2.0;

      if (ppr && !ppr->sectY)
	ppr->sectY = (lo + hi) / 2.0;
    }
  
  px = le->X;
  py = le->Y;
  
  for (i = 1 ; i < cnt ; i++ )
    {

      register double a, b, c, d;

      a = py[i - 1];
      b = py[i];
      c = px[i - 1];
      d = px[i];

      if (a == b) continue;

      if (ppl && ((a > b && ppl->sectY >= b && ppl->sectY <= a) ||
	  (a < b && ppl->sectY <= b && ppl->sectY >= a)))
	/* this line (a,c)-(b,d) has a intersection with line Y = ppl->sectY */
	{
	  double perc;
          double att_x, att_y;
 
	  if (ppl->sectY == a)
	    {
	       att_x = c;
	       att_y = 0;
	    }
          else if (ppl->sectY == b)
	    {
	       att_x = d;
	       att_y = 0;
	    }
          else
	    {
	       perc = (ppl->sectY - a) / (b - a);
	       att_x = perc * (d - c) + c;
	       if (a > b)	/*  downwards */ 
	         att_y = ppl->sectY;
               else		/* upwards */
		 att_y = - ppl->sectY;
            }
          
	  if (!ppl->inter)
	    {
	      ppl->inter = Vect_new_line_struct ();
	      ppl->inter->n_points = 0;
	    }
	  if (Vect_append_point (ppl->inter, att_x, att_y) < 0)
	    return -1;

	}
	
      if (ppr && ((a > b && ppr->sectY >= b && ppr->sectY <= a) ||
	  (a < b && ppr->sectY <= b && ppr->sectY >= a)))
	/* this line (a,c)-(b,d) has a intersection with line Y = ppr->sectY */
	{
	  double perc = (ppr->sectY - a) / (b - a);
	  double att_x, att_y;
        
	  if (ppr->sectY == a)
	    {
	       att_x = c;
	       att_y = 0;
            }
          else if (ppr->sectY == b)
	    {
	       att_x = d;
	       att_y = 0;
            }
          else
	    {
	       perc = (ppr->sectY - a) / (b - a);
	       att_x = perc * (d - c) + c;
	       if (a > b)	/* downwards */
	         att_y = - ppr->sectY;
               else 		/* upwards */
		 att_y = ppr->sectY;
            }

	  if (!ppr->inter)
	    {
	      ppr->inter = Vect_new_line_struct ();
	      ppr->inter->n_points = 0;
	    }
	  if (Vect_append_point (ppr->inter, att_x, att_y) < 0)
	    return -1;

	}
    }

	return (1);
}


/*compare function modified to use void * parameters instead of struct * */

static int
comp_pnts (ci, cj)
     void  *ci, *cj;
{
  /*this:*/
  struct pnts *i =  (struct pnts *) ci;
  struct pnts *j =  (struct pnts *) cj;

  if (i->x < j->x)
    return -1;
  if (i->x > j->x)
    return 1;

  return 0;
}


#ifdef FOO /*the original compare function for qsort*/

static int
comp_pnts (i, j)
     struct pnts *i, *j;
{
  if (i->x < j->x)
    return -1;
  if (i->x > j->x)
    return 1;

  return 0;
}
#endif

#define MAX_INTERSECT 200

int
setup_areapoint ()
{
  double sectX, max;
  int maxpos, i, j, comp_pnts();
  struct pnts  *order_pnts;

#if DEBUG
  double *testX, *testY, *pX, *pY;

  pX = testX = (double *) dig__falloc (max_poly_num, sizeof (double));  
  pY = testY = (double *) dig__falloc (max_poly_num, sizeof (double));  
#endif

  order_pnts = (struct pnts *) dig__falloc (MAX_INTERSECT, sizeof (struct pnts));

  for ( i = 0 ; i < max_poly_num && poly_list[i].inter ; i ++)
    {
      register struct poly_point *pp = &poly_list[i];

      if (pp->inter->n_points < 2)
	/* Only find one intersection which should not happen */
       {
	  fprintf (stderr, "Internal error when caculate area point for poly %d\n", i+1);
	  exit (-1);
       }

      for (j = 0; j < pp->inter->n_points ; j ++)
        {
	  order_pnts[j].x = pp->inter->x[j];
	  order_pnts[j].y = pp->inter->y[j];
        }

      qsort(order_pnts, pp->inter->n_points, sizeof(struct pnts), comp_pnts);

      if (order_pnts[0].y)
	maxpos = 0;
      else if (order_pnts[pp->inter->n_points-1].y)
	maxpos = pp->inter->n_points - 2;
      else 
	{
	  for ( j = 1; j < pp->inter->n_points - 1 ; j ++)
            {
	      if (order_pnts[j].y > 0)
		{
		  maxpos = j;
		  break;
                }
              else if (order_pnts [j].y < 0)
		{
		  maxpos = j - 1;
		  break;
                }
            }
          if (j >= pp->inter->n_points - 1)
	    {
	      fprintf (stderr, "Internal error: no real intersection points for poly %d\n", i+1);
	      return (-1);
            }
        }


#if 0
      max = 0;
      maxpos = 0;

      for (j = 0 ; j < pp->inter->n_points; j += 2)
	{
	  double diff;

	  if (order_pnts[j].y == 0) j ++;

	  diff = order_pnts[j+1].x - order_pnts[j].x;

	  if (diff > max)
	    {
	      max = diff;
	      maxpos = j;
	    }
           
	}

      if (max == 0.0)
	return -1;
#endif

      sectX = (order_pnts[maxpos].x + order_pnts[maxpos+1].x) / 2.0;

#if DEBUG
      *pX++ = sectX;
      *pY++ = pp->sectY;
#endif

      add_area_pnt_to_list ("", pp->id, sectX, pp->sectY);
    }

#if DEBUG
    for (i = 0 ; i < max_poly_num - 1 ; i ++)
      for (j = i + 1 ; j < max_poly_num; j ++)
	if (testX[i] == testX[j] && testY[j] == testY[i])
	  printf ("Error: poly %d has the same areapoint as poly %d\n", i, j);
#endif

  free (order_pnts);

  return (1);
}
