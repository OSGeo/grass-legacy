#include <math.h>
#include <stdlib.h>
#include "Vect.h"
#include "gis.h"
#include "linkm.h"

/*
**
** Vect_get_point_in_area (Map, area, x, y)  get point inside area
**                                           and outside all islands
** Vect_get_point_in_poly (Points, X, Y)     get point inside polygon
** Vect_get_point_in_poly_isl 
**        (Points, Ipoints, x,y,n_isles)     get point inside polygon
**                                           but outside the islands 
**                                           specifiled in IPoints.
**
**
**  Vect_get_point_in_area()
**
**     Take a line and intersect it with the polygon and any islands.
**     sort the list of X values from these intersections.  This will
**     be a list of segments alternating  IN/OUT/IN/OUt of the polygon.
**     Pick the largest IN segment and take the midpoint. 
**   
*/

struct Slink {
    double x;
    struct Slink *next;
};
	
static int comp_double ( double *,double *);
static int V__within (double,double,double);
int Vect__intersect_line_with_poly ();
static void destroy_links ( struct Slink *);
static int Vect__divide_and_conquer (struct Slink *, struct line_pnts *,
    struct link_head *, double *,double *, int);


/* returns 0 on success and -1 on failure */
int Vect_get_point_in_area (
    struct Map_info *Map, int area, double *X,double *Y)
{
    static struct line_pnts *Points;
    static struct line_pnts **IPoints;
    static int first_time = 1;
    static int isl_allocated = 0;
    register int i;

    if (first_time)
    {
	Points = Vect_new_line_struct ();
	IPoints = NULL;
	first_time = 0;
    }
    if(Map->Area[area].n_isles > isl_allocated)
    {
        IPoints = (struct line_pnts **)
	G_realloc(IPoints, (1+Map->Area[area].n_isles) * sizeof(struct line_pnts *));
        for(i=isl_allocated; i< Map->Area[area].n_isles; i++)
                 IPoints[i] = Vect_new_line_struct();
        isl_allocated = Map->Area[area].n_isles;
    }

    if (0 > Vect_get_area_points (Map, area, Points))
	return -1;

    for (i = 0 ; i < Map->Area[area].n_isles ; i++)
    {
	IPoints[i]->alloc_points = 0;
	if (0 > Vect_get_isle_points (Map, Map->Area[area].isles[i], IPoints[i]))
	    return -1;
    }
    return( Vect_get_point_in_poly_isl(Points, IPoints, Map->Area[area].n_isles, X, Y));

}

static int comp_double ( double *i,double *j)
{
    if (*i < *j)
	return  -1;

    if (*i > *j)
	return 1;

    return 0;
}

static int V__within ( double a,double x,double b)
{
    double tmp;

    if (a > b) { tmp = a; a = b; b = tmp; }

    return (x >= a && x <= b);
}

/*
**
**  For each intersection of a polygon w/ a line, stuff the 
**   X value in the Inter  Points array.  I used line_pnts, just
**   cuz the memory management was already there.  I am getting real
**   tired of managing realloc stuff.
**  Assumes that no vertex of polygon lies on Y
**  This is taken care of by functions calling this function
**
** returns 0  or  -1 on error 
*/
int Vect__intersect_line_with_poly (
    struct line_pnts *Points,
    double y,
    struct line_pnts *Inter)
{
    int i;
    double a, b, c, d, x;
    double perc;

    for (i = 1 ; i < Points->n_points ; i++)
    {
	a = Points->y[i-1];
	b = Points->y[i];

	c = Points->x[i-1];
	d = Points->x[i];

	if (V__within (a, y, b))
	{
	    if(a==b) continue;

	    perc = (y - a) / (b - a);
	    x = perc * (d - c) + c;		/* interp X */

	    if (0 > Vect_append_point (Inter, x, y))
		return -1;
	}
    }
    return 0;
}

/*
** This does NOT consider ISLANDS !!! 
**
** returns 0 on success and -1 on failure 
*/
int Vect_get_point_in_poly ( struct line_pnts *Points, double *X,double *Y)
{
    double cent_x, cent_y;
    struct Slink *Head;
    static struct link_head *Token;
    struct Slink *tmp;
    static int first_time = 1;
    register int i;
    double x_max, x_min;
    int ret;

	/* get centroid */
    Vect_find_poly_centroid (Points, &cent_x, &cent_y);
	/* is it w/in poly? */
    if (dig_point_in_poly (cent_x, cent_y, Points))
    {
	*X = cent_x;
	*Y = cent_y;
	/*
	return 0;
	*/
    }

/* guess we have to do it the hard way... */
    /* get min and max x values */
    x_max = x_min = Points->x[0];
    for (i = 0 ; i < Points->n_points ; i++)
    {
	if (x_min > Points->x[i]) x_min = Points->x[i];
	if (x_max < Points->x[i]) x_max = Points->x[i];
    }


/* init the linked list */
    if (first_time)
    {
	/* will never call link_cleanup ()  */
	link_exit_on_error (1);	/* kill program if out of memory */
	Token = (struct link_head *) link_init (sizeof (struct Slink));
	first_time = 0;
    }

    Head = (struct Slink *) link_new (Token);
    tmp = (struct Slink *) link_new (Token);

    Head->next = tmp;
    tmp->next = NULL;

    Head->x = x_min;
    tmp->x = x_max;

    *Y = cent_y;   /* pick line segment (x_min, cent_y) - (x_max, cent_y) */
    ret = Vect__divide_and_conquer (Head, Points, Token, X, Y, 10);

    destroy_links (Head);

    if (ret < 0)
    {
	fprintf (stderr, "Could not find point in polygon\n");
	return -1;
    }

/*DEBUG fprintf (stderr, "Found point in %d iterations\n", 10 - ret); */

    return 0;
}


/*
** provide a breadth first binary division of real space along line segment
**  looking for a point w/in the polygon.
**
**  This routine walks along the list of points on line segment
**  and divides each pair in half. It sticks that new point right into
**  the list, and then checks to see if it is inside the poly. 
**
**  after going through the whole list, it calls itself.  The list 
**   now has a whole extra set of points to divide again.
**
**  returns # levels it took  or -1 if exceeded # of levels
*/
static int Vect__divide_and_conquer (
    struct Slink *Head,
    struct line_pnts *Points,
    struct link_head *Token,
    double *X,double *Y,
    int levels)
{
    struct Slink *A, *B, *C;

/*DEBUG fprintf (stderr, "		LEVEL %d\n", levels); */
    A = Head;
    B = Head->next;

    do {
	C = (struct Slink *) link_new (Token);
	A->next = C;
	C->next = B;

	C->x = (A->x + B->x) / 2.;

	if (dig_point_in_poly (C->x, *Y, Points))
	{
	    *X = C->x;
	    return levels;
	}

	A = B;
	B = B->next;
    } while (B != NULL);

    /*
    **  If it got through the entire loop and still no hits,
    **   then lets go a level deeper and divide again.
    */

    if (levels <= 0)
	return -1;

    return Vect__divide_and_conquer (Head, Points, Token, X, Y, --levels);
}


static void destroy_links ( struct Slink *Head)
{
    struct Slink *p, *tmp;

    p = Head;

    while (p != NULL)
    {
	tmp = p->next;
	link_dispose ((struct link_head *) Head,(VOID_T *) p);
	p = tmp;
    }
}

/* returns 0 or -1 on error */

int Vect_find_poly_centroid (
    struct line_pnts *points,
    double *cent_x,double *cent_y)
{
    int i;
    double *xptr1, *yptr1;
    double *xptr2, *yptr2;
    double cent_weight_x, cent_weight_y;
    double len, tot_len;

    tot_len = 0.0;
    cent_weight_x = 0.0;
    cent_weight_y = 0.0;

    xptr1 = points->x;
    yptr1 = points->y;
    xptr2 = points->x + 1;
    yptr2 = points->y + 1;

    for(i=1; i<points->n_points; i++)
    {
	len = hypot(*xptr1-*xptr2, *yptr1-*yptr2);
	cent_weight_x += len * ((*xptr1 + *xptr2) / 2.);
	cent_weight_y += len * ((*yptr1 + *yptr2) / 2.);
	tot_len += len;
	xptr1++ ; xptr2++ ; yptr1++; yptr2++;
    }

    if (tot_len == 0.0)
	return -1;

    *cent_x = cent_weight_x / tot_len;
    *cent_y = cent_weight_y / tot_len;
    
    return 0;
}

/*
** returns true if point is in any of islands /w in area
** returns 0 if not
** returns -1 on error
*/
int Vect_point_in_islands (
    struct Map_info *Map,
    int area,
    double cent_x,double cent_y)
{
    P_AREA *Area;
    static struct line_pnts *TPoints;
    static int first_time = 1;
    int isle;

    if (first_time == 1)
    {
	TPoints = Vect_new_line_struct ();
	first_time = 0;
    }

    Area =  &(Map->Area[area]) ;

    for (isle = 0 ; isle < Area->n_isles ; isle++)
    {
	if (0 > Vect_get_isle_points (Map, Area->isles[isle], TPoints))
	    return -1;
	
	if (dig_point_in_poly (cent_x, cent_y, TPoints))
	    return 1;
    }

    return 0;
}



/*
** Vect_get_point_in_poly_isl (APoints, IPoints, n_isles, X, Y)     get point inside polygon
**
**  Vect_get_point_in_poly_isl()
**
**     Take a line and intersect it with the polygon and any islands.
**     sort the list of X values from these intersections.  This will
**     be a list of segments alternating  IN/OUT/IN/OUt of the polygon.
**     Pick the largest IN segment and take the midpoint. 
*/

int Vect_get_point_in_poly_isl(
struct line_pnts *Points,struct line_pnts **IPoints,
int n_isles,
double *att_x,double *att_y)


/* returns 0 on success and -1 on failure */
{
    static struct line_pnts *Intersects;
    double cent_x, cent_y;
    register int i, j;
    double max, hi_y, lo_y;
    int maxpos;
    int point_in_sles=0, first_time=1;
    double diff;

    if(first_time)
    {
      Intersects = Vect_new_line_struct();
      first_time = 0;
    }

    if (Points->n_points < 3)		/* test */
    {
	if (Points->n_points > 0)
	{
	    *att_x = Points->x[0];
	    *att_y = Points->y[0];
	    return 0;
	}
	return -1;	
    }

	/* get centroid */
    Vect_find_poly_centroid (Points, &cent_x, &cent_y);
	/* is it w/in poly? */
    if (dig_point_in_poly (cent_x, cent_y, Points))
    /* if the point is iside the polygon */
    {
	for(i=0;i<n_isles; i++)
	{
        if (dig_point_in_poly (cent_x, cent_y, IPoints[i])>0.0)
	    point_in_sles=1;
	}
	if(!point_in_sles) 
	{
	    *att_x = cent_x;
	    *att_y = cent_y;
	    return 0;
	}
    }
/* guess we have to do it the hard way... */

    /* first find att_y close to cent_y so that no points lie on the line */
    /* find the point closest to line from below, and point close to line
       from above and take average of their y-coordinates */

    /* first initializing lo_y,hi_y to be any 2 pnts on either side of cent_y */
    hi_y = cent_y -1;
    lo_y = cent_y +1;
    for(i=0;i<Points->n_points;i++)
    {
      if((lo_y<cent_y)&&(hi_y>=cent_y)) break; /* already initialized */
      if(Points->y[i] < cent_y)
	 lo_y = Points->y[i];
      if(Points->y[i] >= cent_y)
	 hi_y = Points->y[i];
    }
    /* first going throught boundary points */
    for(i=0;i<Points->n_points;i++)
    {
      if((Points->y[i] < cent_y)&&((cent_y - Points->y[i]) < (cent_y - lo_y)))
	 lo_y = Points->y[i];
      if((Points->y[i] >= cent_y)&&((Points->y[i] - cent_y) < (hi_y - cent_y)))
	 hi_y = Points->y[i];
    }
    for (i = 0 ; i < n_isles ; i++)
      for(j = 0 ; j < IPoints[i]->n_points; j++)
      {
         if((IPoints[i]->y[j] < cent_y)&&
	    ((cent_y - IPoints[i]->y[j]) < (cent_y - lo_y)))
	          lo_y = IPoints[i]->y[j];

         if((IPoints[i]->y[j] >= cent_y)&&
	    ((IPoints[i]->y[j] - cent_y) < (hi_y - cent_y)))
	          hi_y = IPoints[i]->y[j];
      }
	 
    if(lo_y == hi_y) return (-1); /* area is empty */
    else  *att_y = (hi_y + lo_y)/2.0;

    Intersects->n_points = 0;
    Vect__intersect_line_with_poly (Points, *att_y, Intersects);

    /* add in intersections w/ holes */
    for (i = 0 ; i < n_isles ; i++)
    {
	if (0 > Vect__intersect_line_with_poly (IPoints[i], *att_y, Intersects))
	    return -1;
    }

    if (Intersects->n_points < 2)	/* test */
	return -1;

    qsort (Intersects->x, Intersects->n_points, sizeof (double), comp_double);

    max = 0;
    maxpos = 0;

    /* find area of MAX distance */
    for (i = 0 ; i < Intersects->n_points ; i += 2)
    {
        diff = 	Intersects->x[i+1] - Intersects->x[i];

	if (diff > max)
	{
	    max = diff;
	    maxpos = i;
	}
    }
    if (max==0.0)  /* area was empty: example ((x1,y1), (x2,y2), (x1,y1)) */
	  return -1;

    *att_x = (Intersects->x[maxpos] + Intersects->x[maxpos+1]) / 2.;

    /* if (dig_point_in_poly (*att_x, *att_y, Points)==0.0)*/
    return 0;
}

