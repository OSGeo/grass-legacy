/**** point.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include <stdlib.h>
#include <math.h>
#include "Vect.h"

#include "cutter.h"

#define  LABEL
#ifdef LABEL

#define TGL(x) ((x) = !(x))


/*
**
** Cut_get_point_in_area (Map, area, x, y)  get point inside area
** Cut_get_point_in_poly (Points, X, Y)     get point inside polygon
**
**
**  Cut_get_point_in_area()
**
**     Take a line and intersect it with the polygon and any islands.
**     sort the list of X values from these intersections.  This will
**     be a list of segments alternating  IN/OUT/IN/OUt of the polygon.
**     Pick the largest IN segment and take the midpoint. 
**   
*/

static int comp_ipoints ();
static int V__within ();
int Cut__intersect_line_with_poly ();

/*
#define DEBUG2
#define DEBUG3
*/

/* returns 0 on success and -1 on failure */
int 
Cut_get_point_in_poly_t (struct poly_t *Poly, double *X, double *Y)
{
    static int first_time = 1;
    static struct intersects I;

    double cent_x, cent_y;
    register int i;
    double max;
    int maxpos;
    int ret;
    P_AREA *Area;
    double diff;
    int level;
    int cpoly;

    if (first_time)
    {
	I.n_points = 0;
	I.n_alloced = 0;
	I.points = NULL;
	first_time = 0;
    }

    /*
    **  Get centroid of poly, and check that it is:
    **    A) within poly
    **    B) not within any of the islands
    **
    **  If so, then we are done
    */
    Cut_find_poly_centroid (Poly->spoly[BASE_POLY].Points, &cent_x, &cent_y);	/* done */
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "Checkpoint 01  %lf %lf\n", cent_x, cent_y);
#endif
	/* is it w/in poly? */
    if (dig_point_in_poly (cent_x, cent_y, Poly->spoly[BASE_POLY].Points))	/* done */
    {
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "Checkpoint 02\n");
#endif
	if (!Cut_point_in_islands (Poly, cent_x, cent_y))	/* done */
	{
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "Checkpoint 02B\n");
#endif
	    *X = cent_x;
	    *Y = cent_y;
	    return 0;
	}
    }
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "Checkpoint 03\n");
#endif

/* guess we have to do it the hard way... */

    *Y = cent_y;   /* pick line segment (x_min, cent_y) - (x_max, cent_y) */

    I.n_points = 0;		/* re-init Points */

    /* add in intersections w/ all polys */
    for (i = 0 ; i < Poly->n_polys ; i++)
    {
	if (0 > Cut__intersect_line_with_poly (Poly->spoly[i].Points, *Y, &I, i))
	    return -1;					/* done */
    }

    qsort (I.points, I.n_points, sizeof (struct ipoints), comp_ipoints); /* done*/


#ifdef DEBUG3
/*DEBUG*/ 

	fprintf (stdout," \n");
	for (i = 0 ; i < I.n_points ; i++)
	    fprintf (stdout, "%lf %lf  I\n", I.points[i].x, *Y);
	fprintf (stdout," \n");
/*DEBUG*/ 
#endif

    max = 0;
    maxpos = -1;
    level = 0;

    /* init num.  I am going to use it to keep track of parity
    **  for each poly.  It was not being used for Opoly
    **
    **  0 means OUT  1 means in
    */
    for (i = 0 ; i < Poly->n_polys ; i++)
    {
	Poly->spoly[i].num = 0;
    }

    /* find area of MAX distance */
    /* for (i = 0 ; i < I.n_points - 1 ; i+=2) */
    for (i = 0 ; i < I.n_points - 1 ; i ++)
    {
	cpoly = I.points[i].poly;
	TGL (Poly->spoly[cpoly].num);

	if (cpoly > 0)
	{
	    if (Poly->spoly[cpoly].num)
		level--;
	    else
		level++;
	}
	else	/* main poly */
	{
	    if (Poly->spoly[cpoly].num)
		level++;
	    else
		level--;
	}

	if (level > 0)
	{
	    diff = I.points[i+1].x - I.points[i].x;

	    if (diff > max)
	    {
		max = diff;
		maxpos = i;
	    }
	}
    }

    if (maxpos < 0)
    {
/*DEBUG*/ debugf ("Error in Cut_get_point_in_poly_t (Poly, X, Y)\n");
	return -1;
    }

    *X = (I.points[maxpos].x + I.points[maxpos+1].x) / 2.;

#ifdef DEBUG3
/*DEBUG*/ fprintf (stderr, "Returning:  %lf %lf\n", *X, *Y);
#endif

    return 0;
}

static int 
comp_ipoints (	/* DONE */
    struct ipoints *i,
    struct ipoints *j
)
{
    if (i->x < j->x)
	return  -1;

    if (i->x > j->x)
	return 1;

    return 0;
}

static int 
V__within (double a, double x, double b)
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
**  Note that if Y equals the endpoint of a line segment, we will 
**   effectively bump the end of the line up a hair so that all such lines
**   reaching Y from above will miss and all comming from below will cross.
**
**  Note that I don't really change any numbers, but just effect that by
**  the logic.
**
** returns 0  or  -1 on error 
*/
int 
Cut__intersect_line_with_poly (	/* DONE */
    struct line_pnts *Points,	/* polygon points */
    double y,			/* Y of intersecting line */
    struct intersects *I,	/* struct w/ array of intersections */
    int poly			/* current poly being intersected */
)
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

	/* TODO.  Check case were line comes up/touches and goes down */
	if (V__within (a, y, b))
	{
	    /* if a or b  == y, then shift it up a tad */
	    if (a == y || b == y)
	    {
		if (a == b) continue;	/* shift both up, no intersection */

		if ( (a == y && b > y) || (b == y && a > y) ) /* high, */
		    continue;
		
		/* otherwise drop thru and count it */
	    }

	    perc = (y - a) / (b - a);
	    x = perc * (d - c) + c;		/* interp X */

	    if (0 > intersect_append_point (I, x, poly))
		return -1;
	}
    }
    return 0;
}





/* returns 0 or -1 on error */

int 
Cut_find_poly_centroid (	  /* DONE */
    struct line_pnts *points,
    double *cent_x,
    double *cent_y
)
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
int 
Cut_point_in_islands (	/* DONE */
    struct poly_t *Poly,
    double cent_x,
    double cent_y
)
{
    int i;

#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "Point in islands num_polys: %d\n", Poly->n_polys-1);
#endif
    for (i = 1 ; i < Poly->n_polys ; i++)
    {
	if (dig_point_in_poly (cent_x, cent_y, Poly->spoly[i].Points))
	    return 1;
    }

    return 0;
}

int 
intersect_append_point (	/* DONE */
    struct intersects *I,
    double x,
    int poly
)
{
    
    if (0 > alloc_intersections (I, I->n_points+1))
    {
      return dig_out_of_memory();
    }
    I->points[I->n_points].x = x;
    I->points[I->n_points].poly = poly;
    I->n_points++;

    return 0;
}



#endif

