/**** poly.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"

struct poly_t *
new_poly_t ()
{
    struct poly_t *Poly;

    Poly = (struct poly_t *) G_malloc (sizeof (struct poly_t));
    Poly->n_alloced = 0;
    Poly->n_polys = 0;
    Poly->spoly = NULL;
    /*Poly->Area = NULL;*/

    /*
    Poly->Points = Vect_new_line_struct ();
    Poly->info = NULL;
    Poly->num = 0;
    Poly->pnum = 0;
    Poly->n_alloced = 0;
    */

    return Poly;
}

/* Load poly_t structure with pertinent info */   
/* returns 0 or -1 on out of memory */
load_poly (Map, poly, P)
    struct Map_info *Map;
    plus_t poly;
    struct poly_t *P;
{
    register int i, j, k;
    P_AREA *Area;
    P_ISLE *Isle;
    plus_t isle;
    int n_lines;
    int line;
    int ret;
    int n_polys;

    Area = &(Map->Area[poly]);

    n_polys = Area->n_isles + 1;

    if (0 > alloc_poly_t (P, n_polys, 1))	/* allocate for each subpoly */
	return -1;
    
    P->n_polys = n_polys;

    P->spoly[BASE_POLY].num = Area->n_lines;
    P->spoly[BASE_POLY].Area = Area;
    P->spoly[BASE_POLY].Points->n_points = 0;	/* re-init Poly->Points data */

    /* allocate for each line */
    if (0 > alloc_poly_t_lines (&(P->spoly[BASE_POLY]), Area->n_lines)) 
	return -1;

    /* MAIN polygon */
    isle = 0;
    for (i = 0 ; i < Area->n_lines ; i++)
    {
	line = Area->lines[i];

	/*  Depends on V2_read_line_poly_order not using TPoints */
	ret = V2_read_line_poly_order (Map, TPoints, line, poly);
	if (ret == -2 || ret == 0)	/* todo  not really accurate */
	{
	    return dig_out_of_memory ();
	}

	/* skip last point on each line */
	for (j = 0 ; j < TPoints->n_points - 1 ; j++)
	    Vect_append_point (P->spoly[BASE_POLY].Points, 
				    TPoints->x[j], TPoints->y[j]);
	P->spoly[BASE_POLY].info[i].n_verts = TPoints->n_points;
	P->spoly[BASE_POLY].info[i].dir  = line > 0 ? FORWARD : REVERSE;
    }

    /* TODO  TEST  this is test code.  I am hoping it doesn't break anything 
    **  the number of points in the polygon changes with this line
    **  and if anything was using that, this could break it.  Guess we'll
    **  find out...
    */

    /* last line  go ahead and write last point, which hopefully is 1st point */
#define FULL_POLY
#ifdef FULL_POLY
    Vect_append_point (P->spoly[BASE_POLY].Points, 
			    TPoints->x[j], TPoints->y[j]);
#endif


    /* islands of polygon */
    for (j = 0 ; j < Area->n_isles ; j++)
    {
	isle = Area->isles[j];
	Isle = &(Map->Isle[isle]);

	n_lines = Isle->n_lines;

	P->spoly[j+1].num = n_lines;
	P->spoly[j+1].Area = (P_AREA *) Isle; /* force info P_AREA */
	P->spoly[j+1].Points->n_points = 0; /* re-init Poly->Points data*/
	if (0 > alloc_poly_t_lines (&(P->spoly[j+1]), n_lines))
	    return -1;

	for (i = 0 ; i < n_lines ; i++)
	{
	    line = Isle->lines[i];

	    /*  This line should work too.
	    ret = V2_read_line_poly_order (Map, TPoints, line, isle);
	    */

	    ret = V2_read_line_poly_order (Map, TPoints, line, poly);
	    if (ret == -2 || ret == 0)	/* todo  not really accurate */
	    {
		return dig_out_of_memory ();
	    }

	    /* skip last point on each line */

		/* use j+1 cuz we want to bump islands up to 1->N 
		** to make room for Area in slot 0
		*/
	    for (k = 0 ; k < TPoints->n_points - 1 ; k++)
		Vect_append_point (P->spoly[j+1].Points, 
		                      TPoints->x[k], TPoints->y[k]);

	    P->spoly[j+1].info[i].n_verts = TPoints->n_points;
	    P->spoly[j+1].info[i].dir  = line > 0 ? FORWARD : REVERSE;
	}

	/* TODO TEST last line  go ahead and write last point, which 
	    hopefully is 1st point */
#ifdef FULL_POLY
	Vect_append_point (P->spoly[j+1].Points, 
				TPoints->x[k], TPoints->y[k]);
#endif

    }

    return 0;
}


/* 
**  When building the poly_vert_info from an intersection,
**  if that intersection is on a VERTEX of A, then we really need
**  to start with the beginning of the next line segment.
**
**
** returns 0 or -1 if not at vertex
*/
bump_vert_to_next_line (P, Vert)
    struct sub_poly *P;
    struct poly_vert_info *Vert;
{
    /*
    struct sub_poly *P;

    P = &(Poly->spoly[Vert->subpoly]);
    */
    if (Vert->pos != P->info[Vert->line].n_verts-1)
	return -1;

    Vert->pos = 0;
    Vert->line = (Vert->line+1) % P->num;


    /* bump ppos while we're at it. */
#ifdef FULL_POLY
    if (Vert->ppos >= P->Points->n_points-1)
	Vert->ppos = 0;
#else
    if (Vert->ppos >= P->Points->n_points)
	Vert->ppos = 0;
#endif

/*DEBUG*/    if (Vert->ppos > P->Points->n_points)
/*DEBUG*/ fprintf (stderr, "ERROR ppos is GREATER than npoints. ppos: %d  n_points: %d\n", Vert->ppos, P->Points->n_points);

    return 0;
}





#ifdef POLY_NEXT_VERT
/* given poly_vert_info struct, return info for next point on poly.
**  if point is non-unique, the returned line will be same as old
**
**  Returns verts 0 -> N-2.  
**
**   returns 1 if point is non-unique  0 if unique
**
**
**
**  **** WARNING ****  This code has never been tested nor varified.
**
**  	and has not been updated for islands
*/

int
poly_next_vert (poly, old, next)
    struct poly_t *poly;
    struct poly_vert_info *old, *next;
{
    int oline;
    int ret;

    oline = old->line;

    next->poly_code = old->poly_code;
    next->poly = old->poly;
    next->midline = 0;
    if (old->pos  < (poly->info[oline].n_verts - 2) )
    {
	next->line = oline;
	next->pos = old->pos + 1;
	next->ppos = old->ppos + 1;
	ret = 0;
    }
    else
    {
	next->line = (oline + 1) % poly->num;
	next->pos = 0;
	next->ppos = (old->ppos + 1) % poly->Points->n_points;
	ret = 1;
    }
    next->x = poly->Points->x[next->ppos];
    next->y = poly->Points->y[next->ppos];

    return ret;
}

/* given poly_vert_info struct, return info for prev point on poly.
**  if point is non-unique, the returned line will be same as old
**
**  returns points N-2 -> 1
** 
**   returns 1 if point is non-unique  0 if unique
*/
int
poly_prev_vert (poly, old, prev)
    struct poly_t *poly;
    struct poly_vert_info *old, *prev;
{
    int oline;	/* line offset in poly array */
    int ret;

    oline = old->line;

    prev->poly = old->poly;
    prev->poly_code = old->poly_code;
    prev->midline = 0;
    if (old->pos  > 1)
    {
	prev->line = oline;
	prev->pos = old->pos - 1;
	prev->ppos = old->ppos - 1;
	ret = 0;
    }
    else
    {
	prev->ppos = ring_mod (old->ppos-1, poly->Points->n_points);
	prev->line = ring_mod (oline-1,     poly->num);
	prev->pos  = poly->info[prev->line].n_verts - 1;

	ret = 1;
    }

    prev->x = poly->Points->x[prev->ppos];
    prev->y = poly->Points->y[prev->ppos];
    return ret;
}
#endif
