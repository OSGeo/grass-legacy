/**** label.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"


/*
**  The base polygon itself has already been written one line at a time.
**   What needs to be done is to make sure each new island gets written
**   out one line at a time.
**  And to generate a label point for the polygon which avoids all islands 
**
**  I am using 'struct poly_t OPoly' to represent the new poly and
**  its babies.  To save memory, I am modifying the code so that 
**   sub_poly->Points does not have its own structures that I fill up,
**   but instead just use pointers which will be pointed to existing
**   line_pnts structures on Polys[A/B].  This should save time and memory.
**
**  Then all I do it generate the label point and write it out.
**
**  The islands will be written out either:
**    A) if they intersect something else
**    B) when I go back and write out all non-intersecting polygons.
*/
write_out_new_poly_att (Maps, Out, Polys, Points)
  struct Map_info Maps[2];
  struct Map_info *Out;
  struct poly_t *Polys[2];
  struct line_pnts *Points;	/* New Polygon points */
{
  P_AREA *Area;
  struct poly_t *P;
  double X, Y;

  /* load struct with PPoints info */
  poly_t_load_base_points (OPoly, Points);

  /*  load all island line representations into OPoly */
  poly_t_load_islands (OPoly, Polys);

  /* and finally go get a label point */


#define LABEL
#ifdef LABEL

  Cut_get_point_in_poly_t (OPoly, &X, &Y);

  /* and write it out   Note B map is the data map. */
  write_att (Out->att_fp, 'A', X, Y, 
      Maps[B_CODE].Att[Polys[B_CODE]->spoly[BASE_POLY].Area->att].cat);
    /* dig that expression.  Does that win a prize? */

/* dump_opoly (OPoly); */
/*
dump_opoly0 (OPoly);
printf ("%lf %lf	x\n", X, Y);
*/
#endif
}

/* 
** Load poly_t structure with pertinent info  for output polygon
**   This is just phase one, where we load the PPoints data into
**   the base poly of the Opoly
**
**  Will be used to
**     find label point for new polygon taking all new islands into account
**
**  BASE_POLY is just a line_pnts structure with the polygon
**
** returns 0 or -1 on out of memory 
*/
poly_t_load_base_points (P, Points)
    struct line_pnts *Points;
    struct poly_t *P;
{
    struct line_pnts *TPoints;
    register int i, j, k;

    /* 0 means don't actually allocate line_pnts */
    if (0 > alloc_poly_t (P, 1, 0))	/* allocate for each subpoly */
	return -1;

    P->n_polys = 1;				/* reset to 1 polygon */

    P->spoly[BASE_POLY].num = 0;		/* don't use line stuff */
    P->spoly[BASE_POLY].Area = NULL;
    P->spoly[BASE_POLY].Points = Points;	/* just point to PPoints */

    return 0;
}

/*
** Pull island info from Polys[0] and Polys[1], and stuff it
**  all into OPoly.  the code is a little tricky keeping track of 
**  which polygon, pointer, or index is to be used.
**
**  Just copy pointers to line_pnts structs to save memory and time.
*/

poly_t_load_islands (OPoly, Polys)
  struct poly_t *OPoly;
  struct poly_t *Polys[2];
{
  int map;
  struct poly_t *P;
  int cur_poly = 1;
  int i;

  for (map = 0 ; map < 2 ; map++)
  {
    P = Polys[map];
    alloc_poly_t (OPoly, OPoly->n_polys + P->n_polys-1, 0);

    for (i = 1 ; i < P->n_polys ; i++)
    {
      OPoly->spoly[cur_poly].Points = P->spoly[i].Points;
      
      cur_poly++;
    }
    OPoly->n_polys += P->n_polys-1;
  }
}

/*
**  like poly_t_load_islands, but for interior polys
**  which do not have Poly_t structures
*/
poly_t_interior_load_islands (OPoly, Polys)
  struct poly_t *OPoly;
  struct poly_t *Polys[2];
{
  int map;
  struct poly_t *P;
  int cur_poly = 1;
  int i;

  for (map = 0 ; map < 2 ; map++)
  {
    P = Polys[map];
    alloc_poly_t (OPoly, OPoly->n_polys + P->n_polys-1, 0);

    for (i = 1 ; i < P->n_polys ; i++)
    {
      OPoly->spoly[cur_poly].Points = P->spoly[i].Points;
      
      cur_poly++;
    }
    OPoly->n_polys += P->n_polys-1;
  }
}

dump_opoly (Poly)
    struct poly_t *Poly;
{
    int i, j;

    printf ( "\n");
    for (i = 0 ; i < Poly->n_polys ; i++)
    {
	printf ( "\n");
	for (j = 0 ; j < Poly->spoly[i].Points->n_points ; j++)
	    printf ( "%lf %lf\n", 
		    Poly->spoly[i].Points->x[j],
		    Poly->spoly[i].Points->y[j]);
    }
}

dump_opoly0 (Poly)
    struct poly_t *Poly;
{
    int i, j;

    printf ( "\n");
    {
	printf ( "\n");
	for (j = 0 ; j < Poly->spoly[0].Points->n_points ; j++)
	    printf ( "%lf %lf\n", 
		    Poly->spoly[0].Points->x[j],
		    Poly->spoly[0].Points->y[j]);
    }
}

write_out_new_line_att (Out, Points, type, att)
  struct Map_info *Out;
  struct line_pnts *Points;
  int type;
  int att;
{
  P_AREA *Line;
  struct poly_t *P;
  double X, Y;

  get_line_center (&X, &Y, Points);

  /* and write it out   Note B map is the data map. */
  write_att (Out->att_fp, (char) dig_new_to_old_type (type), X, Y, 
      att);

}
