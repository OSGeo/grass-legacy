/**** vertex.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"

/*
** struct poly_vert_info refers to line coming OUT from intersection or node
**
** table entries on the other had refer to the line coming INTO the intersection
**  
**  Thus  the use of bump_vert_to_next_line (P, Vert)
*/

/* 
** Given a table entry, fill in a poly_vert_info struct
**  to use in building new polygon
**
*/
inter_to_poly_vert (T, Poly, poly_code, Vert) /* ==ISLE== */
  struct t_data *T;
  int poly_code;
  struct poly_t *Poly;
  struct poly_vert_info *Vert;
{
  register int i;
  int pos;
  plus_t line;
  struct sub_poly *P;

  P = &(Poly->spoly[T->i[poly_code].subpoly]);

  Vert->poly_code = poly_code;
  Vert->poly = T->i[poly_code].poly;
  Vert->line = T->i[poly_code].line;
  Vert->subpoly = T->i[poly_code].subpoly;


  if (T->i[poly_code].vertex)	/* Vertex intersection? */
      Vert->pos  = T->i[poly_code].segment;  /* remember that intersection is */
				     /*	 at END of segment	      */
  else
      Vert->pos  = T->i[poly_code].segment-1;

  Vert->x = T->x;
  Vert->y = T->y;
  Vert->inter = T->inter;

  /* find pos by walking around poly */
  pos = 0;
  line = T->i[poly_code].l_index;
  for (i = 0 ; i < P->Area->n_lines ; i++)
  {
    if (P->Area->lines[i] == line)	/* should I use abs()? */
    {
      pos += Vert->pos;
      break;
    }
/*XDEBUG*/    else
/*XDEBUG*/    if (abs(P->Area->lines[i]) == abs(line))
/*XDEBUG*/        fprintf (stderr, "Sign error: l_index - Area->lines[%d]\n", i);
    
    pos += (P->info[i].n_verts - 1);
  }
  Vert->ppos = pos;

  if (T->i[poly_code].vertex)
  {
    Vert->midline = 0;		/* pos refers to vert #  0 -> N-1 */

    /* if at end of line, need to reset point to next line segment */
    if (Vert->pos == P->info[Vert->line].n_verts-1)
    {
	bump_vert_to_next_line (P, Vert);
    }
  }
  else
  {
    Vert->midline = 1;		/* pos refers to seg #  1 -> N */
  }

}



/*
**  This routine creates a new struct poly_vert_info (Vert) given the 
**    previous intersection (Oldvert)
**
**  Note that inter == 0 for non-intersection Nodes.
**
*/
node_to_poly_vert (Poly, Oldvert, Vert)	/* ==ISLE== */
  struct poly_t *Poly;
  struct poly_vert_info *Oldvert;
  struct poly_vert_info *Vert;
{
  register int i;
  struct sub_poly *P;

  P = &(Poly->spoly[Oldvert->subpoly]);

  Vert->poly_code = Oldvert->poly_code;
  Vert->poly = Oldvert->poly;
  Vert->subpoly = Oldvert->subpoly;
  Vert->line = (Oldvert->line + 1) % P->num;
  Vert->pos  = 0;

  i = sub_poly_line_start_pos (P, Vert->line);
  Vert->ppos  = i;
  Vert->x = P->Points->x[i];
  Vert->y = P->Points->y[i];
  Vert->inter = 0;
  Vert->midline = 0;
}

add_point (P, LPoints, PPoints, area_too)	/* ==ISLE== */
  struct poly_vert_info *P;
  struct line_pnts *LPoints;
  struct line_pnts *PPoints;
{
  Vect_append_point (LPoints, P->x, P->y); 
  if (area_too)
    Vect_append_point (PPoints, P->x, P->y); 
}

add_points_till_eol (Poly, Vert, LPoints, PPoints)	/* ==ISLE== */
  struct poly_t *Poly;
  struct poly_vert_info *Vert;
  struct line_pnts *LPoints;
  struct line_pnts *PPoints;
{
  register int i, j;
  struct sub_poly *P;

  P = &(Poly->spoly[Vert->subpoly]);

/*DEBUG*/ 
/*DEBUG*/ 
/*DEBUG*/ 
/*DEBUG*/ 
/*
{
    int i;
    for (i = 0 ; i < P->Points->n_points  ;  i++)
	fprintf ( stderr, "(%lf,%lf) 1\n", P->Points->x[i], P->Points->y[i]);
    exit (0);
    
}
*/

#ifdef DEBUG1
/*DEBUG*/ debugf ("Add_to_EOL:\n");
/*DEBUG*/ dump_vert (Vert);
#endif
  for (i = 1 ; i < P->info[Vert->line].n_verts - Vert->pos  - 1; i++)
  {
    j = i + Vert->ppos;
    Vect_append_point (LPoints, P->Points->x[j], P->Points->y[j]);
    Vect_append_point (PPoints, P->Points->x[j], P->Points->y[j]);
  }

  /* get end of line, but don't include it in poly */
  j = i + Vert->ppos;
  j = j % P->Points->n_points;
  Vect_append_point (LPoints, P->Points->x[j], P->Points->y[j]);
}

/*
**  1st point has been added by calling routine.
**  last point is added to line, but not to polygon
*/
add_points_till_inter (Poly, Vert, Vert2, LPoints, PPoints)	/* ==ISLE== */
  struct poly_t *Poly;
  struct poly_vert_info *Vert;
  struct poly_vert_info *Vert2;
  struct line_pnts *LPoints;
  struct line_pnts *PPoints;
{
  register int i;
  int stop;
  struct sub_poly *P;

#ifdef DEBUG1
/*DEBUG*/ fprintf (stderr, "Add_to_inter:\n");
/*DEBUG*/ dump_vert (Vert);
/*DEBUG*/ dump_vert (Vert2);
#endif

/* NOTE pos is [0->N-1]  and Points is [0->N-1] */

  P = &(Poly->spoly[Vert->subpoly]);

  /* if next intersection is at vertex at end of line, then
  **  lets sneak in a call to add_till_eol to just load to end of line
  */
  if (Vert2->midline == 0 && Vert2->pos == 0 && (Vert->line+1)%P->num == Vert2->line)
  {
#ifdef DEBUG1
/*DEBUG*/ fprintf (stderr, "Re-calling Add_to_eol:\n");
#endif
    return add_points_till_eol (Poly, Vert, LPoints, PPoints);
  }

  /* TODO  check this for < 0 */
  if (Vert2->midline)
    stop = Vert2->ppos;
  else
    stop = Vert2->ppos - 1;

  /* for (i = Vert->ppos + 1 ; i < Vert2->ppos ; i++) */
  for (i = Vert->ppos + 1 ; i <= stop ; i++)
  {
    Vect_append_point (LPoints, P->Points->x[i], P->Points->y[i]);
    Vect_append_point (PPoints, P->Points->x[i], P->Points->y[i]);
  }
  /* get end of line, but don't include it in poly */
  Vect_append_point (LPoints, Vert2->x, Vert2->y);
}

sub_poly_line_start_pos (P, line)	/*==ISLE==*/
  struct sub_poly *P;
  int line;
{
  register int i;
  int pos;

  pos = 0;
  for (i = 0 ; i < P->Area->n_lines ; i++)
  {
    if (i == line)
      break;
    
    pos += (P->info[i].n_verts - 1);
  }

  return pos;
}

#ifdef FOO  /* not isle ready */
dump_poly (P)
    struct poly_t *P;
{
    int i;

    fprintf (stderr, "\nPOLY_T Structure:\n");
    fprintf (stderr, "  num_lines: %d\n", P->num);
    for (i = 0 ; i < P->num ; i++)
	fprintf (stderr, "     n_verts %d   dir %d\n", P->info[i].n_verts, P->info[i].dir);
    fprintf (stderr, "  num_points: %d\n", P->Points->n_points);
    for (i = 0 ; i < P->Points->n_points ; i++)
	fprintf (stderr, "      %f, %f\n", P->Points->x[i], P->Points->y[i]);
}
#endif


dump_vert (Vert)	/* ==ISLE== */
  struct poly_vert_info *Vert;
{
fprintf (stderr,  "A/B %d Poly: %2d SPoly: %2d Line: %2d  Pos: %2d  PPos: %2d  Mid %d Inter %d\n", 
  Vert->poly_code, 
  Vert->poly, 
  Vert->subpoly, 
  Vert->line, 
  Vert->pos, 
  Vert->ppos, 
  Vert->midline, 
  Vert->inter);
}

/* 
** Given a table entry, fill in a poly_vert_info struct
**  to use in building new polygon
**
*/
inter_to_line_vert (T, poly_code, Vert) 	/*==LINE==*/
  struct t_data *T;
  int poly_code;
  struct poly_vert_info *Vert;
{
  register int i;
  int pos;
  plus_t line;


  Vert->poly_code = poly_code;
  Vert->poly = 0;
  Vert->subpoly = T->i[poly_code].subpoly;
  Vert->line = T->i[poly_code].l_index;		/* note use line index */


  if (T->i[poly_code].vertex)	/* Vertex intersection? */
      Vert->pos  = T->i[poly_code].segment;  /* remember that intersection is */
				     /*	 at END of segment	      */
  else
      Vert->pos  = T->i[poly_code].segment-1;

  Vert->x = T->x;
  Vert->y = T->y;
  Vert->inter = T->inter;

  Vert->ppos = 0;

  if (T->i[poly_code].vertex)
  {
    Vert->midline = 0;		/* pos refers to vert #  0 -> N-1 */
  }
  else
  {
    Vert->midline = 1;		/* pos refers to seg #  1 -> N */
  }
}





/* ==LINE== */
/*  assume direction is forward */
add_line_points_till_eol (Vert, LPoints, Points, dir)
  struct poly_vert_info *Vert;
  struct line_pnts *LPoints;
  struct line_pnts *Points;
{
  register int i;

#ifdef DEBUG1
/*DEBUG*/ debugf ("Add_to_EOL:\n");
/*DEBUG*/ dump_vert (Vert);
#endif

  if (dir == FORWARD)
  {
    for (i = Vert->pos+1 ; i < Points->n_points ; i++)
    {
      Vect_append_point (LPoints, Points->x[i], Points->y[i]);
    }
  }
  else
  {
    for (i = Vert->pos ; i >= 0 ; i--)
    {
      Vect_append_point (LPoints, Points->x[i], Points->y[i]);
    }
  }
}



/*
**  1st point has been added by calling routine.
*/

/*  assume direction is forward */
add_line_points_till_inter (Vert, Vert2, LPoints, Points, dir)
  struct poly_vert_info *Vert;
  struct poly_vert_info *Vert2;
  struct line_pnts *LPoints;
  struct line_pnts *Points;
{
  register int i;
  int stop, start;

#ifdef DEBUG1
/*DEBUG*/ fprintf (stderr, "Add_to_inter:\n");
/*DEBUG*/ dump_vert (Vert);
/*DEBUG*/ dump_vert (Vert2);
#endif

/* NOTE pos is [0->N-1]  and Points is [0->N-1] */

  if (dir == FORWARD)
  {
    /* TODO  check this for < 0 */
    if (Vert2->midline)
    {
      stop = Vert2->pos;
    }
    else
    {
      stop = Vert2->pos - 1;
    }

    /* for (i = Vert->ppos + 1 ; i < Vert2->ppos ; i++) */
    for (i = Vert->pos + 1 ; i <= stop ; i++)
    {
      Vect_append_point (LPoints, Points->x[i], Points->y[i]);
    }
    /* get end of line, but don't include it in poly */
    Vect_append_point (LPoints, Vert2->x, Vert2->y);
  }
  else
  {
    /* TODO  check this for < 0 */
    stop = Vert2->pos + 1;

    if (Vert->midline)
	start = Vert->pos;
    else
	start = Vert->pos - 1;

    /* for (i = Vert->ppos + 1 ; i < Vert2->ppos ; i++) */
    for (i = start ; i >= stop ; i--)
    {
      Vect_append_point (LPoints, Points->x[i], Points->y[i]);
    }
    /* get end of line, but don't include it in poly */
    Vect_append_point (LPoints, Vert2->x, Vert2->y);
  }
}
