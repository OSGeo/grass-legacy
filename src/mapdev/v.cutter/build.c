/**** build.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"
#define DEBUG1

/* make Polys available to be used by interior poly code */
struct poly_t *Polys[2];	/* descriptive info about current
				** active polygons        */

/*
**  Build each new polygon from table
*/
int build_polys (		/* ==ISLE== */
    struct Map_info Maps[2],		/* Cutter/data to be cut */
    struct Map_info *MapC,		/* output map */
    struct t_data *Table		/* intersection table */
)
{
  static int first_time = 1;
  static struct array_p *Active[2];	/* A B Active lists */
  static struct line_pnts *LPoints;	/* points for current line */
  static struct line_pnts *PPoints;	/* points for new polygon */
  struct sub_poly *P;
  int table_size = 0;
  int cur_table;
  int new_polys = 0;

  struct t_data *T;	/* Active Pointer to build polys */
  struct t_data *TP;	/* Pointer to walk once through Table */
  struct t_data *TP2;	/* Pointer to walk once through Active list */
  struct t_data *NT;	/* Pointer to Table entry of Next intersection */
  struct t_data *NNT;	/* Assoc w/ NN, Temp Table pointer for next intersection
			** on polygon */
  int N;		/* tmp index returned from find_next_intersection() */
  int NN;		/* tmp index to next intersection on poly */

  plus_t A;		/* Index to Poly A */
  plus_t B;		/* Index to Poly B */
  int ai;

  struct poly_vert_info *Vert;	/* complete info on current intersection/node */
  struct poly_vert_info *Vert2; /* info on next intersection */

  int IN_MODE;		/* Mode of current Vert.  IN/OUT */
  int AB_MODE;		/* Current Polygon we are on A/B */
  int Other_mode;	/* Opposite of AB_MODE */
  int island;		/* flag identifying if this is an island, or main poly*/


  if (first_time)
  {
      first_time = 0;

      Polys[A_CODE] = new_poly_t ();
      Polys[B_CODE] = new_poly_t ();
      OPoly 	    = new_poly_t ();	/* output polygon structure */

      Vert  =(struct poly_vert_info *)G_malloc (sizeof (struct poly_vert_info));
      Vert2 =(struct poly_vert_info *)G_malloc (sizeof (struct poly_vert_info));
      if (NULL == (Active[A_CODE] = (struct array_p *) Array_new_struct (sizeof (struct t_data *))))
	return dig_out_of_memory ();
      if (NULL == (Active[B_CODE] = (struct array_p *) Array_new_struct (sizeof (struct t_data *))))
	return dig_out_of_memory ();
      if (NULL == (LPoints = Vect_new_line_struct ()))
	return dig_out_of_memory();
      if (NULL == (PPoints = Vect_new_line_struct ()))
	return dig_out_of_memory();
  }


  {
      /* count size of table */
      for (TP = Table->next ; TP != NULL ; TP = TP->next)
	table_size++;

      cur_table = 0;

      if (Table->next == NULL)	/* cover percent incase table empty */
	if (!Quiet)
	    G_percent (1, 1, 1);

      for (TP = Table->next ; TP != NULL ; TP = TP->next)
      {
	if (!Quiet)
	    G_percent (++cur_table, table_size, 5);

	if (TP->used == NOTUSED)
	{

	  if (TP->in_out == IN)
	  {
	    T = TP;		/* save TP, and use T to cycle polygon */
	    island = 1;		/* initialize flag */
	    PPoints->n_points = 0;

	    A = T->i[A_CODE].poly;
	    load_poly (&(Maps[A_CODE]), A, Polys[A_CODE]);

	    load_active_list (Table, Active[A_CODE], A, A_CODE);
	    sort_intersections_on_poly (Active[A_CODE], Polys[A_CODE], A_CODE);

	    /*
	    for (TP2 = Active[A_CODE]->next ; TP2 != NULL ; TP2 = TP2->next)
	    */
	    for (ai = 0 ; ai < Active[A_CODE]->num ; ai++)
	    {
	      TP2 = Active[A_CODE]->data[ai];
	      if (TP2->used != NOTUSED)
	        continue;
	      if (TP2->in_out != IN)
		continue;
	      T = TP2;		/* save TP, and use T to cycle polygon */
	      island = 1;		/* initialize flag */
	      PPoints->n_points = 0;

	      B = TP2->i[B_CODE].poly;
	      load_poly (&(Maps[B_CODE]), B, Polys[B_CODE]);
	      load_active_list (Table, Active[B_CODE], B, B_CODE);
	      sort_intersections_on_poly (Active[B_CODE], Polys[B_CODE], B_CODE);

	      if (Debug_on)
	      {
		    fprintf (stderr, "\n\nActive list for polygon A %d\n", A);
		    dump_active_list (Active[0]);
		    fprintf (stderr, "\n\nActive list for polygon B %d\n", B);
		    dump_active_list (Active[1]);
	      }


	      T->used = IN_USE;   /* have to come back to this point */
	      AB_MODE = B_CODE;	/* will be swapped immediately */
	      IN_MODE = OUT;	/* will be swapped immediately */

      swap:	  /* at an intersection */
	      AB_MODE = AB_MODE == A_CODE ? B_CODE : A_CODE;
	      Other_mode = AB_MODE == A_CODE ? B_CODE : A_CODE;
	      IN_MODE = IN_MODE == IN     ? OUT    : IN;

	      inter_to_poly_vert (T, Polys[AB_MODE], AB_MODE, Vert);  /*=ISLE=*/

      next_line:
	      LPoints->n_points = 0;
	      add_point (Vert, LPoints, PPoints, 1);
	      N = find_next_intersection (Active[AB_MODE], Vert, Vert2, AB_MODE, IN_MODE, Polys[AB_MODE]);
	      NT = Active[AB_MODE]->data[N];

	      /* if next inter is on same line */
	      /* TODO.  if inters are on same line and vert2 comes before
	      **   vert, then have to go through the node.
	      **   ppos provides that unless they are on same segment.  
	      **   then I need to go to the Poly_active table for ordering.
	      */
#ifdef DEBUG1
      /*DEBUG*/ debugf ("working on intersections: %d - %d\n", Vert->inter, Vert2->inter);
#endif


      /*
      dump_vert (Vert);
      dump_vert (Vert2);
      */

      /* if on main polygon, then not an island */
      if (!Vert->subpoly || !Vert2->subpoly)
	  island = 0;

  /*
  **  This ridiculously large IF is checking to see if the next stop
  **  is an intersection or whether there is a node between here and there.
  **
  **  if (both intersections on same polygon (kinduv redundant)   AND
  **       both intersections on same line  AND
  **        (
  **              2nd intersection comes after 1st  OR
  **	     (
  **		1st & 2nd intersections are on same segment and
  **		    2nd inters comes after 1st
  **	     )  OR
  **	     (
  ** 		2nd intersection is either the 1st or last point on its line
  **			(i.e. a NODE)
  **		and the line is the next line around the polygon.
  **	     )
  ** 	  )
  **
  **   In other words  we don't want to stop at a node and then have a VERY
  **    short line from the node to the intersection.
  */

	      {
		  int a;
		  struct poly_t *P2;	
		  struct sub_poly *SP;

		  a = Vert->subpoly;
		  P2 = Polys[AB_MODE];
		  SP = P2->spoly;

		  P = &(SP[a]);
	      }
	      /*
	      P = &(Polys[AB_MODE]->spoly[Vert->subpoly]);
	      */


	      if (
		    Vert2->poly == Vert->poly && 
		    Vert2->subpoly == Vert->subpoly &&		/* ==ISLE== */
		    (
		      (
			Vert2->line == Vert->line &&
			(
			  (Vert2->ppos >  Vert->ppos) || 
			  (Vert2->ppos == Vert->ppos && 
			     vert2_after_vert1 (Vert, Vert2, Polys, AB_MODE))/*ISLE*/
			)
		      ) || 
		      (
			(Vert2->line == (Vert->line + 1) % P->num) &&
			  (Vert2->pos == 0 && Vert2->midline == 0)
		      ) ||
		      (
			(Vert2->line == ring_mod ((Vert->line - 1), P->num) ) &&
			  (Vert2->pos==P->info[Vert2->line].n_verts-1
			    && Vert2->midline == 0)
		      )
		    )
		 )
	      {
#ifdef DEBUG1
      /*DEBUG*/ debugf ( "Calling  add_till_inter\n");
#endif
		add_points_till_inter (Polys[AB_MODE], Vert, Vert2, LPoints, PPoints);/*ISLE*/

		write_cur_line (Maps, MapC, LPoints, AB_MODE, abs(Polys[AB_MODE]->spoly[Vert->subpoly].Area->lines[Vert->line]));

		if (NT->used == IN_USE)	/* **** DONE **** */
		{
		    /* close polygon  */
		    Vect_append_point (PPoints, PPoints->x[0], PPoints->y[0]);

		    /* mark 1st point as used */
		    NT->used = USED;

		    goto finished;
		}

		/* if node applies to both current polygons then 
		** goto swap, otherwise, just passing through another poly
		** so set up cur inter correctly and go to next_line code.
		*/
		/*
		** TODODODDODODODO 
		**
		** The whole premise of this code has changed.
		*/
		if (T->i[AB_MODE].poly == NT->i[AB_MODE].poly && 
		    T->i[Other_mode].poly == NT->i[Other_mode].poly )
		{
		  if (T->used != IN_USE)
		    T->used = USED;
		  T = NT;		/* point to next intersection */
		  goto swap;
		}
		else
		{
  /*DEBUG*/ fprintf (stderr, "GOT TO WEIRD STUFF \n");
		  {
		    struct poly_vert_info *tmp;

		    /* set up Vert to point to new intersection for top of loop */
		    tmp = Vert;
		    Vert = Vert2;
		    Vert2 = tmp;
		  }

		  /* TODO  Check this out. */
		  /* check for an OUT/IN pair at same intersection */
		  if (NT->in_out == OUT)
		  {
		    NN = (N+1) % Active[AB_MODE]->num;
		    NNT = Active[AB_MODE]->data[NN];
		    if (NNT->inter == NT->inter)
		    {
		      NT = NNT;
		      N = NN;
		      inter_to_poly_vert (NT, Polys[AB_MODE], AB_MODE, Vert);/*ISLE*/
		    }
		  }
		  goto next_line;
		}
	      }
	      else	/* Next point is node at end of line */
	      {
#ifdef DEBUG2
		/*
		dump_vert (Vert);		
		dump_vert (Vert2);
		*/

  /*DEBUG*/ fprintf (stderr, "Calling add_points_till_eol\n");
#endif
		add_points_till_eol (Polys[AB_MODE], Vert, LPoints, PPoints);/*ISLE*/
		write_cur_line (Maps, MapC, LPoints, AB_MODE, abs(Polys[AB_MODE]->spoly[Vert->subpoly].Area->lines[Vert->line]));

		/* make next node new cur point */
		swap_pointers ((void **)&Vert, (void **)&Vert2);
		node_to_poly_vert (Polys[AB_MODE], Vert2, Vert);/*ISLE*/

		goto next_line;
	      }

  finished:  ;      /* Polygon is built.  Now write it out */
	  
	      if (!island)
	      {
		new_polys++;	/* increment counter */
		write_out_new_poly_att (Maps, MapC, Polys, PPoints);
	      }
#ifdef DEBUG2
  /*DEBUG*/   else
  /*DEBUG*/	fprintf (stderr, "ISLAND: No ATTRIBUTE\n");


  /*DEBUG  dump_line (PPoints); */
#endif

	    }
	  }
	}
      }
  }

  /*  first_time static.  so these should never go away.
  Array_destroy (Active[A_CODE]);
  Array_destroy (Active[B_CODE]);
  */


  return new_polys;
}

int swap_pointers (void **P1, void **P2)
{
  void *t;

  t = *P1;
  *P1 = *P2;
  *P2 = t;

  return 0;
}



/*
** this came from Pat McGuinness.  Instead of searching entire
**  table for next intersection on poly, make one pass through the
**  table and build a list of just those entries which refer to 
**  the two polygons of interest.
**
** First he said to include only those entries where 
**  A.poly==Apoly && B.poly==Bpoly,
**  but are also interested in all other intersections on A or B
**  in between major intersections, so had to change it to load
**  all intersections on B and all intersections on A
**
**  ISLE:  poly must be positive
*/
int load_active_list (	/* ==ISLE==*/
    struct t_data *Table,
    struct array_p *active,
    plus_t poly,
    int code)
{
  struct t_data *P;

  active->num = 0;

  for (P = Table->next ; P != NULL ; P = P->next)
  {
    if (P->i[code].poly == poly)
    {
      if (0 > Array_alloc ((struct array_t *)active, active->num+1))
	return dig_out_of_memory ();

      active->data[active->num++] = P;
    }
  }


  return 0;
}


/*
** walk around poly looking for next intersection from table.
**  fill in Vert2 w/ data and return index to next entry in active list
**
**  returns index into active list to next intersection table pointer
*/

int find_next_intersection ( /*==ISLE==*/
    struct array_p *active,
    struct poly_vert_info *Vert,
    struct poly_vert_info *Vert2,
    int ab_mode,
    int in_out,
    struct poly_t *Poly
)
{
  struct t_data *P1, *P2;
  register int i;

  /* TODO check this */
  if (0 == Vert->inter)	/* Non-intersection Node */
			      /* search for next highest line */
  {
    for (i = 0 ; i < active->num ; i++)
    {
      P1 = active->data[i];

      if (P1->i[ab_mode].poly == Vert->poly)	/* this seems redundant! */
      {
	if (P1->i[ab_mode].subpoly == Vert->subpoly)
	{
	  if (P1->i[ab_mode].line >= Vert->line)	/* take 1st >= line */
	  {
	    P2 = P1;
	    goto gotit;
	  }
	}
      }
    }


    /* none of them matched.  Next intersection must be the first one */
    /*  Next intersection on same sub-poly that is.... */
    i = 0;
    while (1)
    {
      P2 = active->data[i];
      if (P2->i[ab_mode].subpoly == Vert->subpoly)
	  goto gotit;
      i = (i+1) % active->num;
    }
  }
  else
  {


    for (i = 0 ; i < active->num ; i++)
    {
      /* isolate correct entry by:
      **   intersection
      **   polgon
      **   IN_OUT
      */
      P1 = active->data[i];


      /* TODO check this like one above */
      /* find current intersection in active table by comparing: */
      if (Vert->inter == P1->inter)		/* same intersection */
      {
	if (Vert->poly == P1->i[ab_mode].poly)	/* same poly */
	{
	  if (Vert->subpoly == P1->i[ab_mode].subpoly)	/* same subpoly */
	  {
	    if (P1->in_out == in_out)		/* same direction */
	    {
		/* next one is it. just walk around till find next one on
		**  same sub-poly 
		*/
	      while (1)
	      {
		  i = (i+1) % active->num;
		  P2 = active->data[i];
		  if (P2->i[ab_mode].subpoly == Vert->subpoly)
		      goto gotit;
	      }
	    }
	  }
	}
      }
    }
/*DEBUG*/ fprintf (stderr, "find_next: couldn't find intersection Mode: %c Inter: %d Poly: %d  IN_OUT %d\n", ab_mode == A_CODE ? 'A' : 'B', Vert->inter, Vert->poly, in_out);

    fprintf (stderr, "\n\nACTIVE LIST:\n");
    dump_active_list (active);
    exit (1);
  }



gotit:

  /*
  dump_table_entry (P1);
  dump_table_entry (P2);
  */

  inter_to_poly_vert (P2, Poly, ab_mode, Vert2);	/* =ISLE= */

  return i;
}

int dump_active_list (struct array_p *active)
{
    register int i;
    

    for (i = 0 ; i < active->num ; i++)
	dump_table_entry (active->data[i]);

    return 0;
}

#ifdef FOO

swap:
  set next in_mode to  (cur_mode + 1) %2

  set cur_point = intersection point
  set cur_segm  = intersection segment

top:
  add current point

  search all intersections on line ahead of current intersection
    next_intersection = closest intersection
  
  if (next_intersection)
  {
    add all points from cur_point+1 to next_intersection - 1 

    if (next_intersection.used == IN_USE)
      DONE
    else
      goto swap
  }
  else
  {
    add all points from cur_point + 1   to end of line - 1
    set cur_point = next_line[0];
    set cur_segm  = next_line[0];

    goto top;
  }
#endif


#ifdef DRAW
int draw_vertex (double x, double y, char *colr)
{
    FILE *fp, *popen();

    if (NULL == (fp = popen ("d.mapgraph", "w")))
    {
	fprintf (stderr, "popen failed\n");
	return;
    }
    fprintf (fp, "color %s\n", colr);
    fprintf (fp, "icon o 2 %f %f\n", x, y);
    pclose (fp);

    return 0;
}
#endif

static double dist_squared (double x1, double y1, double x2, double y2)
{
    double dx, dy;

    dx = x1 - x2;
    dy = y1 - y2;
    return (dx*dx + dy*dy);
}

/*
** Assume:  Vert->ppos  == Vert2->ppos
**
**  returns 1 if vert2 is after vert1
*/

int vert2_after_vert1 (struct poly_vert_info *Vert,
    struct poly_vert_info *Vert2, struct poly_t *Polys[2], int AB_MODE)
{
    double x, y;
    double dist1, dist2;

    x = Polys[AB_MODE]->spoly[Vert->subpoly].Points->x[Vert->ppos];
    y = Polys[AB_MODE]->spoly[Vert->subpoly].Points->y[Vert->ppos];

    dist1 = dist_squared (Vert->x, Vert->y, x, y);
    dist2 = dist_squared (Vert2->x, Vert2->y, x, y);

    if (dist2 > dist1)
	return 1;
    else
	return 0;
}

int dump_line (struct line_pnts *Points)
{
    int i;

    fprintf (stdout, "\n");
    for (i = 0 ; i < Points->n_points ; i++)
	fprintf (stdout, "%f %f\n", Points->x[i], Points->y[i]);

    return 0;
}
