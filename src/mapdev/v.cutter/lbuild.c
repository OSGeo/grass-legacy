/**** lbuild.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"
#define DEBUG1

/* make Polys available to be used by interior poly code */

/*
**  Build each new polygon from table
*/

/*
**  TODO, this can be optimized by
**  keeping the current active list and line loaded until
**  it is used up
**
**  If this code looks convoluted, it is because it was
**   taken from build_polys() and I just hacked and slashed at it.
*/

build_lines (Maps, MapC, Table)		/* ==ISLE== */
    struct Map_info Maps[2];		/* Cutter/data to be cut */
    struct Map_info *MapC;		/* output map */
    struct t_data *Table;		/* intersection table */
{
  static int first_time = 1;
  static struct array_p *Active[2];	/* A B Active lists */
  static struct line_pnts *LPoints;	/* points for current line */
  static struct line_pnts *Points;	/* points for current line */
  struct sub_poly *P;
  int table_size = 0;
  int cur_table;
  int new_polys = 0;
  P_LINE *Line;

  struct t_data *T;	/* Active Pointer to build polys */
  struct t_data *TP;	/* Pointer to walk once through Table */
  struct t_data *TP2;	/* Pointer to walk once through Active list */
  struct t_data *NT;	/* Pointer to Table entry of Next intersection */
  struct t_data *NNT;	/* Assoc w/ NN, Temp Table pointer for next intersection
			** on polygon */
  int N;		/* tmp index returned from find_next_intersection() */
  int NN;		/* tmp index to next intersection on poly */

  plus_t B;		/* Index to Poly B */
  int ai;

  struct poly_vert_info *Vert;	/* complete info on current intersection/node */
  struct poly_vert_info *Vert2; /* info on next intersection */

  int IN_MODE;		/* Mode of current Vert.  IN/OUT */

  int AB_MODE;		/* Current Polygon we are on A/B */


  static int inner, outer;


  if (first_time)
  {
      first_time = 0;

      Vert  =(struct poly_vert_info *)G_malloc (sizeof (struct poly_vert_info));
      Vert2 =(struct poly_vert_info *)G_malloc (sizeof (struct poly_vert_info));

      /* don't need active list for cutter, just for the current line in data.*/
      if (NULL == (Active[B_CODE] = (struct array_p *) Array_new_struct (sizeof (struct t_data *))))
	return dig_out_of_memory ();

      if (NULL == (LPoints = Vect_new_line_struct ()))
	return dig_out_of_memory();
      if (NULL == (Points = Vect_new_line_struct ()))
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

	/*  take intersections either direction
	*/
	{
	  T = TP;		/* save TP, and use T to work w/ line */
	  LPoints->n_points = 0;

	  B = abs(T->i[B_CODE].l_index);

	  /* =LINE= */
	  load_active_line_list (Table, Active[B_CODE], B, B_CODE);

	  /* =LINE= */
	  sort_intersections_on_line (Active[B_CODE], B_CODE);

	  {
	    if (0 > V2_read_line (&(Maps[B_CODE]), Points, B))
	      return dig_out_of_memory ();
	  }

	  for (ai = 0 ; ai < Active[B_CODE]->num ; ai++)
	  {
	    TP2 = Active[B_CODE]->data[ai];
	    if (TP2->used != NOTUSED)
	      continue;
	    T = TP2;		/* save TP2, and use T to cycle line */


	    if (Debug_on)
	    {
		  fprintf (stderr, "\n\nActive list for line B %d\n", B);
		  dump_active_list (Active[B_CODE]);
	    }


	    T->used = USED;  
	    AB_MODE = B_CODE;	


	    /* =LINE= */
	    inter_to_line_vert (T, AB_MODE, Vert); 

	    LPoints->n_points = 0;
	    add_point (Vert, LPoints, (struct line_pnts *) NULL, 0);/* =LINE=*/

	    /* =LINE= */
	    N = find_next_line_intersection (Active[AB_MODE], Vert, Vert2, AB_MODE, T->in_out, T);

	    Line = &(Maps[B_CODE].Line[B]);

	    /* mark line as having been intersected */
	    BM_set (intersect_bitmap, B, 0, 1);

	    if (N < 0)
	    {
	      add_line_points_till_eol (Vert, LPoints, Points, T->in_out);
	    }
	    else
	    {
#ifdef DEBUG1
    /*DEBUG*/ debugf ("working on intersections: %d - %d\n", Vert->inter, Vert2->inter);
#endif
	      NT = Active[AB_MODE]->data[N];
	      add_line_points_till_inter (Vert, Vert2, LPoints, Points, T->in_out);
	      NT->used = USED;
	    }
	    write_cur_line (Maps, MapC, LPoints, AB_MODE, B);

	    /*
	    if (Line->att && Maps[B_CODE].Att[Line->att].cat)
	      write_out_new_line_att (MapC, LPoints, Line->type, Maps[B_CODE].Att[Line->att].cat);
	  */

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


/*  ==LINE==  */
load_active_line_list (Table, active, line, code)
    struct t_data *Table;
    struct array_p *active;
    plus_t line;
    int code;
{
  struct t_data *P;

  active->num = 0;

  for (P = Table->next ; P != NULL ; P = P->next)
  {
    /* TODO */
    if (P->i[code].l_index == line)
    {
      if (0 > Array_alloc ((struct array_t *)active, active->num+1))
	return dig_out_of_memory ();

      active->data[active->num++] = P;
    }
  }


  return 0;
}


/*
** look for next intersection on line in proper direction
**  if one exists
**    fill in Vert2 w/ data and return index to next entry in active list
**  else
**    just return 0
**
**  returns index into active list to next intersection table pointer
**   or -1 if no next intersection
*/

int
find_next_line_intersection (active, Vert, Vert2, ab_mode, dir, T) /*==LINE==*/
  struct array_p *active;
  struct poly_vert_info *Vert;
  struct poly_vert_info *Vert2;
  int ab_mode;
  int dir;
  struct t_data *T;	/* Active Pointer to build polys */
{
  struct t_data *P1, *P2;
  register int i;

  /* TODO check this */
  {
    for (i = 0 ; i < active->num ; i++)
    {
      /* isolate correct entry by:
      **   intersection
      **   polgon
      **   IN_OUT
      */
      P1 = active->data[i];
      if (P1 == T)		/* this is it */
      {
	if (dir > 0)
	{
	  if (i < active->num-1)
	  {
	    i++;
	    P2 = active->data[i];
	    goto gotit;
	  }
	  else
	  {
	    return -1;
	  }
	}
	else
	{
	  if (i > 0)
	  {
	    i--;
	    P2 = active->data[i];
	    goto gotit;
	  }
	  else
	  {
	    return -1;
	  }
	}
      }
    }
/*DEBUG*/ fprintf (stderr, "find_next: couldn't find intersection Mode: %c Inter: %d Poly: %d  \n", ab_mode == A_CODE ? 'A' : 'B', Vert->inter, Vert->poly);

    fprintf (stderr, "\n\nACTIVE LIST:\n");
    dump_active_list (active);
    exit (1);
  }



gotit:

  /*
  dump_table_entry (P1);
  dump_table_entry (P2);
  */

  inter_to_line_vert (P2, ab_mode, Vert2);	

  return i;
}
