/**** table.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"
#include "parse.h"

/* note that significant part of code is in macros.h */
#include "macros.h"

#ifdef DEBUG2
#define DEBUG_VERTS /*DEBUG*/ debugf ("A [(%9.3lf,%9.3lf) (%9.3lf,%9.3lf) (%9.3lf,%9.3lf)]\n", At.p1.x, At.p1.y, At.p2.x, At.p2.y, At.p3.x, At.p3.y); \
/*DEBUG*/ debugf ("B [(%9.3lf,%9.3lf) (%9.3lf,%9.3lf) (%9.3lf,%9.3lf)]\n", Bt.p1.x, Bt.p1.y, Bt.p2.x, Bt.p2.y, Bt.p3.x, Bt.p3.y);
#else
#define DEBUG_VERTS
#endif

/*  A map is Cutter
**  B map is Data
*/


static int next_intersection;	/* Uniq ID for each intersection 1->N */

struct table_base *
build_table (MapA, MapB)
    struct Map_info *MapA;
    struct Map_info *MapB;
{
    register int i, j;
    struct line_pnts *APoints;
    struct line_pnts *BPoints;
    P_LINE *LineA, *LineB;
    /*struct t_data T_entry;*/
    struct table_base *PTable;
    int cur_line = 0;

/* A is data file  B is cutter file */

    APoints = Vect_new_line_struct ();
    BPoints = Vect_new_line_struct ();

    PTable = table_init ();

    next_intersection = 1;

    /* init Poly intersection stuff */
    /*
    ** The alive flag just has to be non-zero
    **  So to save space, I am re-using it to keep track
    **  of which polygons do or do not have intersections
    ** All that do not need to be tested for being interior to 
    **  polgyons of other map.
    */
    {
	for (i = 1 ; i <= MapA->n_areas ; i++)
	    MapA->Area[i].alive = POLY_NOINTERSECT;

	for (i = 1 ; i <= MapB->n_areas ; i++)
	    MapB->Area[i].alive = POLY_NOINTERSECT;

	for (i = 1 ; i <= MapA->n_isles ; i++)	
	    MapA->Isle[i].alive = POLY_NOINTERSECT;

	for (i = 1 ; i <= MapB->n_isles ; i++)
	    MapB->Isle[i].alive = POLY_NOINTERSECT;
    }


    /*
    ** since cutter map will tend to have much less data, 
    ** reverse order of loops for optimizing G_percent () 
    */

    for (j = 1 ; j <= MapB->n_lines ; j++)
    {
	if (!Quiet)
	    G_percent (j, MapB->n_lines, 5);

	LineB = &(MapB->Line[j]);
	/*T_entry.i[B_CODE].l_index = j;*/

	if (LineB->type != AREA)
	    continue;		/* only interested in polygon cuts */

	for (i = 1 ; i <= MapA->n_lines ; i++)
	{
	    LineA = &(MapA->Line[i]);
	    /*T_entry.i[A_CODE].l_index = i;*/

	    /* only interested in polygons on cutter */
	    if (LineA->type != AREA)
		continue;		
				

	    if (line_bboxes_cross (LineA, LineB))
	    {
		V2_read_line (MapA, APoints, i);
		if (cur_line != j)
		    V2_read_line (MapB, BPoints, j);
		cur_line = j;

/*DEBUG*/ debugf ("***** Lines %d %d\n", i, j);
		intersect_table (MapA, MapB, APoints,BPoints,i,j,PTable);
	    }
	}
    }

    Vect_destroy_line_struct (APoints);
    Vect_destroy_line_struct (BPoints);


    return PTable;
}


intersect_table (MapA, MapB, APoints, BPoints, aline, bline, Table)
    struct Map_info *MapA;
    struct Map_info *MapB;
    struct line_pnts *APoints;
    struct line_pnts *BPoints;
    plus_t aline;
    plus_t bline;
    struct table_base *Table;
{
  int a, b;
  double ax1,ax2,ay1,ay2;
  double bx1,bx2,by1,by2;
  double x, y;
  plus_t aarea, barea;
  int ret;
  int in_out;
  struct t_data *TP;
  struct line_t A;
  struct line_t B;
  int i, j;
  int New_inter;
  P_AREA *Area;	/* re-used for several things */
  int FLAG;	/* Maintains IN/OUT status */
  int VERTEX;	/* maintains Vertex T/F status */
  int Adir, Bdir;		/* -1 rev  1  for */
  struct tribble At, Bt;
  struct line_t TempA, TempB;
  int method;	/* determines which path the code takes */

  int A_subpoly;	
  int B_subpoly;

  int Bool1, Bool2;
  int tmp1, tmp2;

  New_inter = 0;
  /*
  **  Range of points is 0 -> N-1
  **  But am dealing with segments here so they are
  **    range  1 -> N-1
  */
  for (a = 1 ; a < APoints->n_points ; a++)
  {
    for (b = 1 ; b < BPoints->n_points ; b++)
    {
/*DEBUG  debugf ( "          Segments  (%d, %d)\n", a, b);  */
      ax1 = APoints->x[a-1]; ax2 = APoints->x[a];
      ay1 = APoints->y[a-1]; ay2 = APoints->y[a];
      bx1 = BPoints->x[b-1]; bx2 = BPoints->x[b];
      by1 = BPoints->y[b-1]; by2 = BPoints->y[b];

      A.p1.x = APoints->x[a-1]; A.p2.x = APoints->x[a];
      A.p1.y = APoints->y[a-1]; A.p2.y = APoints->y[a];
      B.p1.x = BPoints->x[b-1]; B.p2.x = BPoints->x[b];
      B.p1.y = BPoints->y[b-1]; B.p2.y = BPoints->y[b];

      if (New_inter)
      {
	next_intersection ++;
	New_inter = 0;
      }


/* \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ */

/*
**For vertex intersections, since there are 4 line sections coming in to it,
**(2 in and 2 out) ignore all except the case where the TO points of each 
**segment touch. 
** e.g.:
**
**	A1-B1   NO
**	A1-B2   NO
**	A2-B1   NO
**	A2-B2   YES
*/

      if (!seg_bboxes_cross (&A, &B))
	continue;

      /* check if vertices of lines touch   (vertex-vertex intersection) */
      if (vertices_touch (&A, &B))
      {
	method = METHOD_VERTEX;
#ifdef FOO
/*DEBUG*/ fprintf (stderr, " METHOD VERTEX %d %d \n", aline, bline);
#endif
      }
      else
      {
	method = METHOD_MIDLINE;
#ifdef FOO
/*DEBUG*/ fprintf (stderr, " METHOD MIDLINE %d %d \n", aline, bline);
#endif

	ret = find_intersection (ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,&x,&y);
	if (ret > 0)
	{
	    int res2;
	  debugf ("	return = (%d,%d) %2d: %s %d\n", a,b, ret, ret ? "Intersect" : "No Intersect", next_intersection);

	  /* hack to check righttoleft/leftoright */
	  res2 = point_right_of_line (A.p1, A.p2, B.p1);
	  if (res2 == -1)
	    ret = 2;
	}

      if (ret > 0)
	  {
/* TODO  handle vertex-line  intersections */
	    if ((x == ax1 && y == ay1) || (x == ax2 && y == ay2) || 
		(x == bx1 && y == by1) || (x == bx2 && y == by2))
	    {
	      do_vertex_intersection();
    /*DEBUG*/  fprintf (stderr, "WARNING: Vertex intersection  (%d,%d)\n", a, b);
    /*DEBUG*/ fprintf (stderr, "(%lf,%lf):  (%lf,%lf)  (%lf,%lf)\n", x, y, ax1,ay1, ax2,ay2);
    /*DEBUG*/ fprintf (stderr, "                      (%lf,%lf)  (%lf,%lf)\n", bx1,by1,bx2,by2);
	      continue;
	    }
	  }

      /* in_out is set for case 1 (B crossing R -> L), but is simply
      ** inverted for case 2, and they share the same code
      */
      in_out = IN;
      switch (ret) {
	case -1:	/* co-linear */		

/*DEBUG*/  fprintf (stderr, "*********  Co-linear intersection  (%d,%d)  Should never get here!\n", a, b);
	  do_colinear_intersection();
	  continue;
	  break;
	case 0:  	/* no intersection */
/*DEBUG debugf ("No intersection\n"); */
	  continue;
	  break;
	case 2:		/* B crosses left to right */
	  in_out = -in_out;
	    /*FALLTHROUGH*/
	case 1:		/* B crosses right to left */
	  {
/*DEBUG debugf ("Lines cross:  %s\n", in_out > 0 ? "IN" : "OUT"); */
	/* just drop on down to actual code */
	    break;
	  }
	default:
	    fprintf (stderr, "ERROR: FELL THROUGH SWITCH %d\n", ret);
	    continue;
	    break;
      }
    }


	/* represent each polygon involved */
/*11111aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa*/
      for (i = 0 ; i < 2 ; i++)
      {

	if (i == 0)
	  aarea = MapA->Line[aline].right;
	else
	  aarea = MapA->Line[aline].left;


	/* TODO.  Need to support LINE lines. here? */
	if (aarea == 0)
	  continue;

	if (aarea > 0) 		/* Area */
	{
	  A_subpoly = 0;

	  if (!All)
	  {	/* if area is not labeled, dont included it */
	    Area = &(MapA->Area[aarea]);
	    if (!Area->att || !MapA->Att[Area->att].cat)
	      continue;
	  }
	}
	else			/* island */
	{
	  register int i;
	  plus_t area;

	  if (MapA->Isle[-aarea].area == 0) /* ignore if parent is universe */
	      continue;

	  area = MapA->Isle[-aarea].area;
	  Area = &(MapA->Area[area]);

	  if (!All)
	  {	/* if area is not labeled, dont included it */
	    if (!Area->att || !MapA->Att[Area->att].cat)
	      continue;
	  }

  /*DEBUG*/   A_subpoly = -1;
	  for (i = 0 ; i < Area->n_isles ; i++)
	  {
	      if (abs(aarea) == abs(Area->isles[i]))
	      {
		  A_subpoly = i+1;
		  break;
	      }
	  }
  /*DEBUG*/   if (A_subpoly < 0)
  /*DEBUG*/ 	fprintf (stderr, "Bad island %d A\n", A_subpoly);
	}
/*1111AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA*/
	
	/* this stuff was outside the if with aarea stuff, but
	** was moved inside for optimization 
	*/
	if (i == 0)  
	{
	  Adir = FORWARD;
	  if (method == METHOD_VERTEX)
	  {
	    TempA.p1.x = A.p1.x;  TempA.p1.y = A.p1.y;
	    TempA.p2.x = A.p2.x;  TempA.p2.y = A.p2.y;
	  }
	}
	else       
	{
	  Adir = REVERSE;
	  if (method == METHOD_VERTEX)
	  {
	    TempA.p1.x = A.p2.x;  TempA.p1.y = A.p2.y;
	    TempA.p2.x = A.p1.x;  TempA.p2.y = A.p1.y;
	  }
	}

/*11111bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb*/
	for (j = 0 ; j < 2 ; j++)
	{

	  if (j == 0)  
	    barea = MapB->Line[bline].right;
	  else
	    barea = MapB->Line[bline].left;

	  /* TODO */
	  if (barea == 0)
	    continue; 

	  if (barea > 0) 	
	  {
	    B_subpoly = 0;

	    if (!All)
	    {	/* if area is not labeled, dont included it */
	      Area = &(MapB->Area[barea]);
	      if (!Area->att || !MapB->Att[Area->att].cat)
		continue;
	    }
	  }
	  else			/* island */
	  {
	    register int i;
	    plus_t area;
	    P_AREA *Area;


	    area = MapB->Isle[-barea].area;

	    if (area == 0) /* ignore if parent is universe */
		continue;

	    Area = &(MapB->Area[area]);

	    if (!All)
	    {	/* if area is not labeled, dont included it */
	      if (!Area->att || !MapB->Att[Area->att].cat)
		continue;
	    }



  /*DEBUG*/   B_subpoly = -1;
	    for (i = 0 ; i < Area->n_isles ; i++)
	    {
		if (abs(barea) == abs(Area->isles[i]))
		{
		    B_subpoly = i+1;
		    break;
		}
	    }
  /*DEBUG*/   if (B_subpoly < 0)
  /*DEBUG*/ 	fprintf (stderr, "Bad island %d B\n", B_subpoly);
	  }
/*11111BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB*/

	  if (j == 0)
	    Bdir = FORWARD;
	  else
	    Bdir = REVERSE;
	  

	  if (method == METHOD_MIDLINE)
	  {
/*DEBUG*/ debugf ("Midline intersection\n");

  /*
  **	AR,BR:	+in_out	 so use xor on i, j
  **	AR,BL:	-in_out
  **	AL,BR:	-in_out
  **	AL,BL:	+in_out
  */
	    FLAG = i^j ? -in_out : in_out;	
	    VERTEX = 0;

	    {
	      New_inter = 1;

	      ADD_NEW_TABLE_ENTRY;	/***1***************************/
	    }
	  }
	  else
	  {
	    struct point_t *p;
	    int lret;	/* temp variable */
	    int PLANE;	/* Maintains which Half plane point is in */
	    int COLIN;	/* set true if any 2 segs are colinear */
	    int ANGLE;	/* line a1-a2-a3  acute or obtuse */
	    int RESULT;	/* final result 1/0 */
	    double tx, ty;
	    int res1, res2;

	    if (j == 0)  
	    {
	      TempB.p1.x = B.p1.x;  TempB.p1.y = B.p1.y;
	      TempB.p2.x = B.p2.x;  TempB.p2.y = B.p2.y;
	    }
	    else       
	    {
	      TempB.p1.x = B.p2.x;  TempB.p1.y = B.p2.y;
	      TempB.p2.x = B.p1.x;  TempB.p2.y = B.p1.y;
	    }

  /* TODO, this does not  handle unequal colinear segments correctly yet */
	    /*==========================================================*/

	    lret = vertices_touch (&TempA, &TempB);

	    if (!(lret & A2B2))
		continue; /*  Not an A2B2 intersection */
#ifdef FOO
/*DEBUG*/ fprintf (stderr, "A2B2: %d %d\n", aline, bline);
#endif

/*DEBUG*/ debugf ("Vertex intersection\n");

	    At.p1.x = TempA.p1.x; At.p1.y = TempA.p1.y;
	    At.p2.x = TempA.p2.x; At.p2.y = TempA.p2.y;
	    p = next_vert (MapA, APoints, aline, Adir > 0 ? a:a-1, aarea, Adir);
	    At.p3.x = p->x;   At.p3.y = p->y;

	    Bt.p1.x = TempB.p1.x; Bt.p1.y = TempB.p1.y;
	    Bt.p2.x = TempB.p2.x; Bt.p2.y = TempB.p2.y;
	    p = next_vert (MapB, BPoints, bline, Bdir > 0 ? b:b-1, barea, Bdir);
	    Bt.p3.x = p->x;   Bt.p3.y = p->y;

    DEBUG_VERTS

	    /*
	    draw_tribble (&At, "red");
	    draw_tribble (&Bt, "blue");
	    */

	    {
	      int tmp_col;	/* tmp var for optimization */
	      FLAG = OUT;
	      PLANE = RIGHT;
	      COLIN = 0;

    /* what I want to do is, if there is a co-linear pair, to 
    **  flip the tribbles around until co-linear is on segs A1 and B1
    **  so that I only have to code for one case.
    */


    /* Now, how to do that... */


#ifdef KARNAUGH_MAP

    A A    A1B1  |      0       |       0       |       1       |       1
    2 2          |              |               |               |
    B B    A1B2  |      0       |       1       |       0       |       1
    1 2          |              |               |               |
  ===============+==============+===============+===============+=============+
                 |              |               |               |
                 |      0       |   FLIP  B     |       0       |  Do not Care
    0  0         |              |               |               |
                 |              |               |               |
  ===============+==============+===============+===============+=============+
                 |              |               |               |
                 |  FLIP        |  Do not Care  |       0       |  Do not Care
    0  1         |    A and B   |               |               |
                 |              |               |               |
  ===============+==============+===============+===============+=============+
                 |              |               |               |
                 |  FLIP  A     |   FLIP  A     |               |  Do not Care
    1  0         |              |    (or B)     |  Do not Care  |
                 |              |               |               |
  ===============+==============+===============+===============+=============+
                 |              |               |               |
                 |              |               |               |
    1  1         |  Do not Care |  Do not Care  |  Do not Care  |  Do not Care
                 |              |               |               |
                 |              |               |               |
  ===============+==============+===============+===============+=============+

#endif



/* There is a bug in the Sun compiler that it can not handle the
**  statements below.  Either that or I have a bug that I can not figger
*/

/*
	      Bool1 = (seg_colinear (&At.p2, &At.p3, &Bt.p1, &Bt.p2) << 1) | 
		      seg_colinear (&At.p2, &At.p3, &Bt.p2, &Bt.p3);


	      Bool2 = (seg_colinear (&At.p1, &At.p2, &Bt.p1, &Bt.p2) << 1) | 
		      seg_colinear (&At.p1, &At.p2, &Bt.p2, &Bt.p3);
*/

      Bool1 = ((tmp1 = seg_colinear (&At.p2, &At.p3, &Bt.p1, &Bt.p2)) << 1) | 
	      (tmp2 = seg_colinear (&At.p2, &At.p3, &Bt.p2, &Bt.p3));


      Bool2 = ((tmp1 = seg_colinear (&At.p1, &At.p2, &Bt.p1, &Bt.p2)) << 1) | 
	      (tmp2 = seg_colinear (&At.p1, &At.p2, &Bt.p2, &Bt.p3));


/*DEBUG*/ debugf ("Bool1 == %d  Bool2 == %d\n", Bool1, Bool2);

	      if (Bool1 && !(Bool2 & 2))
	      {
/*DEBUG*/ debugf ("Reversing A Verts\n");
		/* reverse A verts */
		tx = At.p1.x;         ty = At.p1.y;
		At.p1.x = At.p3.x;    At.p1.y = At.p3.y;
		At.p3.x = tx;         At.p3.y = ty;
		PLANE = -PLANE;
		COLIN = 1;
    DEBUG_VERTS
	      }


	    
	      if ( !((Bool1|Bool2) & 2) && ((Bool1 | Bool2) & 1) )
	      {
		/* reverse B verts */
/*DEBUG*/ debugf ("Reversing B Verts\n");
		tx = Bt.p1.x;         ty = Bt.p1.y;
		Bt.p1.x = Bt.p3.x;    Bt.p1.y = Bt.p3.y;
		Bt.p3.x = tx;         Bt.p3.y = ty;
		FLAG = -FLAG;
		COLIN = 1;
    DEBUG_VERTS
	      }

	      /* Check the only other segment pair for colinear */
	      if (!COLIN)
		  COLIN = seg_colinear (&Bt.p1, &Bt.p2, &At.p1, &At.p2);

	      /*
	      **  All other code except this next block is written for 
	      **  the very special case that at least one leg of each 
	      **  tribble is colinear w/ one  of the other
	      **
	      **  If not, then we simply have to test whether the
	      **  tribbles cross or not.
	      */

	      VERTEX = 1;
	      x = At.p2.x;
	      y = At.p2.y;

	      if (!COLIN)	
	      {
	       /*
	       **  if the tribbles cross, then there is an intersection,
	       **  if they just skim each other at a point and diverge,
	       **  then there is no intersection
	       **
	       **  Note that there is NO tribble line reversal at this point
	       */
	       if (tribbles_intersect (At, Bt))
	       {
	       /*  need to check right to left crossing */
		if( ((point_right_of_line (At.p1, At.p2, Bt.p1) == 1) &&
		     (point_right_of_line (At.p2, At.p3, Bt.p1) == 1))   || 
		    ((!point_right_of_line (At.p1, At.p2, Bt.p3) == 1) &&
		     (!point_right_of_line (At.p2, At.p3, Bt.p3) == 1)) )
		    FLAG = IN;
		else
		    FLAG = OUT;

/*DEBUG*/ debugf (" IN TRIBBLE CODE\n");
		New_inter = 1;	/* TEST */

		ADD_NEW_TABLE_ENTRY;	/***2***************************/

	       }

		/* either way, we are done here */
		continue;
	      }

	      /* if (A0-A1-A2 == B0-B1-B2)     co-co-linear */

	      /* actually the first test should already be true
	      ** if we get this far.
	      */
	      if (seg_colinear (&At.p1, &At.p2, &Bt.p1, &Bt.p2) &&
		  seg_colinear (&At.p2, &At.p3, &Bt.p2, &Bt.p3))
	      {
/*DEBUG*/ debugf ("CO-CO-LINEAR  No intersection\n");
		/* NO Crossing */
		continue;
	      }
	      else
	      {

  /*
  **		Plane:
  **		RIGHT		LEFT
  ** Angle:    +--------------+-------------+
  **    ACUTE  | test right   | test right  |
  **	       |              |             |
  **   	       | use result   | use !result |
  **	       +--------------+-------------+
  **	OBTUSE | test left    | test left   |
  **	       |              |             |
  **	       | use !result  | use result  |
  **	       +--------------+-------------+
  **
  **    A | P |  !result  == XOR
  **    --+---+---------
  **    0 | 0 |   0
  **    0 | 1 |   1
  **    1 | 0 |   1
  **    1 | 1 |   0
  **
  */

	       /* ACUTE = 1 : OBTUSE = 0 */
	       /* make PLANE:  RIGHT: 1   LEFT: 0 */

	       ANGLE = (point_right_of_line (At.p1, At.p2, At.p3) == 1);
	       PLANE = (PLANE == 1);

	       res1 = point_right_of_line (At.p1, At.p2, Bt.p3);
	       res2 = point_right_of_line (At.p2, At.p3, Bt.p3);
	       if (ANGLE == 1)	/* ANGLE == ACUTE */
	       {	/*  test right */
		   RESULT = (res1 == 1) && (res2 == 1);
	       }
	       else
	       {	/*  test left */
		   RESULT = (res1 == -1) && (res2 == -1);
	       }

	       /* note this is bitwise, but effect is logical xor */
	       if (PLANE^ANGLE)	/* PLANE XOR ANGLE */
	       {
		   RESULT = !RESULT;
	       }

	       if (RESULT)
	       {  /* B(2) is in PLANE quarter-plane of A0-A1-A2 */

		New_inter = 1;

/*DEBUG*/ debugf ("CO-LINEAR  intersection\n");
    DEBUG_VERTS
	    /*
	    draw_tribble (&At, "red");
	    draw_tribble (&Bt, "blue");
	    */


		x = At.p2.x;
		y = At.p2.y;


		ADD_NEW_TABLE_ENTRY;	/***3***************************/

	       }
	       else
		   continue;	/* no crossing */


	      }
	    }
	  }
	}
      }
    }
  }

  /* catch the last one for this line_pair */
  if (New_inter)
  {
    next_intersection++;
    New_inter = 0;
  }
  return 0;
}

do_vertex_intersection()
{
}

do_colinear_intersection()
{
}

destroy_table (Table)
    struct table_base *Table;
{
    table_cleanup (Table);
}

draw_tribble (T, colr)
    struct tribble *T;
    char *colr;
{
    FILE *fp, *popen();

/*DEBUG*/ fprintf (stderr, "IN DRAW_TRIBBLE\n");

    if (NULL == (fp = popen ("d.mapgraph", "w")))
    {
	fprintf (stderr, "popen failed\n");
	return;
    }
    fprintf (fp, "color %s\n", colr);
    fprintf (fp, "move %lf %lf\n", T->p1.x, T->p1.y);
    fprintf (fp, "draw %lf %lf\n", T->p2.x, T->p2.y);
    fprintf (fp, "draw %lf %lf\n", T->p3.x, T->p3.y);
    pclose (fp);
}

#ifdef FOO
FLAG = OUT;
PLANE = RIGHT;

if lines cross
if lines cross in mid-segment
  just determine if B crosses from L->R (OUT) or R->L (IN)

else /* lines meet at one or two vertex  */
  if either vertex is a node  
      do extra stuff
  if co-lin on segment A2-A3
      reverse A verts
      PLANE = -PLANE;
  if co-lin on segment B2-B3
      reverse B verts
      FLAG = -FLAG;

  if (A0-A1-A2 == B0-B1-B2)	/* co-co-linear */
      NO Crossing
  else
      if B(2) is in PLANE Half-plane of A0-A1-A2
	  Crossing-flag = FLAG
      else
	  NO Crossing
#endif
