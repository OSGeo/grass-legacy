

#include "digit.h"

/*
**  watch how routines work if A == B.  Will have to make this work
**  on a line against itself
*/

static struct line_pnts APoints;
static struct line_pnts BPoints;
static int first = 1;
double line_length ();


intersect (Map, ident_only)
    struct Map_info *Map;
    int ident_only;
{
    int A, B;
    P_LINE *Aline, *Bline;
    char *p, *getenv ();


    if (first)
    {
	APoints.alloc_points = 0;
	BPoints.alloc_points = 0;
	first = 0;
    }

    /* check all lines for being identical */
    for (A = 1 ; A <= Map->n_lines ; A++)
    {
	Aline = &(Map->Line[A]);
	if (!LINE_ALIVE (Aline))
	    continue;

	for (B = A ; B <= Map->n_lines ; B++)
	{
	    Bline = &(Map->Line[B]);
	    if (!LINE_ALIVE (Bline))
		continue;

	    /* if Nodes match up */
	    if  ( (Aline->N1 == Bline->N1 && Aline->N2 == Bline->N2) ||
		  (Aline->N1 == Bline->N2 && Aline->N2 == Bline->N1) )
	    {
		/* Are worth checking in detail */
		if (Same_lines (Map, A, B))
		    break;
	    }
	}
    }    

    if (ident_only)	/* 4.1 */
	return;



    for (A = 1 ; A <= Map->n_lines ; A++)
    {
	Aline = &(Map->Line[A]);
	if (!LINE_ALIVE (Aline))
	    continue;

	for (B = A ; B <= Map->n_lines ; B++)
	{
	    Bline = &(Map->Line[B]);
	    if (!LINE_ALIVE (Bline))
		continue;

	    if (bboxes_cross (Aline, Bline))
	    {
		/* if not, then see if they intersect */
		if (cross_lines (Map, A, B))
		    break;
	    }
	}
    }
}

cross_lines (Map, A, B)
    struct Map_info *Map;
    int A, B;
{
    P_LINE *Aline, *Bline;
    double X, Y;
    int cnt = 0;
    int a, b, endA, endB;
    int ret;
    int A_node, B_node;

    Aline = &(Map->Line[A]);
    Bline = &(Map->Line[B]);

    V1_read_line (Map, &APoints, Aline->offset);
    V1_read_line (Map, &BPoints, Bline->offset);

/*DEBUG*/  debugf2 ( "Checking lines:  %d  %d\n", A, B);
    endA = APoints.n_points-1;
    endB = BPoints.n_points-1;

    /*
    if (too_small (Aline, &APoints))
	_remove_line (Map, A);
    if (too_small (Bline, &BPoints))
	_remove_line (Map, B);
    */

    for (a = 1 ; a < APoints.n_points ; a++)
    {
	/*for (b = (b == a ? a : 1) ; b < BPoints.n_points ; b++)*/
	for (b = 1 ; b < BPoints.n_points ; b++)
	{
	    /* special cases */
	    if (A == B)
	    {
		/* if (a == b then same segment on same line */
		/*  if (a ==   b +/- 1) then adjoining segments */
		if (a == b || a == b+1 || a == b-1)
		    continue;
	    }

/*DEBUG*/  debugf2 ( "		Segments  (%d, %d)\n", a, b);
	    if 
	    (
	      /* for now, ignore co-linear */
	      (BPoints.x[b-1]==APoints.x[a] && BPoints.x[b]==APoints.x[a-1]) &&
	      (BPoints.y[b-1]==APoints.y[a] && BPoints.y[b]==APoints.y[a-1])  ||
	      (BPoints.x[b-1]==APoints.x[a-1] && BPoints.x[b]==APoints.x[a]) &&
	      (BPoints.y[b-1]==APoints.y[a-1] && BPoints.y[b]==APoints.y[a])
	    ) 
	    {
/*DEBUG*/  debugf2 ( "CHECKPOINT A  colinear\n");
		continue;
	    }
/*DEBUG*/  debugf2 ( "CHECKPOINT B\n");

	    if (ret = find_intersection (
		APoints.x[a-1], APoints.y[a-1], APoints.x[a], APoints.y[a], 
		BPoints.x[b-1], BPoints.y[b-1], BPoints.x[b], BPoints.y[b],
		&X, &Y))
	    {

		if (ret < 0)  /* Overlap, for now ignore TODO */
		{
/*DEBUG*/ debugf2 ("         OVERLAPPING segments\n");
		    continue;
		}

		/* test for nodal intersection */
		/* if is endpoint of line */
		if (((X == APoints.x[0]    && Y == APoints.y[0]) ||
		     (X == APoints.x[endA] && Y == APoints.y[endA]))  &&

		    /* AND if is also the point wE ARE looking at */
		    ((X == APoints.x[a-1] && Y == APoints.y[a-1]) ||
		     (X == APoints.x[a]   && Y == APoints.y[a])) )
		    A_node = 1;
		else
		    A_node = 0;

		/* if is endpoint of line */
		if (((X == BPoints.x[0] && Y == BPoints.y[0]) ||
		     (X == BPoints.x[endB] && Y == BPoints.y[endB]))  &&
		    /* AND if is also the point we are looking at */
		    ((X == BPoints.x[b-1] && Y == BPoints.y[b-1]) ||
		     (X == BPoints.x[b]   && Y == BPoints.y[b])) )
		     B_node = 1;
		else
		    B_node = 0;

		if (A_node || B_node) 
		{
		    if (A_node && B_node)
		    {
/*DEBUG*/  debugf2 ( "Checkpoint C  < NODES >  (%15.15lf, %15.15lf)\n", X, Y);
			continue;
		    }

		    /* TODO if breaking at node, need to use check_nodes() */
		    if (!B_node)
		    {
/*DEBUG*/  debugf2 ( "Breaking B at A Node\n");
			debug_graphic (Map, A, B, X, Y);
			break_lines_at_segs_w_point (Map, X, Y, B, b, 
			    &BPoints, 0, 0, NULL, 1);
		    }
		    else if (!A_node)
		    {
/*DEBUG*/  debugf2 ( "Breaking A at B Node\n");
			debug_graphic (Map, A, B, X, Y);
			break_lines_at_segs_w_point (Map, X, Y, A, a, 
			    &APoints, 0, 0, NULL, 1);
		    }

		    /*continue;*/
		    return (1);
		}


		/* subrout will compare A == B */
/*DEBUG*/ debugf2 ( "BREAKING  A AND (possibly) B (%15.15lf,%15.15lf)\n", X, Y);
		debug_graphic (Map, A, B, X, Y);
		break_lines_at_segs_w_point (Map, X, Y, A, a, 
		    &APoints, B, b, &BPoints, 0);
		return (1);
	    }
	    else
	    {
/*DEBUG*/  debugf2 ( "No Intersection found\n");
	    }

	}
    }
/*DEBUG*/  debugf2 ( "NO INTERSECTIONS\n");
    return (0);
}

Same_lines (Map, A, B)
    struct Map_info *Map;
    int A, B;
{
    P_LINE *Aline, *Bline;
    double X, Y;
    int cnt = 0;
    int a, b, endA, endB;
    int ret;
    int A_node, B_node;

    /* SAME line, return */

    if (A == B)
    {
	return (0);
    }

    Aline = &(Map->Line[A]);
    Bline = &(Map->Line[B]);

#ifdef FOO 		/* already checked this above */
    /* if Nodes don't match up */
    if  ( ! ( (Aline->N1 == Bline->N1 && Aline->N2 == Bline->N2) ||
              (Aline->N1 == Bline->N2 && Aline->N2 == Bline->N1) ) 
	)
    {
/*DEBUG debugf2 ("SAME:   Nodes DONT match %d %d\n", A, B );*/
	return (0);
    }
#endif

    V1_read_line (Map, &APoints, Aline->offset);
    V1_read_line (Map, &BPoints, Bline->offset);

/*DEBUG*/  debugf2 ( "Checking for SIMILAR lines:  %d  %d      ", A, B);
    endA = APoints.n_points-1;
    endB = BPoints.n_points-1;


    /* if not same # points, return */
    if (APoints.n_points != BPoints.n_points)
    {
/*DEBUG*/ debugf2 ("DIFF number of points: %d  %d\n", A, B);
	return (0);
    }


    /* well we've gotten this far, now check them point for point */
    /* see if they are identical */
    {
	int same = 1;

	/* first try b forwards */
	for (a = 0 ; a < APoints.n_points ; a++)
	{
	    if (APoints.x[a] != BPoints.x[a] || 
		APoints.y[a] != BPoints.y[a])
	    {
		same = 0;
		break;
	    }
	}
	/* if that didnt work, try b backwards */
	if (!same)
	{
	    same = 1;
	    for (a = 0 ; a < APoints.n_points ; a++)
	    {
		if (APoints.x[a] != BPoints.x[endB-a] || 
		    APoints.y[a] != BPoints.y[endB-a])
		{
		    same = 0;
		    break;
		}
	    }
	}
	if (same)	/* remove A and return */
	{
/*DEBUG*/ debugf2 ( "Lines %d and %d are SAME \n", A, B);
	    _remove_line (Map, A);
	    return (1);
	}
    }
    return (0);
}

#ifdef FOO
too_small (Line, Points)
    Pline *Line;
    struct line_pnts *Points;
{
    if (Line->type & DOT)	/* dont test SITES */
	return (0);
    if (Points.n_points > 2)	/* TODO, now only 1 segment lines */
	return (0);
    if (Line->N1 == Line->N2)	/* Same node?  too small */
	return (1);
    
}
#endif

/*
** Remove all lines of type LINE|AREA with zero length and connected
** at both ends to same node.
**  Also remove all lines that are less than threshold and have an
**   unsnapped node at at least one end. (These are considered to be
**   overshoots
*/

clean_lines (Map, thresh)
    struct Map_info *Map;
    double thresh;
{
    int A;
    P_LINE *Aline;
    char *p, *getenv ();
    double len;


    if (first)
    {
        APoints.alloc_points = 0;
        BPoints.alloc_points = 0;
        first = 0;
    }
    /* check all lines for being identical */
    for (A = 1 ; A <= Map->n_lines ; A++)
    {
        Aline = &(Map->Line[A]);
        if (!LINE_ALIVE (Aline))
            continue;
        if (!(Aline->type  & LINE|AREA))
            continue;

        if (Aline->N1 == Aline->N2 || Map->Node[Aline->N1].n_lines == 1
	       			   || Map->Node[Aline->N2].n_lines == 1 )
	{
	    V1_read_line (Map, &APoints, Aline->offset);
	    if (APoints.n_points <= 2)
	    {
		if ((len = line_length (&APoints)) < thresh)
		{
/*DEBUG*/ debugf2 ("****************  CLEANING line %d  %lf < %lf****\n", A, len, thresh);
		    _remove_line (Map, A);
		}
	    }
	}
    }
}

/* returns negative number on error   */

double
line_length (Points)
    struct line_pnts *Points;
{
    register int i, n_points;
    double *ux, *uy, dist, fabs ();

    n_points = Points->n_points;
    ux = Points->x;
    uy = Points->y;

    if (n_points <= 0)
        return -1.;

    if (n_points == 1)
        return (0.);

    dist = 0.0;
    /* get total dist */
    for (i = 1 ; i < n_points ; i++)
        dist += (fabs(ux[i]-ux[i-1]) + fabs(uy[i]-uy[i-1]));

    return (dist);
}
