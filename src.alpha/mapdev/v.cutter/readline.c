/**** readline.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "Vect.h"

/*
** reads any specified line   This is NOT affected by constraints
*/
/*
** returns:  0 on error	  ( goes against standard returns for sake of
**				standard on 1,-1 orientation)
**           -2 EOF
**           line type (positive value) if it found a line
** 	     1 on forward orientation
** 	    -1 on reverse orientation
*/

/*
**  Note that poly should be index for polygon, even for island lines.
**   But the code has been fixed up so that it should work if an
**   island index is passed for poly.
*/


V2_read_line_poly_order (Map, Points, line, poly)
    struct Map_info *Map;
    struct line_pnts *Points;
    int line;
    plus_t poly;
{
    register int i, j;
    register double tmp;
    int ret;
    int rev;

    line = abs(line);

    if (line < 1 || line > Map->n_lines)                /* ALL DONE */
        return -2;

    ret = Vect__Read_line (Map, Points, Map->Line[line].offset);
    if (ret < 0)
	return 0;
    
    ret = 1;
    /* if line is backwards, reverse line */
    /*
    if (poly < 0 ? Map->Line[line].left != poly : Map->Line[line].left == poly)
    */

    rev = 0;
    if (poly > 0)
    {
	if (Map->Line[line].left == poly)
	    rev = 1;
	else
	    if (Map->Line[line].right == poly)
		rev = 0;
	    else	 /* we are dealing with an island at this point */
		if (Map->Line[line].right < 0)
		    rev = 0;
		else
		    rev = 1;
    }
    else
    {
	if (Map->Line[line].right < 0)
	    rev = 0;
	else
	    rev = 1;
    }

    if (rev)
    {

#ifdef DEBUG1
/*DEBUG*/ fprintf (stderr, "REVERSING: POLY %d  Line %d   Pleft: %d  Pright: %d\n", poly, line, Map->Line[line].left, Map->Line[line].right);
#endif
	ret = -1;
	for (i = 0, j = Points->n_points -1 ; i < j ; i++,j--)
	{
	    tmp = Points->x[i];
	    Points->x[i] = Points->x[j];
	    Points->x[j] = tmp;
	    tmp = Points->y[i];
	    Points->y[i] = Points->y[j];
	    Points->y[j] = tmp;
	}
    }
#ifdef DEBUG1
    else
/*DEBUG*/ fprintf (stderr, "NOT REVERSING: POLY %d  Line %d   Pleft: %d  Pright: %d\n", poly, line, Map->Line[line].left, Map->Line[line].right);
#endif

    return ret;
}
