/*
**  Written by: Dave Gerdes 5 1988
**  US Army Construction Engineering Research Lab
**
**  Modified by Dave Gerdes 1/91  for portable code
*/


/* for globs.h */
#define CONTROL

#include "digit.h"

/*  Read_line ()
**     read line info from digit file into line_points structure 
**
**  Returns     (int)  type  or
**	 -2  End of file
**	-1 Out of memory
*/

/*  Read_line handles ALL memory management internally 
**  all caller has to do is pass  double **x, **y   
**   memory is reused on each call
*/
long ftell ();

int
dig_Read_line (fp, offset, xarray, yarray, n_points)
    FILE *fp;
    long offset;
    double **xarray, **yarray;
    int *n_points;
{
    int itype;
    int alloced;
    int ret;
    static struct line_pnts Points;
    static int first_time;	/* 0 on startup */

    if (first_time == 0)
    {
	Points.alloc_points = 0;
	first_time = -1;
    }
    if (0 > (ret = dig__Read_line (&Points, fp, offset)))
	goto done;

    *n_points = Points.n_points;
    *xarray = Points.x;
    *yarray = Points.y;

done:
    return (ret);
}

/* dig_Read_line, renamed for 4.0 Vlib , cuz dig_Read_line must go away */
int
dig_v_Read_line (fp, offset, xarray, yarray, n_points)
    FILE *fp;
    long offset;
    double **xarray, **yarray;
    int *n_points;
{
    int itype;
    int alloced;
    int ret;
    static struct line_pnts Points;
    static int first_time;	/* 0 on startup */

    if (first_time == 0)
    {
	Points.alloc_points = 0;
	first_time = -1;
    }
    if (0 > (ret = dig__Read_line (&Points, fp, offset)))
	goto done;

    *n_points = Points.n_points;
    *xarray = Points.x;
    *yarray = Points.y;

done:
    return (ret);
}

/* write line info to DIGIT file */
/*  returns offset into file */
long
dig_Write_line(digit, type, xarray, yarray, n_points) 
    FILE *digit;
    char type;
    double *xarray, *yarray;
    int n_points;
{
    long offset;

    if (Lines_In_Memory)
    {
	fprintf (stderr, "CATASTROPHIC ERROR! attempt to append to memory file\n");
	return (-1);
    }
    fseek( digit, 0L, 2) ;		/*  end of file */
    offset = ftell (digit);

    dig_Rewrite_line (digit, offset, type, xarray, yarray, n_points);

    return (offset);
}

/* write line info to DIGIT file */
/*  at the given offset */
/*  obviously the number of points must NOT have changed */
/*  from when line was read in */

dig_Rewrite_line (digit, offset, type, xarray, yarray, n_points) 
    FILE *digit;
    long offset;
    char type;
    double *xarray, *yarray;
    int n_points;
{
    int itype;
    struct line_pnts points;

    points.x = xarray;
    points.y = yarray;
    points.n_points = n_points;
    return (dig__Rewrite_line (digit, offset, type, &points));
}
