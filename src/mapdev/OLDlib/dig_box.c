
/*
 *******************************************************************
 *  #include "dlg.h"
 *
 *  dig_init_box (n, s, e, w)
 *		double n,s,e,w ;	UTM window you the lines int
 *			stores the window for use by dig_read_next_line().
 *
 ******************************************************************
 *
 *	dig_read_line_struct_in_box (fp, line_p)
 *		FILE *fp ;
 *		struct  line_pnts  *line_p ;
 *			reads thru digit file looking for next line within window,
 *			stores info in struct instead of args.
 *		NOTE:  line_p->alloc_points better be set to 0 for the first call.
 *
 *	dig_read_line_in_box (fp, n_points, x, y)
 *		FILE *fp ;
 * 		int  *n_points ;
 *		double  **x, **y ;
 *			reads thru digit file looking for next line within window
 *
 * returns:  -1 on error
 *			  -2 EOF
 *            line type (positive value) if it found a line
 */

#include "digit.h"

static double N, S, E, W ;

dig_init_box (n, s, e, w)
	double  n, s, e, w ;
{
	if(n<=s)
		return(-1) ;
	if(e<=w)
		return(-1) ;
	
	N = n ;
	S = s ;
	E = e ;
	W = w ;

	return(0) ;
}	/*  dig_init_box()  */


dig_read_line_struct_in_box (fp, line_p)
    FILE *fp ;
	struct line_pnts  *line_p ;
{
	int itype ;

	double n, s ;
	double e, w ;

	while (1)
	{
		itype = dig_read_next_line_struct(fp, line_p) ;
		if (itype < 0)
			return(itype) ;

	/*  calculate the bounding box for the line  */
	    /* 4.0 dig_bound_box2() needs a scale to figure fudge factor
	    **   I am not concered about fudge here, so just take 
	    **   any number.  I picked 16000 cuz that is the default
	    **   in dig_bound_box2() and thus faster.
	    */
		dig_bound_box2 ( line_p, &n, &s, &e, &w, 16000) ; /*4.0*/

		if (n < S)
			continue ;
		if (s > N)
			continue ;
		if (e < W)
			continue ;
		if (w > E)
			continue ;

		return(itype) ;

	}

}  /*  dig_read_line_struct_in_box()  */


static	struct line_pnts  line_p ;
static	int first_time = 1 ;

dig_read_line_in_box (fp, n_points, x, y)
    FILE *fp ;
	int  *n_points ;
	double  **x, **y ;
{
	int itype ;

/*  first call to this function initialize alloc_points  */
	if(first_time)
	{
		line_p.alloc_points = 0 ;
		first_time = 0;
	}

	itype = dig_read_line_struct_in_box(fp, &line_p) ;
	if (itype < 0)
		return(itype) ;

/*  break the structure down for the calling program  */
	*n_points = line_p.n_points ;
	*x = line_p.x ;
	*y = line_p.y ;

	return(itype) ;

}

