/* %W% %G% */
/*
**  Written by:  Mike Higgins
**		 Dave Gerdes   5 1988
**  US Army Construction Engineering Research Lab
*/

/*  
 *******************************************************************
 *  #include "head.h"
 *
 *  dig_init (fd)
 *      FILE *fd ;
 *
 *
 * returns:  -1 on error
 *            0 on completion
 */

#include "digit.h"
#include	"dig_head.h"


dig_init (fp)
	FILE *fp ;
{

	return(dig_read_head_binary(fp, &head) ) ;
}

dig_rewind (fp)
    FILE *fp;
{
    return (dig_read_head_binary (fp, &head));
}

dig_fini ()
{
}

dig_print_header()
{

	printf("\nSelected information from dig header\n") ;
	printf(" Organization:  %s\n", head.organization) ;
	printf(" Map Name:      %s\n", head.map_name) ;
	printf(" Source Date:   %s\n", head.source_date) ;
	printf(" Orig. Scale:   %d\n", head.orig_scale) ;

}
/*
 *******************************************************************
 *
 *  dig_read_next_line (fp, n_points, x, y)
 *      FILE *fp	      file containing dig lines
 *	    reads in the next ALIVE line from a digit file.
 *
 *  dig_read_next_line_type (fp, n_points, x, y, type)
 *      FILE *fp	      file containing dig lines
 *	    reads in the next ALIVE line from a digit file.
 *	    that matches given type "mask"
 *
 *  dig_read_next_line_struct (fp, pnts)
 *      FILE *fp	      file containing dig lines
 *	    reads in the next line from a digit file and places the info
 *	    in a line_pnts struct.
 *
 *  Returns     (int)  type  or
 *	 -2  End of file
 *	-1 Out of memory
 *
 */


dig_read_next_line (fp, n_points, x, y)
    FILE *fp ;
    int  *n_points ;
    double  **x, **y ;
{
    int type;

    /* dont read sites in */
    type = dig_read_next_line_type (fp, n_points, x, y, AREA | LINE);
    return (type);
}

dig_read_next_line_type (fp, n_points, x, y, utype)
    FILE *fp ;
    int  *n_points ;
    double  **x, **y ;
    int utype;
{
    long  ftell() ;
    long  offset ;
    int type;

    while (1)
    {
	/*  will trick Read_line() for now by passing the current offset  */
	offset = ftell(fp) ;

	type = dig_Read_line(fp, offset, x, y, n_points) ;

	switch (type) {		/* skip over deleted lines */
	    case DEAD_LINE:
	    case DEAD_AREA:
	    case DEAD_DOT:
		continue;
	}

	/* if valid read AND type is not valid type, keep looking */
	if (type > 0 && !(type & utype))
	    continue;

	return (type);
    }
}


dig_read_next_line_struct (fp, pnts)
    FILE *fp ;
    struct line_pnts *pnts ;
{
    long  offset ;
    long  ftell() ;
    int type;

    while (1)
    {
    /*  will trick Read_line() for now by passing the current offset  */
	offset = ftell(fp) ;

        type =  dig__Read_line( pnts, fp, offset) ;
	switch (type) {		/* skip over deleted lines */
	    case DEAD_LINE:
	    case DEAD_AREA:
	    case DEAD_DOT:
		continue;
	}

	return (type);
    }
}

dig__window (fp, n, s, e, w)
    FILE *fp;
    double *n, *s, *e, *w;
{
    *n = head.N;
    *s = head.S;
    *e = head.E;
    *w = head.W;

    return (0);
}
