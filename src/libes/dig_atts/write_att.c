/* %W% %G% */

/*  Functions in this file: write_att(), write_att_struct(), and,
*   write_att_line();
*   Each att line is exactly 50 characters long so that the line can be
*   rewritten if need be.
*/

 /*
 *******************************************************************
 *  #include "dig_atts.h"
 *
 *  write_att (fp, type, x, y, cat)
 *   FILE *fp              file to write attribute to
 *   char	type ;         A or L for Area or Line
 *   double	x, y ;         category location
 *   int	cat,           the category number
 *   
 *  Writes arguments to attribute file in proper format.
 *  The file pointer should be sitting at the exact point to write at,
 *  because the function does no seeking.
 *
 * returns: 0 on completion
 *
 */


 /*
 *******************************************************************
 *  #include "dig_atts.h"
 *
 *  write_att_struct (fp, att)
 *      FILE *fp              file to write attribute to
 *      struct att *att 	structure to write info from
 *
 * returns: 0 on completion
 *
 *  Writes info in struct to attribute file in proper format.
 *  The file pointer should be sitting at the exact point to write at,
 *  because the function does no seeking.
 */


 /*
 *******************************************************************
 *  #include "dig_atts.h"
 *
 *  write_att_line (fp, x_coors, y_coors, n_coors, cat)
 *   FILE *fp              file to write attribute to
 *   char	type ;         A or L for Area or Line
 *   double	*x_coors, *y_coors ;  line  coordinates
 *   int	n_coors,       number of coordinates in line
 *          cat,           the category number
 *   
 *
 *  Calculates Line category location then writes arguments to attribute file
 *  in proper format.
 *  The file pointer should be sitting at the exact point to write at,
 *  because the function does no seeking.
 *
 * returns: 0 on completion
 */


#include "dig_atts.h"
#include <stdio.h>

static char * float_point ();
static n_index ();
static char fbuf1[100];
static char fbuf2[100];

static
make_code (type)
    char type;
{
	switch (type) {
	    case LINE:
		type = 'L';
		break;
	    case AREA:
		type = 'A';
		break;
	    case DOT:
		type = 'P';
		break;
	    case DEAD_LINE:
		type = 'l';
		break;
	    case DEAD_AREA:
		type = 'a';
		break;
	    case DEAD_DOT:
		type = 'p';
		break;
	}
	return ((int) type);
}


write_att (fp, type, x, y, cat)
	FILE	*fp ;
	char	type ;
	double	x, y ;
	int	cat ;
{

	char	buf[80] ;

    /*  don't let them label categories with zero  */
	if (!cat)
		return(-1) ;

	type = (char) make_code (type);
	sprintf( buf, WRITE_ATT_FORMAT, type, float_point (fbuf1, FlSIZ, x), float_point (fbuf2, FlSIZ, y), cat) ;
	fprintf( fp, "%-49s\n", buf) ;

	return(0) ;

}	/*  write_att()  */


write_att_struct (fp, att)
	FILE *fp ;
	struct attribute *att ;
{

	char	buf[80] ;

	att->type = (char) make_code (att->type);
	sprintf( buf, WRITE_ATT_FORMAT, att->type, float_point (fbuf1, FlSIZ, att->x), float_point (fbuf2, FlSIZ, att->y), att->cat) ;
	fprintf( fp, "%-49s\n", buf) ;

	return(0) ;

}	/*  write_att_struct()  */



write_att_line (fp, x_coors, y_coors, n_coors, cat)
	FILE	*fp ;
	double	*x_coors, *y_coors ;
	int	n_coors,  cat ;
{
	int  i ;
	int  which_coor ;
	double  x,  y ;
	char	buf[80] ;


    /*  don't let them label categories with zero  */
	if (!cat)
		return(-1) ;

    /*  line must have at least two nodes  */
	if (n_coors < 2)
		return(-1) ;

    /*  find the relative middle of the line and print it  */
	if ( n_coors > 2)
	 {
		which_coor = n_coors / 2 ;
		sprintf( buf, WRITE_ATT_FORMAT, 'L', float_point (fbuf1, FlSIZ, x_coors[which_coor]),
			float_point (fbuf2, FlSIZ, y_coors[which_coor]), cat) ;
		fprintf( fp, "%-49s\n", buf) ;

		return(0) ;
	 }

    /*  calculate the middle of line with only two points  */

	x = (x_coors[0] + x_coors[1]) / 2 ;
	y = (y_coors[0] + y_coors[1]) / 2 ;
	sprintf( buf, WRITE_ATT_FORMAT, 'L', float_point (fbuf1, FlSIZ, x), float_point (fbuf2, FlSIZ, y), cat) ;
	fprintf( fp, "%-49s\n", buf) ;

	return(0) ;

}	/*  write_att_line()  */


static
char *
float_point (buf, size, num)
    char *buf;
    int size;
    double num;
{
    char tmp[100];
    register int whole, frac;

    sprintf (tmp, "%lf", num);
    whole = n_index (tmp, '.');
    frac = size - whole - 1;
    sprintf (buf, "%*.*lf", whole, frac, num);
    return (buf);
}

static
n_index (str, chr)
    char *str;
    int chr;
{
    register int cnt;

    for (cnt = 0 ; *str ; str++, cnt++)
	if (*str == chr)
	    return (cnt);
    return (-1);
}
