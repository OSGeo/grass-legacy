#include "raster.h"
#include "graph.h"


/*!
 * \brief define multiple colors
 *
 * Sets color numbers
 * <b>min</b> through <b>max</b> to the intensities represented in the arrays
 * <b>red, grn, and blue.</b>
 *
 *  \param min
 *  \param max
 *  \param red
 *  \param grn
 *  \param blue
 *  \return int
 */

int R_reset_colors( int min,int max ,
    unsigned char *red,unsigned char *grn,unsigned char *blu )
{
    int i,n ;
/* only send a chunk at a time - to avoid malloc() in the driver */
    while (min <= max)
    {
	n = max - min + 1 ;
	if (n > 512)
	    n = 512;
	_send_ident(RESET_COLORS) ;
	i = min ;
	_send_int(&i) ;
	i = min + n - 1;
	_send_int(&i) ;
	_send_char_array(n, red) ; red += n;
	_send_char_array(n, grn) ; grn += n;
	_send_char_array(n, blu) ; blu += n;
	min += n;
    }

	return 0;
}
