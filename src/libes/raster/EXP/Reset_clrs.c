#include "graph.h"

R_reset_colors(min, max, red, grn, blu)
    int min, max ;
    unsigned char *red, *grn, *blu ;
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
}
