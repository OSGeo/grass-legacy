
#include "graph.h"

R_reset_colors(min, max, red, grn, blu)
	int min, max ;
	unsigned char *red, *grn, *blu ;
{
	int n ;
	if (max < min)
		return ;  /* silently */
	_send_ident(RESET_COLORS) ;
	n = min ;
	_send_int(&n) ;
	n = max ;
	_send_int(&n) ;
	n = max - min + 1 ;
	_send_char_array(n, red) ;
	_send_char_array(n, grn) ;
	_send_char_array(n, blu) ;
}
