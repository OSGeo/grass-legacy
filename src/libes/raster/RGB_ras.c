
#include "graph.h"

R_set_RGB_color(r,g,b)
	unsigned char r[256], g[256], b[256] ;
{
	_send_ident(RGB_COLORS) ;
	_send_char_array(256,r) ;
	_send_char_array(256,g) ;
	_send_char_array(256,b) ;
}

R_RGB_raster(n, nrows, red, grn, blu, withzeros)
	int n ;
	int nrows ;
	unsigned char *red, *grn, *blu ;
	int withzeros ;
{
	int z ;
	_send_ident(RGB_RASTER) ;
	z = n ;
	_send_int(&z) ;
	z = nrows ;
	_send_int(&z) ;
	_send_char_array(n,red) ;
	_send_char_array(n,grn) ;
	_send_char_array(n,blu) ;
	z = withzeros ;
	_send_int(&z) ;
}
