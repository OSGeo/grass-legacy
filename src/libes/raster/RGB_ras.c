#include "raster.h"
#include "graph.h"

int R_set_RGB_color(unsigned char *r,unsigned char *g,unsigned char *b)
{
	_send_ident(RGB_COLORS) ;
	_send_char_array(256,r) ;
	_send_char_array(256,g) ;
	_send_char_array(256,b) ;

	return 0;
}

int R_RGB_raster(int n, int nrows,
	unsigned char *red, unsigned char *grn, unsigned char *blu,
	unsigned char *nul)
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
	_send_char_array(n,nul ? nul : red) ;
	z = (int) nul ;
	_send_int(&z) ;

	return 0;
}
