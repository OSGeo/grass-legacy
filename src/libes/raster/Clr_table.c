#include "raster.h"
#include "graph.h"

int R_color_table_float()
{
	int i ;
	_send_ident(COLOR_TABLE_FLOAT) ;
	_get_int(&i) ;
	return(i) ;
}

int R_color_table_fixed()
{
	int i ;
	_send_ident(COLOR_TABLE_FIXED) ;
	_get_int(&i) ;
	return(i) ;
}

int R_color_offset(int n)
{
	int i ;
	i = n ;
	_send_ident(COLOR_OFFSET) ;
	_send_int(&i) ;

	return 0;
}
