
#include "graph.h"

R_color_table_float()
{
	int i ;
	_send_ident(COLOR_TABLE_FLOAT) ;
	_get_int(&i) ;
	return(i) ;
}

R_color_table_fixed()
{
	int i ;
	_send_ident(COLOR_TABLE_FIXED) ;
	_get_int(&i) ;
	return(i) ;
}

R_color_offset(n)
	int n ;
{
	int i ;
	i = n ;
	_send_ident(COLOR_OFFSET) ;
	_send_int(&i) ;
}
