#include "colors.h"
Get_color_index(r, g, b)
	float r, g, b ;
{
	return( 50 + (int)( b / .20)
	           + 5 * (int)( g / .20)
	           + 25 * (int)( r / .20) ) ;
}
