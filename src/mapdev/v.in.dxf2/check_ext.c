#include "dxf2vect.h"

int 
dxf_check_ext (double x, double y)
{
	if (y < s)
	{
		s = y;
	}
	if (y > n)
	{
		n = y;
	}
	if (x < w)
	{
		w = x;
	}
	if (x > e)
	{
		e = x;
	}

	return 0;
}
