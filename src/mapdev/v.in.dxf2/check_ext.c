#include "dxf2vect.h"

dxf_check_ext (x, y)
double	x, y;
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
}
