
#include "gis.h"
#include "expression.h"
#include "func_proto.h"

/**********************************************************************
x() easting at center of column
y() northing at center of row
**********************************************************************/

int
f_x(int argc, const int *argt, void **args)
{
	DCELL *res = args[0];
	DCELL x;
	int i;

	if (argc > 0)
		return E_ARG_HI;

	if (argt[0] != DCELL_TYPE)
		return E_RES_TYPE;

	x = G_col_to_easting(0.5, &current_region);

	for (i = 0; i < columns; i++)
	{
		res[i] = x;
		x += current_region.ew_res;
	}

	return 0;
}

int
f_y(int argc, const int *argt, void **args)
{
	DCELL *res = args[0];
	DCELL y;
	int i;

	if (argc > 0)
		return E_ARG_HI;

	if (argt[0] != DCELL_TYPE)
		return E_RES_TYPE;

	y = G_row_to_northing(current_row + 0.5, &current_region);

	for (i = 0; i < columns; i++)
		res[i] = y;

	return 0;
}
