
#include "expression.h"
#include "func_proto.h"

/**********************************************************************
row() row number
col() col number
**********************************************************************/

int
f_row(int argc, const int *argt, void **args)
{
	CELL *res = args[0];
	int row = current_row + 1;
	int i;

	if (argc > 0)
		return E_ARG_HI;

	if (argt[0] != CELL_TYPE)
		return E_RES_TYPE;

	for (i = 0; i < columns; i++)
		res[i] = row;

	return 0;
}

int
f_col(int argc, const int *argt, void **args)
{
	CELL *res = args[0];
	int i;

	if (argc > 0)
		return E_ARG_HI;

	if (argt[0] != CELL_TYPE)
		return E_RES_TYPE;

	for (i = 0; i < columns; i++)
		res[i] = i + 1;

	return 0;
}
