
#include <math.h>

#include "expression.h"
#include "func_proto.h"

/**********************************************************************
abs(x)

   absolute value. if x is negative returns -x
**********************************************************************/

int 
f_abs(int argc, const int *argt, void **args)
{
	int i;

	if (argc < 1)
		return E_ARG_LO;
	if (argc > 1)
		return E_ARG_HI;

	if (argt[0] != argt[1])
		return E_RES_TYPE;

	switch (argt[1])
	{
	case CELL_TYPE:
	{
		CELL *res = args[0];
		CELL *arg1 = args[1];
		for (i = 0; i < columns; i++)
			if (IS_NULL_C(&arg1[i]))
				SET_NULL_C(&res[i]);
			else
				res[i] = arg1[i] < 0
					? -arg1[i]
					: arg1[i];
		return 0;
	}
	case FCELL_TYPE:
	{
		FCELL *res = args[0];
		FCELL *arg1 = args[1];
		for (i = 0; i < columns; i++)
			if (IS_NULL_F(&arg1[i]))
				SET_NULL_F(&res[i]);
			else
				res[i] = (FCELL) fabs(arg1[i]);
		return 0;
	}
	case DCELL_TYPE:
	{
		DCELL *res = args[0];
		DCELL *arg1 = args[1];
		for (i = 0; i < columns; i++)
			if (IS_NULL_D(&arg1[i]))
				SET_NULL_D(&res[i]);
			else
				res[i] = fabs(arg1[i]);
		return 0;
	}
	default:
		return E_INV_TYPE;
	}
}
