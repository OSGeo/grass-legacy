#include "flag.h"

flag_get(flags, row, col)
FLAG *flags;
int row, col;
{
	return(flags->array[row][col>>3] & (1 << (col & 7)));
}
