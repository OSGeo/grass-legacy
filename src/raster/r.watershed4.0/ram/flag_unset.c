#include "flag.h"

flag_unset(flags, row, col)
FLAG *flags;
int row, col;
{
	flags->array[row][col>>3] &= ~(1 << (col & 7));
}
