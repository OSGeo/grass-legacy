#include "flag.h"

flag_clear_all(flags)
FLAG *flags;
{
	register int r, c;

	for(r=0; r<flags->nrows; r++)
	{
		for (c=0; c<flags->leng; c++)
		{
			flags->array[r][c] = 0;
		}
	}
}
