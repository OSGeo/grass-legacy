/*
 * $Id$
 */

#include "flag.h"

void FlagClearAll(flags)
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


FLAG *
FlagCreate(nrows,ncols)
int nrows, ncols;
{
	char *malloc(), *calloc();
	unsigned char *temp;
	FLAG *new_flag;
	register int i;

	new_flag  = (FLAG *)malloc(sizeof(FLAG));
	if (new_flag == NULL)
	{
		return ((FLAG *) NULL);
	}
	new_flag->nrows = nrows;
	new_flag->ncols = ncols;
	new_flag->leng = (ncols + 7) / 8;
	new_flag->array = (unsigned char **)malloc(nrows * sizeof(unsigned char *));
	if (new_flag->array == NULL)
	{
		free(new_flag);
		return((FLAG *) NULL);
	}
	temp = (unsigned char *)calloc(nrows * new_flag->leng, sizeof(unsigned char));
	if (temp == NULL)
	{
		free(new_flag->array);
		free(new_flag);
		return((FLAG *) NULL);
	}
	for (i=0; i<nrows; i++)
	{
		new_flag->array[i] = temp;
		temp += new_flag->leng;
	}
	return(new_flag);
}

void FlagDestroy(flags)
FLAG *flags;
{
	free(flags->array[0]);
	free(flags->array);
	free(flags);
}

int FlagGet(flags, row, col)
FLAG *flags;
int row, col;
{
	return(flags->array[row][col>>3] & (1 << (col & 7)));
}

void FlagSet(flags, row, col)
FLAG *flags;
int row, col;
{
	flags->array[row][col>>3] |= (1 << (col & 7));
}

void FlagUnset(flags, row, col)
FLAG *flags;
int row, col;
{
	flags->array[row][col>>3] &= ~(1 << (col & 7));
}
