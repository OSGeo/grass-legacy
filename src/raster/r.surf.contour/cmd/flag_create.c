#include <stdlib.h>
#include "flag.h"

FLAG *flag_create (int nrows, int ncols)
{
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
