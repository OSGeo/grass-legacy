#include "gis.h"
#include "flag.h"

FLAG *
flag_create(nrows,ncols)
int nrows, ncols;
{
	unsigned char *temp;
	FLAG *new_flag;
	register int i;

	new_flag  = (FLAG *) G_malloc (sizeof(FLAG));
	if (!new_flag)
	{
		G_warning ("Memory error in flag_create() at FLAG");
		return (NULL);
	}
	new_flag->nrows = nrows;
	new_flag->ncols = ncols;
	new_flag->leng = (ncols + 7) / 8;
	new_flag->array = (unsigned char **)G_malloc(nrows * sizeof(unsigned char *));
	if (!new_flag->array)
	{
		G_warning ("Memory error in flag_create() at array");
		free(new_flag);
		return(NULL);
	}
	temp = (unsigned char *)G_calloc(nrows * new_flag->leng, sizeof(unsigned char));
	if (!temp)
	{
		G_warning ("Memory error in flag_create() at temp");
		free(new_flag->array);
		free(new_flag);
		return(NULL);
	}
	for (i=0; i<nrows; i++)
	{
		new_flag->array[i] = temp;
		temp += new_flag->leng;
	}
	return(new_flag);
}
