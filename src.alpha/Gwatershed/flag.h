/* flag.h is a set of routines which will set up an array of bits
** that allow the programmer to "flag" cells in a cell file.
**
** FLAG *
** flag_create(nrows,ncols)
** int nrows, ncols;
**	opens the structure flag.  
**	The flag structure will be a two dimensional array of bits the
**	size of nrows by ncols.  Will initalize flags to zero (unset).
**
** flag_destroy(flags)
** FLAG *flags;
**	closes flags and gives the memory back to the system.
**
** flag_clear_all(flags)
** FLAG *flags;
**	sets all values in flags to zero.
**
** flag_unset(flags, row, col)
** FLAG *flags;
** int row, col;
**	sets the value of (row, col) in flags to zero.
**
** flag_set(flags, row, col)
** FLAG *flags;
** int row, col;
**	will set the value of (row, col) in flags to one.
**
** int
** flag_get(flags, row, col)
** FLAG *flags;
** int row, col;
**	returns the value in flags that is at (row, col).
**
** idea by Michael Shapiro
** code by Chuck Ehlschlaeger
** April 03, 1989
*/

#define FLAG		struct flag
FLAG	
{
	int	nrows, ncols, leng;
	unsigned char	**array;
};


FLAG *
flag_create(nrows,ncols)
int nrows, ncols;
{
	char *malloc(), *calloc();
	unsigned char *temp;
	void free();
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

flag_destroy(flags)
FLAG *flags;
{
	void free();

	free(flags->array[0]);
	free(flags->array);
	free(flags);
}

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

#define FLAG_UNSET(flags,row,col) \
	(flags)->array[(row)][(col)>>3] &= ~(1<<((col) & 7))
 
flag_unset(flags, row, col)
FLAG *flags;
int row, col;
{
	flags->array[row][col>>3] &= ~(1 << (col & 7));
}

#define FLAG_SET(flags,row,col) \
	(flags)->array[(row)][(col)>>3] |= (1<<((col) & 7))

flag_set(flags, row, col)
FLAG *flags;
int row, col;
{
	flags->array[row][col>>3] |= (1 << (col & 7));
}

#define FLAG_GET(flags,row,col) \
	(flags)->array[(row)][(col)>>3] & (1<<((col) & 7))

flag_get(flags, row, col)
FLAG *flags;
int row, col;
{
	return(flags->array[row][col>>3] & (1 << (col & 7)));
}
