/* flag.[ch] is a set of routines which will set up an array of bits
** that allow the programmer to "flag" cells in a cell file.
**
** FLAG *
** FlagCreate(nrows,ncols)
** int nrows, ncols;
**	opens the structure flag.  
**	The flag structure will be a two dimensional array of bits the
**	size of nrows by ncols.  Will initalize flags to zero (unset).
**
** FlagDestroy(flags)
** FLAG *flags;
**	closes flags and gives the memory back to the system.
**
** FlagClearAll(flags)
** FLAG *flags;
**	sets all values in flags to zero.
**
** FlagUnset(flags, row, col)
** FLAG *flags;
** int row, col;
**	sets the value of (row, col) in flags to zero.
**
** FlagSet(flags, row, col)
** FLAG *flags;
** int row, col;
**	will set the value of (row, col) in flags to one.
**
** int
** FlagGet(flags, row, col)
** FLAG *flags;
** int row, col;
**	returns the value in flags that is at (row, col).
**
** idea by Michael Shapiro
** code by Chuck Ehlschlaeger
** April 03, 1989
*/
#include <stdio.h>
#define FLAG	struct _f_l_a_g_

FLAG	
{
	int	nrows, ncols, leng;
	unsigned char	**array;
};

#define FLAG_UNSET(flags,row,col) \
	(flags)->array[(row)][(col)>>3] &= ~(1<<((col) & 7))
 
#define FLAG_SET(flags,row,col) \
	(flags)->array[(row)][(col)>>3] |= (1<<((col) & 7))

#define FLAG_GET(flags,row,col) \
	(flags)->array[(row)][(col)>>3] & (1<<((col) & 7))
