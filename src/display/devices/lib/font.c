#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "driverlib.h"

static long *index ;
static unsigned char *font ;
static long nchars ;
static int first = 1 ;
static int X_copy(int *,char *,int);

int init_font(char *filename)
{
	int file ;
	long offset ;
	int size ;

	if(first)
	{
		font = NULL ;
		index = NULL ;
		first = 0 ;
	}

	file = open (filename, 0);
	if (file < 0)
		return(-1) ;

	read (file, &offset, sizeof(offset));
	/*
	fprintf (stdout,"index is at %ld\n", offset);
	*/

/* Read entire font into memory */
	lseek (file, 0L, 0);
	if (font != NULL)
		free(font) ;
	font = (unsigned char *) malloc ((int)offset) ;
	if (font == NULL)
	{
		perror("insufficient memory for font") ;
		exit(-1) ;
	}
	if (read (file, font, (int)offset) != (int)offset)
	{
		fprintf (stdout,"can't read font!\n");
		exit(-1);
	}

/* Read font index into memory */
	lseek (file, offset, 0);
	read (file, &nchars, sizeof nchars);
/*
	fprintf (stderr, "font contains %d characters\n", nchars);
*/
	size = nchars * sizeof (*index);
	if (index != NULL)
		free(index) ;
	index = (long *)malloc (size);
	if (index == NULL)
	{
		perror ("insufficient memory for index") ;
		exit(-1);
	}
	if (read (file, index, size) != size)
	{
		fprintf (stdout,"can't read index!\n");
		exit(0);
	}

	close(file) ;
	return 0;
}

int get_char_vects(
	unsigned char achar,
	int *n,
	unsigned char **X,
	unsigned char **Y)
{
	unsigned char *work_point ;
	int i;
	long tn;

	if (font == NULL)
	{
		*n = 0 ;
		return 0;
	}
	i = (int) achar - 040;   /* translate achar to char# in font index */
	if (i < 1 || i >= nchars)
	{
		*n = 0 ;
		return 0;
	}
	work_point = font + index[i];

	/*
	*n = *((int *) work_point) ;
	*/
    /* alignment problem, resolve by copying pseudo int to int variable
	X_copy (n, work_point, sizeof(int)); */
	tn = (long)(work_point);
	*n = (int)tn;

	*X = work_point + sizeof(int) ;
	*Y = *X + *n ;

	return 0;
}

static int X_copy (int *a,char *b, int n)
{
     while (n-- > 0)
	(char)*a++ = *b++;

     return 0;
}
