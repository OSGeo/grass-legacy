#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gis.h"

static long *findex, nchars;
static unsigned char *font;
static int first = 1;

int font_init(const char *filename)
{
	int file, size;
	long offset;

	if (first)
	{
		font = NULL;
		findex = NULL;
		first = 0;
	}

	file = open(filename, 0);
	if (file < 0)
		return -1;

	/* First record: an offset to the number of character in the font. */
	read(file, &offset, sizeof(offset));

	/* Read entire font into memory */
	lseek(file, 0L, 0);
	font = G_realloc(font, (size_t) offset);

	size = read(file, font, (size_t) offset);
	if (size != offset)
		G_fatal_error("can't read font! %d bytes read", size);

	/* Read font index into memory */
	lseek(file, offset, 0);
	read(file, &nchars, sizeof nchars);
	size = nchars * sizeof(*findex);

	findex = G_realloc(font, (size_t) size);
	if (read(file, findex, size) != size)
		G_fatal_error("can't read findex!");

	close(file);

	return 0;
}

int get_char_vects(
	unsigned char achar,
	int *n, unsigned char **X, unsigned char **Y)
{
	unsigned char *work_point;
	int i;

	if (!font)
	{
		*n = 0;
		return 1;
	}

	i = (int) achar - 040;   /* translate achar to char# in font index */
	if (i < 1 || i >= nchars)
	{
		*n = 0;
		return 1;
	}
	work_point = font + findex[i];

	/* n = *((int *) work_point) ; */
	/* alignment problem, resolve by copying pseudo int to int variable */
	memcpy(n, work_point, sizeof(int));

	*X = work_point + sizeof(int);
	*Y = *X + *n;

	return 0;
}

