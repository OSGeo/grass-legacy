#define NULL	0

static long *index = NULL ;
static unsigned char *font = NULL ;
static char *basedir = "/usr/local/grass/binfont" ;

int init_font (char *filename)
{
	int file ;
	long offset ;
	long nchars ;
	int size ;
	int buf[256] ;

	sprintf(buf,"%s/%s", basedir, filename) ;
	file = open (buf, 0);
	if (file < 0)
	{
		perror ("binfont");
		return(-1) ;
	}
	read (file, &offset, sizeof(offset));
	fprintf (stdout,"index is at %ld\n", offset);

/* Read entire font into memory */
	lseek (file, 0L, 0);
	if (font == NULL)
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
	fprintf (stdout,"font contains %d characters\n", nchars);
	size = nchars * sizeof (*index);
	if (index == NULL)
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
}

int get_char_vects (int achar, int *n, unsigned char **X, unsigned char **Y)
{
	unsigned char *work_point ;

	work_point = font + index[achar - ' '] ;
	copy((char *)n, work_point, sizeof(int) ) ;
	*X = work_point + sizeof(int) ;
	*Y = *X + *n ;
}

int copy (char *new, char *old, int n)
{
	while(n--)
		*new++ = *old++ ;
}
