#include <gis.h>

char *getenv();
char *malloc();

#define STANDARD_FONT "romans"

/* the font info, and index */
static char *font = NULL;
static long *font_index = NULL;
static long nchars = 0;
static char curfontname[128];

/* one routine to build a font file name */
char *
fontfilename (name, filename)
    char *name;
    char *filename;
{
    sprintf (filename, "%s/fonts/%s", G_gisbase(), name);
    return filename;
}

/* does font exist and can it be read?
 *  1 ok,
 *  0 not found,
 * -1 not readable
 */
check_font (name)
    char *name;
{
    char filename[1024];
    fontfilename (name, filename);
    if (access(filename, 0) != 0) /* exists? */
	return 0;
    if (access(filename, 4) != 0) /* readable? */
	return -1;
    return 1;
}

select_font (name)
    char *name;
{
    char filename[1024];
    long bytes;
    int len;
    int fd;

    if (strcmp (name, "standard") == 0)
	name = STANDARD_FONT;

#ifdef DEBUG
printf ("select_font(%s)\n", name);
#endif DEBUG
/* if this is a new font, release previous font data */
    if (font)
    {
	if (strcmp(name,curfontname) == 0)
		return;
	free (font);
	font = NULL;
    }
    if (font_index)
    {
	free (font_index);
	font_index = NULL;
    }
    nchars = 0;

/* open font file in GRASS font directory */
    fontfilename (name, filename);
    fd = open (filename, 0);
    if (fd < 0)
    {
	fonterror (name, "can't open");
    }

#ifdef DEBUG
printf ( " opened\n");
#endif DEBUG

/* read entire font into memory */
    read (fd, &bytes, sizeof(bytes));
    len = bytes; /* cast long to int */
    font = (char *) malloc (len);
    if (font == NULL)
	fonterror (name, "insufficient memory for");

    lseek(fd, 0L, 0);
    if (read (fd, font, len) != len)
	fonterror (name, "unable to read");

#ifdef DEBUG
printf ( " font read\n");
#endif DEBUG

    if (read (fd, &nchars, sizeof(nchars)) != sizeof(nchars))
	fonterror (name, "unable to read");

    len = nchars * sizeof(long);
    font_index = (long *) malloc (len);
    if (font_index == NULL)
	fonterror (name, "insufficient memory for");

    if (read (fd, font_index, len) != len)
	fonterror (name, "unable to read");

#ifdef DEBUG
printf ( " index read, %ld chars\n", nchars);
#endif DEBUG

/* font is now in memory! */
    close (fd);
    strcpy (curfontname, name);
}

select_standard_font()
{
    select_font(STANDARD_FONT);
}

list_fonts ()
{
    char filename[1024];
    char command[1024];

    sprintf (command, "ls %s", fontfilename ("", filename));
    system(command);
}

static
fonterror (name, msg)
    char *name;
    char *msg;
{
    fprintf (stderr, "%s: ERROR: %s font <%s>\n", G_program_name(), msg, name);
    exit(1);
}

/* for a given character c, reads the font vector into X,Y */
get_font_char(c, n, X, Y)
    unsigned char c ;
    int *n ;
    char **X ;
    char **Y ;
{
    char *p ;

/* fake a space. first entry is a dummy (not used)
 * last entry is move after char is drawn
 * space char will have 0 vectors (even though it looks like 2)
 * Note: 10+92-'R' = 10+92-82=20 (see graph_text.c)
 */
    static char space_x[] = {0,92};
    static char space_y[] = {0,0};
    static int  space_n = 0;

    if (font == NULL)
    {
	fprintf (stderr, "%s: ERROR: no font selected\n", G_program_name());
	exit(1);
    }

    if (c == 040 || c == '\t')
    {
	*X = space_x;
	*Y = space_y;
	*n = space_n;
	return;
    }
    if (c < 041 || c > 0176)
    {
	c = '?'; /* mod shapiro */
    /*
	*n = 0 ;
	return ;
    */
    }
    p = font + font_index[c - 040] ;

/* get number of X,Y points (n) from font.
 * solve alignment problem by copying pseudo int to int variable
 */
    copy (n, p, sizeof(int));

    *X = p + sizeof(int) ;
    *Y = *X + *n ;
}

static
copy (a, b, n)
    char *a, *b;
    int n;
{
    while (n-- > 0)
	*a++ = *b++;
}
