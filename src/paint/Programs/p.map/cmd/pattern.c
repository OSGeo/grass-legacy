#include "gis.h"
#include "pattern.h"
#include "misc.h"

#define PLIST struct _plist_

PLIST
{
    char *name;
    long offset;
    int nlines;
    int nrows;
    int ncols;
    int file;
    PLIST *next;
};

static PLIST *plist = NULL;
static FILE *patternfd = NULL;
static char *patternfile = NULL;
static long cur_offset = 0;
static char *cur_name;
static int nl;
static int nr;
static int nc;
static int cur_file = -1;
static int cur_mode = -1;

add_pattern_to_list (name, offset, nlines, nrows, ncols, file)
    char *name;
    long offset;
{
    PLIST *p;

    if (name == NULL)
	name = cur_name;
    else
	name = G_store (name);

    p = (PLIST *) G_malloc (sizeof(PLIST));

    p->name   = name;
    p->offset = offset;
    p->nlines = nlines;
    p->nrows  = nrows;
    p->ncols  = ncols;
    p->file   = file;

    p->next = plist;
    plist = p;
}

find_pattern (name, offset, nlines, nrows, ncols, file)
    char *name;
    long *offset;
    int *nlines, *nrows, *ncols;
    int *file;
{
    PLIST *p;

    read_predefined_patterns();

    for (p = plist; p; p = p->next)
	if (strcmp (name, p->name) == 0)
	{
	    *offset = p->offset;
	    *nlines = p->nlines;
	    *nrows  = p->nrows;
	    *ncols  = p->ncols;
	    *file   = p->file;
	    return 1;
	}
    return 0;
}

read_pattern (p, nlines)
    PATTERN *p;
{
    char *pp;
    char buf[1024];
    int line, row, col;
    int nrows, ncols;
    int xcenter, ycenter;
    int count;
    int i, color;

#ifdef DEBUG
printf ("read_pattern\n");
#endif

    nrows = p->nrows;
    ncols = p->ncols;
    if (nrows == 0) nrows = 1;
    if (ncols == 0) ncols = 1;


    p->pat = (char **) G_malloc (nrows * sizeof(char *));
    pp = G_malloc (nrows * ncols);

    G_zero (pp, nrows*ncols);

    for (row= 0; row < nrows; row++)
    {
	p->pat[row] = pp;
	pp += ncols;
    }

    xcenter = ycenter = 0;
    count = 0;
    p->colors[0] = -1;
    for (i = 1; i < 10; i++)
	p->colors[i] = BLACK;
    row = 0;
    for (line = 0; line < nlines; line++)
    {
	if (G_getl (buf, sizeof buf, patternfd) == NULL)
	    break;
#ifdef DEBUG
printf ("%d\t[%s]\n", row, buf);
#endif
	if (scan_patcolor (buf, &i, &color) == 1)
	{
	    if (i >= 0)
		p->colors[i] = color;
	    continue;
	}
	for (col = 0; col < p->ncols && buf[col] != 0; col++)
	{
	    if (buf[col] >= '1' && buf[col] <= '9')
	    {
		p->pat[row][col] = buf[col]-'0';
		xcenter += col;
		ycenter += row;
		count++;
	    }
	}
	row++;
    }
    if (count)
    {
	ycenter /= count;
	xcenter /= count;
    }
    p->xcenter = xcenter;
    p->ycenter = ycenter;
    p->nrows = nrows;
    p->ncols = ncols;
}

get_pattern (name, p)
    char *name;
    PATTERN *p;
{
    long offset;
    int nlines, nrows, ncols;
    int file;

#ifdef DEBUG
printf("get_pattern(%s)\n",name);
#endif
    if (!find_pattern(name, &offset, &nlines, &nrows, &ncols, &file))
	return 0;

    p->nrows = nrows;
    p->ncols = ncols;

    open_pattern_file (file, 0);
    if (patternfd == NULL)
	return 0;

    fseek (patternfd, offset, 0);
    read_pattern (p, nlines);

#ifdef DEBUG
    print_pattern(p);
#endif

    return 1;
}

begin_pattern (name)
    char *name;
{
    long ftell();


    open_pattern_file (TEMP_PATTERNS, 1);
    if (patternfd)
    {
	fflush (patternfd);
	fseek (patternfd, 0L, 2);
	cur_offset = ftell (patternfd);
	cur_name = G_store (name);
	nl = nr = nc = 0;
    }
    return 1;
}

store_pattern (buf)
    char *buf;
{
    int len;
    int n, color;

#ifdef DEBUG
printf ("store_pattern(%s)\n", buf);
#endif
    if (patternfd)
    {
#ifdef DEBUG
printf ("  checking color\n");
#endif
	switch (scan_patcolor (buf, &n, &color))
	{
	case -1: return -1;
	case  1: fprintf (patternfd, "%s\n", buf);
#ifdef DEBUG
printf ("  got color(%d) for cat(%d)\n", color, n);
#endif
		 nl++;
		 return 1;
	}
	len = strlen (buf);
	fprintf (patternfd, "%s\n", buf);
	nl++;
	nr++;
	if (len > nc)
	    nc = len;
    }
    return 1;
}

end_pattern ()
{
    if (patternfd)
    {
	fflush (patternfd);
	add_pattern_to_list ((char *)NULL, cur_offset, nl, nr, nc, cur_file);
    }
}

any_patterns()
{
    return plist != NULL;
}

/* this function treats the pattern list as a circular list, ie when
   the end of the list is reached it starts again at the beginning.
*/
next_pattern (pat,first)
    PATTERN *pat;
{
    static PLIST *p ;

    if (plist == NULL)	/* any patterns? */
	return 0;
    if (first)
	p = plist;
    pat->nrows = p->nrows;
    pat->ncols = p->ncols;

    if (!open_pattern_file (p->file, 0))
	return 0;

    fseek (patternfd, p->offset, 0);
    read_pattern (pat, p->nlines);

    p = p->next;
    if (p == NULL)
	p = plist;
    return 1;
}

open_pattern_file(file,mode)
{
    char buf[1024];
    char *name;
    if (patternfile == NULL)
    {
	patternfile = G_tempfile();
#ifdef DEBUG
printf ("pattternfile is %s\n", patternfile);
#endif
	patternfd = fopen(patternfile,"w");
	if (patternfd == NULL)
		G_fatal_error ("no temp files");
	fclose(patternfd);
	patternfd = NULL;
	cur_mode = -1;
    }
    if (cur_mode == mode && file == cur_file)
	return 1;
    close_pattern_file();
    cur_file = file;
    cur_mode = mode;
    switch (file)
    {
    case GLOBAL_PATTERNS:
	sprintf (name = buf, "%s/etc/paint/patterns", G_gisbase());
	break;
    case USER_PATTERNS:
	sprintf (name = buf, "%s/patterns", G_home());
	break;
    case TEMP_PATTERNS:
	name = patternfile;
	break;
    }

    if(patternfd = fopen(name,mode?"a":"r"))
    {
	cur_mode = mode;
	return 1;
    }
    cur_mode = cur_file = -1;
    return 0;
}

close_pattern_file()
{
    if (patternfd)
	close (patternfd);
    patternfd = NULL;
    cur_file = -1;
    cur_mode = -1;
}

unlink_pattern_file()
{
    close_pattern_file();
    unlink (patternfile);
    patternfile = NULL;
}

print_pattern(p)
    PATTERN *p;
{
    int row,col;

    for (row=0; row < p->nrows; row++)
    {
	for (col = 0; col < p->ncols; col++)
	    printf ("%c", p->pat[row][col] + '0');
	printf ("\n");
    }
    printf ("colors");
    for (row = 0; row < 10; row++)
	printf (" %d", p->colors[row]);
    printf ("\n");
}
