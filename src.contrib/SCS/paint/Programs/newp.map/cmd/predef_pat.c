#include "gis.h"
#include "misc.h"
#include "pattern.h"
int 
read_predefined_patterns (void)
{
    char indexfile[300];
    static int first = 1;


    if (!first) return;
    first = 0;

    sprintf (indexfile, "%s/etc/paint/patterns.index", G_gisbase());
    read_pats (indexfile, GLOBAL_PATTERNS);

    sprintf (indexfile, "%s/patterns.index", G_home());
    read_pats (indexfile, USER_PATTERNS);
}

static 
read_pats (char *indexfile, int file)
{
    FILE *fd;
    int line;
    char name[100];
    char buf[1024];
    long offset;
    int nlines, nrows, ncols;
    static int tell = 1;

    fd = fopen (indexfile, "r");
    if (fd == NULL) return;
    if (verbose && tell)
    {
	fprintf (stdout,"loading predefined patterns ...");
	fflush (stdout);
    }

    line = 0;
    while (G_getl (buf, sizeof buf, fd))
    {
	line++;
	if (sscanf (buf, "%s %ld %d %d %d", name, &offset, &nlines, &nrows, &ncols) != 5)
	{
	    sprintf (buf, ": line %d", line);
	    error (indexfile, buf, "invalid pattern index file");
	    break;
	}
	add_pattern_to_list (name, offset, nlines, nrows, ncols, file);
    }
    fclose (fd);
    if (verbose && tell)
	fprintf (stdout,"\n");
    tell = 0;
}
