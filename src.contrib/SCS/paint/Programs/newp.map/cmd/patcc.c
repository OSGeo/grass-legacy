#include <stdio.h>

main (argc, argv) char *argv[];
{
    char name[100];
    char buf[1024];
    int in_pat;
    int nlines, nrows, ncols;
    int len;
    FILE *patfd, *indexfd;
    long ftell();

    if (argc != 2)
    {
	fprintf (stderr, "usage: %s patternfile\n", argv[0]);
	exit(1);
    }
    patfd = fopen (argv[1], "r");
    if (patfd == NULL)
    {
	perror (argv[1]);
	exit(1);
    }
    sprintf (buf, "%s.index", argv[1]);
    indexfd = fopen (buf, "w");
    if (indexfd == NULL)
    {
	perror (buf);
	exit(1);
    }

    in_pat = 0;
    while (fgets (buf, sizeof buf, patfd))
    {
	if (sscanf (buf, "defpat %s", name) == 1)
	{
	    if (in_pat)
		fprintf (indexfd, "%d %d %d\n", nlines, nrows, ncols);
	    fprintf (indexfd, "%s %ld ", name, ftell(patfd));
	    nrows = 0;
	    ncols = 0;
	    nlines = 0;
	    in_pat = 1;
	    continue;
	}
	if (sscanf (buf, "%s", name) == 1 && strcmp (name, "end") == 0)
	{
	    if (in_pat)
		fprintf (indexfd, "%d %d %d\n", nlines, nrows, ncols);
	    in_pat = 0;
	}
	if (!in_pat) continue;
	nlines++;
	if (sscanf (buf, "color %1s", name) == 1)
	{
	    continue;
	}
	len = strlen(buf)-1; /* ignore newline */
	if (len > ncols) ncols = len;
	nrows++;
    }
    if (in_pat)
	fprintf (indexfd, "%d %d %d\n", nlines, nrows, ncols);

    fclose (patfd);
    fclose (indexfd);
    exit(0);
}
