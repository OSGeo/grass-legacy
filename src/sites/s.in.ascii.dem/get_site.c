#include "gis.h"
get_site (fd, east, north, desc, fs)
    FILE *fd;
    double *east, *north;
    char *desc;
    char *fs;
{
    char ibuf[1024], buf[1024], *b;
    char ebuf[256], nbuf[256];
    static int first = 1;
    static int line = 0;
    static int tty;
    static int proj;

    if (first)
    {
	tty = isatty(fileno(fd));
	proj = G_projection();
	if(tty)
	{
	    printf ("Enter sites, one per line, in the format: east north description\n");
	    printf ("When finished, type: end\n");
	}
	first = 0;
    }
    if (tty)
	printf ("east north desc> ");
    else
	line++;

    if (!G_getl(ibuf, sizeof ibuf, fd)) 
	return 0;
    strcpy (buf, ibuf);
    G_squeeze (buf);
    if (*buf == 0)
	return -1;
    if (strcmp (buf, "end") == 0) return 0;
    if (fs)
    {
	for (b = buf; *b; b++)
	    if (*b == *fs)
	    {
		*b = ' ';
		break;
	    }
	for (; *b; b++)
	    if (*b == *fs)
	    {
		*b = ' ';
		break;
	    }
    }
    if (sscanf (buf, "%s %s", ebuf, nbuf) != 2
    || !G_scan_easting (ebuf, east, proj)
    || !G_scan_northing (nbuf, north, proj))
    {
	if (!tty)
	{
	    fprintf (stderr, "%s - line %d ",
		G_program_name(), line);
	}
	fprintf (stderr, "** invalid format **\n");
	if (!tty)
	    fprintf (stderr, "<%s>\n", ibuf);
	return -1;
    }
    *desc = 0;
    sscanf (buf, "%s %s %[^\n]", ebuf, nbuf, desc);
    return 1;
}
