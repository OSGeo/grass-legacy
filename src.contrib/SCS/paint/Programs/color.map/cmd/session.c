#include "gis.h"
static char cur[2000];
static char *sessionfile = NULL;
static FILE *fd;

add_to_session(indent,buf)
    char *buf;
{
    accept ();
    sprintf (cur, "%s%s", indent?"  ":"", buf);
}

accept()
{
    if (sessionfile == NULL)
    {
	*cur = 0;
	sessionfile = G_tempfile();
	fd = fopen (sessionfile, "w");
	if (fd == NULL)
	{
	    error ("session file","","can't open");
	    return;
	}
    }
    if (fd != NULL && *cur)
    {
	fprintf (fd, "%s\n", cur);
	fflush (fd);
	*cur = 0;
    }
}

reject()
{
    *cur = 0;
}

print_session(out)
    FILE *out;
{
    FILE *in;
    char buf[1024];

    if (sessionfile == NULL)
	return;
    if (fd != NULL)
	fflush (fd);
    if ((in = fopen (sessionfile, "r")) == NULL)
    {
	error ("session file","","can't open");
	return;
    }
    while (fgets(buf, sizeof buf, in))
	fprintf (out, "%s",buf);
    fclose (in);
}
