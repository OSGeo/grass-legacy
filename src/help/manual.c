#include "gis.h"

static char *tempfile = NULL;

manual (entry, find)
    char *entry;
{
    if (tempfile == NULL)
	tempfile = G_tempfile();
    return (showman (entry, 1, find) || showman (entry, 2, find));
}

showman (entry, section, find)
    char *entry;
{
    char buf[1024];
    char temp[3];
    FILE *in, *out;
    int blanks;

    sprintf (buf, "%s/man/%d/%s", G_gisbase(), section, entry);
    if ((in = fopen (buf, "r")) == NULL)
	return 0;
    if (find)
    {
	fclose (in);
	return 1;
    }

/* copy entry to temp file, squeezing multiple blank lines into 1 */
    out = fopen (tempfile, "w");
    if (out == NULL)
    {
	fprintf (stderr, "\7WARNING: Can't open any temp files\n");
	return 0;
    }
    blanks = 0;
    while (fgets (buf, sizeof buf, in))
    {
	if (sscanf (buf, "%1s", temp) != 1)
	    blanks = 1;
	else
	{
	    if (blanks)
		fprintf (out, "\n");
	    fprintf (out, "%s", buf);
	    blanks = 0;
	}
    }
    fclose (in);
    fclose (out);

    sprintf (buf, "more -d %s", tempfile);
    system (buf);
    unlink (tempfile);
    if (!G_yes("\nWould you like to send this entry to the printer? ", 0))
	return 1;
    printf ("Sending to the printer ...\n");
    sprintf (buf, "lpr %s/man/%d/%s", G_gisbase(), section, entry);
    system (buf);
    return 1;
}
