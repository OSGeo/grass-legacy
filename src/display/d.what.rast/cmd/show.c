#include "gis.h"

static int nlines = 100;

show_cat (width, mwidth, name, mapset, cat, label, terse, fs)
    char *name, *mapset, *label;
    int terse;
    char * fs;
{
    char *fname;
    if (terse)
    {
	fname = G_fully_qualified_name(name,mapset);
	if (!isatty(fileno(stdout)))
	    fprintf (stdout, "%s%s%d%s%s\n", fname, fs, cat, fs, label);
	fprintf (stderr, "%s%s%d%s%s\n", fname, fs, cat, fs, label);
    }
    else
    {
	if (!isatty(fileno(stdout)))
	    fprintf (stdout, "%*s in %-*s (%d)%s\n", width, name, mwidth, mapset, cat, label);
	fprintf (stderr, "%*s in %-*s (%d)%s\n", width, name, mwidth, mapset, cat, label);
    }
    nlines += 1;
}

show_utm (north, east, window, terse, button, fs)
    double north, east;
    struct Cell_head *window;
    int terse, button;
    char * fs;
{
    char e[50], n[50];

    if (window->proj == PROJECTION_LL && !isatty (fileno (stdout)))
    {
	/* format in decimal rather than d.m.s */
	G_format_northing (north, n, -1);
	G_format_easting  (east,  e, -1);
    }
    else
    {
	G_format_northing (north, n, window->proj);
	G_format_easting  (east,  e, window->proj);
    }

    if (terse)
    {
	if (!isatty(fileno(stdout)))
	    fprintf (stdout, "\n%s%s%s%s%d\n", e, fs, n, fs, button);
	fprintf (stderr, "\n%s%s%s%s%d\n", e, fs, n, fs, button);
    }
    else
    {
	if (window->proj != PROJECTION_LL)
	{
	    strcat (n, "(N)");
	    strcat (e, "(E)");
	}

	if (!isatty(fileno(stdout)))
	    fprintf (stdout, "\n%s %s\n", e, n);
	fprintf (stderr, "\n%s %s\n", e, n);
    }
    nlines += 2;
}

show_buttons (once)
{
    if (once)
    {
	fprintf (stderr, "\nClick mouse button on desired location\n\n");
	nlines = 3;
    }
    else if (nlines >= 18)	/* display prompt every screen full */
    {
	fprintf (stderr, "\n");
	fprintf (stderr, "Buttons\n");
	fprintf (stderr, " Left:  what's here\n");
	fprintf (stderr, " Right: quit\n");
	nlines = 4;
    }
}
