#include "gis.h"

static int nlines = 100;

show_cat (width, name, cat, label)
    char *name, *label;
{
    if (!isatty(fileno(stdout)))
	fprintf (stdout, "%-*s (%d)%s\n", width, name, cat, label);
    fprintf (stderr, "%-*s (%d)%s\n", width, name, cat, label);
    nlines += 1;
}

show_utm (north, east, window)
    double north, east;
    struct Cell_head *window;
{
    char e[50], n[50];

    G_format_northing (north, n, window->proj);
    G_format_easting  (east,  e, window->proj);
    if (window->proj != PROJECTION_LL)
    {
	strcat (n, "(N)");
	strcat (e, "(E)");
    }
    if (!isatty(fileno(stdout)))
	fprintf (stdout, "\n%s %s\n", e, n);
    fprintf (stderr, "\n%s %s\n", e, n);
    nlines += 2;
}

show_mouse ()
{
    if (nlines >= 18)
    {
	fprintf (stderr, "\n");
	fprintf (stderr, "Buttons\n");
	fprintf (stderr, " Left:  what's here\n");
	fprintf (stderr, " Right: quit\n");
	nlines = 4;
    }
}
