#include <stdio.h>
static int nlines = 100;

show_cat (width, name, cat, label)
    char *name, *label;
{
    if (!isatty(fileno(stdout)))
	fprintf (stdout, "%-*s (%d)%s\n", width, name, cat, label);
    fprintf (stderr, "%-*s (%d)%s\n", width, name, cat, label);
    nlines += 1;
}

show_utm (north, east)
    double north, east;
{
    if (!isatty(fileno(stdout)))
	fprintf (stdout, "\n%.2lf(E) %.2lf(N)\n", east, north);
    fprintf (stderr, "\n%.2lf(E) %.2lf(N)\n", east, north);
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

show_none ()
{
    fprintf (stderr, "No line found\n");
}
