#include <stdio.h>
static int nlines = 100;

int show_cat (int width, char *name, int cat, char *label)
{
    if (!isatty(fileno(stdout)))
	fprintf (stdout, "%-*s (%d)%s\n", width, name, cat, label);
    fprintf (stderr, "%-*s (%d)%s\n", width, name, cat, label);
    nlines += 1;
}

int show_utm (double north, double east)
{
    if (!isatty(fileno(stdout)))
	fprintf (stdout, "\n%.2f(E) %.2f(N)\n", east, north);
    fprintf (stderr, "\n%.2f(E) %.2f(N)\n", east, north);
    nlines += 2;
}

int show_mouse (void)
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
