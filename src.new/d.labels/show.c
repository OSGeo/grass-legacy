#include <stdio.h>

show_utm (north, east)
    double north, east;
{
if (!isatty(fileno(stdout)))
   fprintf (stdout, "\n%.2lf(N) %.2lf(E)\n", north, east);
fprintf (stderr, "\n%.2lf(N) %.2lf(E)\n", north, east);
}

show_mouse ()
{
fprintf (stderr, "\n");
fprintf (stderr, "Buttons\n");
fprintf (stderr, " Left:    Place Label Here\n");
fprintf (stderr, " Middle:  Where Am I ?\n");
fprintf (stderr, " Right:   Quit\n");
}
