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
fprintf (stderr, " Middle:  Quit\n");
fprintf (stderr, " Right:   Where Am I ?\n");
}

show_menu1 ()
{
fprintf (stderr, "\n");
fprintf (stderr, "Buttons\n");
fprintf (stderr, " Left:    Edit existing Labels\n");
fprintf (stderr, " Middle:  Quit\n");
fprintf (stderr, " Right:   Add more Labels\n");
}

show_menu2 ()
{
fprintf (stderr, "\n");
fprintf (stderr, "Buttons\n");
fprintf (stderr, " Left:    Accept this label \n");
fprintf (stderr, " Middle:  Quit\n");
fprintf (stderr, " Right:   Reject this label\n");
}

show_loc ()
{
fprintf (stderr, "\n");
fprintf (stderr, "Buttons\n");
fprintf (stderr, " Left:    Select this label \n");
fprintf (stderr, " Middle:  Quit\n");
fprintf (stderr, " Right:   Where Am I ?\n");
}
