#include <stdio.h>
#include <unistd.h>

int 
show_utm (double north, double east)
{
if (!isatty(fileno(stdout)))
   fprintf (stdout, "\n%.2f(N) %.2f(E)\n", north, east);
fprintf (stderr, "\n%.2f(N) %.2f(E)\n", north, east);

return 0;
}

int 
show_mouse (void)
{
fprintf (stderr, "\n");
fprintf (stderr, "Buttons\n");
fprintf (stderr, " Left:    Place Label Here\n");
fprintf (stderr, " Middle:  Quit\n");
fprintf (stderr, " Right:   Where Am I ?\n");

return 0;
}

int 
show_menu1 (void)
{
fprintf (stderr, "\n");
fprintf (stderr, "Buttons\n");
fprintf (stderr, " Left:    Edit existing Labels\n");
fprintf (stderr, " Middle:  Quit\n");
fprintf (stderr, " Right:   Add more Labels\n");

return 0;
}

int 
show_menu2 (void)
{
fprintf (stderr, "\n");
fprintf (stderr, "Buttons\n");
fprintf (stderr, " Left:    Accept this label \n");
fprintf (stderr, " Middle:  Quit\n");
fprintf (stderr, " Right:   Reject this label\n");

return 0;
}

int 
show_loc (void)
{
fprintf (stderr, "\n");
fprintf (stderr, "Buttons\n");
fprintf (stderr, " Left:    Select this label \n");
fprintf (stderr, " Middle:  Quit\n");
fprintf (stderr, " Right:   Where Am I ?\n");

return 0;
}
