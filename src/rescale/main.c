/* %W%  %G% */

#include "gis.h"
main()
{
    char buf[512];
    char title[200];
    char old_name[40], *old_mapset;
    char new_name[40];
    long old_min, old_max;
    long new_min, new_max;
    int flag, get_range;

    G_gisinit ("RESCALE");

    G_clear_screen();
    printf ("This program allows you to rescale a data layer so that all\n");
    printf ("the data falls with in a range that you choose\n\n");

/* ask for map to be rescaled */
    old_mapset = G_ask_cell_old ("enter map to be rescaled", old_name);
    if (!old_mapset)
	exit(0);

/* ask for new map */
    if (!G_ask_cell_new ("", new_name))
	exit(0);

/* should I determine range for user? */
    if (!quick_range (old_name, old_mapset, &old_min, &old_max))
    {
	sprintf (buf,
	    "\nWould you like me to determine the range of data in <%s>?\n",
		old_name);
	old_min = old_max = 0;
	if (G_yes(buf,1) && !slow_range (old_name, old_mapset, &old_min, &old_max))
	    printf ("\n** unable to get the range, sorry\n");
    }

/* ask user for data range to be rescaled, and title */

    strcpy (title,G_get_cell_title (old_name, old_mapset));
    G_strip (title);
    if (*title == 0) strcpy (title, old_name);
    strcat (title, " (rescaled)");

    new_min = 1;
    new_max = 255;

    ask (&old_min, &old_max, &new_min, &new_max, title);

    sprintf (buf, "Grescale '%s in %s' '%s' %ld %ld %ld %ld '%s'",
	old_name, old_mapset, new_name,
	old_min, old_max, new_min, new_max,
	title);
    system (buf);
}
