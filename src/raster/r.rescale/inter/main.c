#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
	char buf[512];
	char title[200];
	char old_name[100], *old_mapset;
	char new_name[100];
	long old_min, old_max;
	long new_min, new_max;

	G_gisinit (argv[0]);

	G_clear_screen();
	fprintf (stdout,"This program allows you to rescale a data layer so that all\n");
	fprintf (stdout,"the data falls with in a range that you choose\n\n");

	/* ask for map to be rescaled */
	old_mapset = G_ask_cell_old ("enter map to be rescaled", old_name);
	if (!old_mapset)
		exit(0);

	/* ask for new map */
	if (!G_ask_cell_new ("", new_name))
		exit(0);

	/* should I determine range for user? */
	get_range (old_name, old_mapset, &old_min, &old_max);

	/* ask user for data range to be rescaled, and title */
	*title = 0;
	new_min = 0;
	new_max = 255;

	ask (&old_min, &old_max, &new_min, &new_max, title);

	sprintf (buf,
	    "r.rescale input='%s' from=%ld,%ld output='%s' to=%ld,%ld",
	    G_fully_qualified_name(old_name, old_mapset), old_min, old_max,
	    new_name, new_min, new_max);
	if (*title)
	{
		strcat (buf, " title='");
		strcat (buf, title);
		strcat (buf, "'");
	}
	system (buf);

	exit(0);
}
