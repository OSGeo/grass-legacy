#include <string.h>
#include "vask.h"
#include <unistd.h>
#include "gis.h"
int 
ask_perms (int *group, int *other)
{
    char vgroup[2];
    char vother[2];
    char title[80];

    strcpy (vgroup, *group ? "x" : "");
    strcpy (vother, *other ? "x" : "");


    sprintf (title, "LOCATION: %-40s MAPSET: %s", G_location(), G_mapset());
    V_clear();

    V_line (0, title);
    V_line (2, "This program allows you to control access to your mapset by other users.");
    V_line (3, "Access may be granted/removed for everyone, or for everyone in your group.");

    V_line (5, "Mark an 'x' to allow access, erase the field to restrict access.");

    V_line (9, "      GROUP: ___");
    V_line (10, "      OTHER: ___");

    V_ques (vgroup, 's', 9, 14, 1);
    V_ques (vother, 's', 10, 14, 1);

    V_intrpt_ok();
    if (!V_call())
    {
	fprintf (stdout,"Permissions NOT changed!\n");
	sleep(2);
	exit(0);
    }

    *group = (vgroup[0] != 0);
    *other = (vother[0] != 0);

    return 0;
}
