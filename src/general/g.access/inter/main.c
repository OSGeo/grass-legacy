#include "gis.h"
#include "local_proto.h"
#include <unistd.h>

int 
main (int argc, char *argv[])
{
    char path[1024];
    int perms; /* full mapset permissions */
    int group, other; /* bool. want group/other permission */

/* init the GRASS library */
    G_gisinit(argv[0]);

/* get the unix file name for the mapset directory */
    G__file_name (path, "", "", G_mapset());

/* get the current permissions */
    if(get_perms (path, &perms, &group, &other) < 0)
	G_fatal_error ("Can't determine mapset permssions");

/* this part is until PERMANENT no longer holds DEFAULT_WIND and MYNAME */
    if (strcmp (G_mapset(), "PERMANENT") == 0)
    {
	fprintf (stdout,"NOTE: access to %s must be open under GRASS 3.0\n", G_mapset());
	set_perms (path, perms, 1, 1);
	exit(0);
    }

    do
	ask_perms (&group, &other);
    while (!verify_perms(group,other));

    set_perms (path, perms, group, other);

    return 0;
}
