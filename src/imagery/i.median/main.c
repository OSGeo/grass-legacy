#define GLOBAL
#include "globals.h"


main(argc,argv) 
    char *argv[];
{
    char temp[60];
    char title[100];
    int n;

    G_gisinit (argv[0]);
    if (!I_ask_group_old ("", group))
	exit(0);

    I_get_group_ref (group, &ref);

    if (ref.nfiles <= 0)
    {
	printf ("no files in imagery group %s\n", group);
	exit(0);
    }

/* allow user to set/change color configuration for group */
    sprintf (temp, "GROUP: %s", group);
    I_location_info (title, temp);
    if(!I_ask_ref_colors (title, &ref))
	exit(0);
    I_put_group_ref (group, &ref);
    switch (I_read_group_colors (group, &ref))
    {
	case -1: G_fatal_error ("Can't read group color tables");
	case 0:  G_fatal_error ("No color assignment for group");
    }

/* get color levels */
    r_level = g_level = b_level = ask_levels();


    if (!G_ask_cell_new ("", result))
	exit(0);

    compose ();

/* associate the result with the group */
    I_add_file_to_group_ref (result, G_mapset(), &ref);
    I_put_group_ref (group, &ref);
    exit(0);
}
