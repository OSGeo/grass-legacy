#include "imagery.h"
main(argc, argv) char *argv[];
{
    char *group;
    char temp[80];
    char title[80];
    struct Ref ref;

    if (argc != 2)
    {
	fprintf (stderr, "usage: %s group\n", argv[0]);
	exit(1);
    }

    G_gisinit (argv[0]);
    group = argv[1];
    if (!I_find_group (group))
    {
	fprintf (stderr, "** Group [%s] not found\n", group);
	exit(1);
    }

    I_get_group_ref (group, &ref);
    if (ref.nfiles <= 0)
    {
	fprintf (stderr, "** Group [%s] contains no files.\n", group);
	exit(1);
    }
    sprintf (temp, "GROUP: %s", group);
    I_location_info (title, temp);

    if(I_ask_ref_colors (title, &ref) && (I_put_group_ref (group, &ref) >= 0))
	printf ("Group colors updated!\n");
    else
	printf ("Group colors not updated!\n");
    exit(1);
}

