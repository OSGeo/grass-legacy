#include "gis.h"


main (argc, argv) char *argv[];
{
    char *mapset;
    double east, north;
    char ebuf[128], nbuf[128];
    char *desc;
    int full, all;
    char msg[200];
    FILE *fd;
    struct Cell_head window;

    G_gisinit (argv[0]);

    full = 0;
    all  = 0;
    if (argc > 1 && argv[1][0] == '-')
    {
	char *option;

	option = &argv[1][1];
	if (*option == 0)
	{
	    fprintf (stderr, "%s: - unknown option\n", G_program_name());
	    usage ();
	}
	while (*option)
	{
	    switch (*option)
	    {
	    case 'f': full = 1; break;
	    case 'w': all  = 1; break;
	    default:
		fprintf (stderr, "%s: - unknown option\n", G_program_name());
		usage ();
	    }
	    option++;
	}

	argc--;
	argv++;
    }

    if (argc != 2)
	usage();

    mapset = G_find_file ("site_lists", argv[1], "");
    if (mapset == NULL)
    {
	sprintf (msg, "sites file [%s] not found", argv[1]);
	G_fatal_error (msg);
    }

    if (!all)
	G_get_window (&window);
    fd = G_fopen_sites_old (argv[1], mapset);
    if (fd == NULL)
    {
	sprintf (msg, "can't open sites file [%s]", argv[1]);
	G_fatal_error (msg);
    }

    while (G_get_site (fd, &east, &north, &desc) > 0)
    {
	if (all || (east >= window.west && east <= window.east &&
		    north <= window.north && north >= window.south))
	{
	    G_format_easting (east, ebuf, -1);
	    G_format_northing (north, nbuf, -1);
	    printf ("%s %s", ebuf, nbuf);
	    if (full)
		printf (" %s", desc);
	    printf ("\n");
	}
    }
    fclose (fd);
    exit(0);
}

usage()
{
    fprintf (stderr, "Usage: %s [-fw] sitefile\n", G_program_name());
    exit(1);
}
