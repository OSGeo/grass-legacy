/* %W% %G% */
#define MAIN
#include "list.h"

main (argc,argv) char *argv[];
{
    int n;
    char buf[300];
    static char *mapset = "";
    char *pgm;
    int full;


    G_gisinit (pgm = argv[0]);

    read_list(0);	/* read element list from etc */

    argc--;
    full = 0;
    if (argc >= 1)
    {
	if (strcmp (argv[1],"-f") == 0)
	{
	    argc--;
	    argv++;
	    full = 1;
	}
    }
    if (argc < 1)	/* user must specify an element */
    {
	fprintf (stderr, "usage: %s element [mapset]\n", G_program_name());
	show_elements();
	exit(1);
    }

    for (n = 0 ; n < nlist; n++)
    {
	if (strcmp (list[n].element[0], argv[1]) == 0)
	    break;
	if (strcmp (list[n].alias, argv[1]) == 0)
	    break;
    }

    if (n >= nlist)
    {
	fprintf (stderr, "%s: %s - no such database element\n",
	    G_program_name(), argv[1]);
	show_elements();
	exit(1);
    }
    if (argc > 1)
	mapset = argv[2];
    if (strcmp (mapset,".") == 0)
	mapset = G_mapset();
    if (full)
    {
	char lister[300];
	sprintf (lister, "%s/etc/lister/%s", G_gisbase(), argv[1]);
	if (access (lister, 1) == 0) /* execute permission? */
	    execl (lister, pgm, mapset, 0);
    }
    do_list (n,mapset);
}
