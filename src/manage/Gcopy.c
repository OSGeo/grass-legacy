/* %W% %G% */
#define MAIN
#include "list.h"

main (argc,argv) char *argv[];
{
    int n;
    char *mapset;

    G_gisinit (argv[0]);

    read_list(0);	/* read element list from etc */

    argc--;
    if (argc != 3)
    {
	fprintf (stderr, "usage: %s element old new\n", G_program_name());
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
	fprintf (stderr, "%s: <%s> no such database element\n",
		G_program_name(), argv[1]);
	show_elements();
	exit(1);
    }

    mapset = find (n, argv[2], "");
    if (!mapset)
    {
	fprintf (stderr, "%s: %s file <%s> not found\n",
		G_program_name(), list[n].desc[0], argv[2]);
	exit(1);
    }
    if (G_legal_filename (argv[3]) < 0)
    {
	fprintf (stderr, "%s: <%s> illegal name\n", G_program_name(), argv[3]);
	exit(1);
    }
    if (strcmp (mapset, G_mapset()) == 0 && strcmp (argv[2],argv[3]) == 0)
    {
	fprintf (stderr, "%s: files are the same, no copy required\n",
	    G_program_name());
	exit(0);
    }
    do_copy (n, argv[2], mapset, argv[3]);
}
