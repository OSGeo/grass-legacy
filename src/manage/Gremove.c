/* %W% %G% */
#define MAIN
#include "list.h"

main (argc,argv) char *argv[];
{
    int n;
    int i;


    G_gisinit ("Gremove");

    read_list(0);	/* read element list from etc */

    argc--;
    if (argc < 2)
    {
	fprintf (stderr, "usage: %s element file\n", G_program_name());
	show_elements();
	exit (1);
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
	fprintf (stderr,"%s: %s - no such database element\n",
	    G_program_name(), argv[1]);
	show_elements();
	exit(1);
    }

    for (i = 2; i <= argc; i++)
	do_remove (n, argv[i]);
}
