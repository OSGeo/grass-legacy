/* %W% %G% */
#define MAIN
#include "list.h"

main (argc,argv) char *argv[];
{
    int n;
    char new[30], old[40];
    char *mapset;
    char buf[300];


    G_gisinit ("copy");

    read_list(0);	/* read element list from etc */

    argc--;
    if (argc > 0)	/* user has specifed an element */
    {
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
    }
    else
    {
	n = menu(COPY);
    }

    if (n < 0)
	exit(0);

    while (1)
    {
	sprintf (buf, "enter %s file to be copied", list[n].desc[0]);
	mapset = ask_old (n, buf, old);
	if (!mapset) break;
	if (ask_new (n, "", new))
	{
	    sprintf (buf, "Gcopy '%s' '%s in %s' '%s'", list[n].element[0],
		old, mapset, new);
	    system (buf);
	}
    }
}
