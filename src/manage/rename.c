/* %W% %G% */

#define MAIN
#include "list.h"

main (argc,argv) char *argv[];
{
    int n;
    char new[30], old[40];
    char buf[300];


    G_gisinit ("rename");

/* read element list from etc */
    if(!read_list(1))
    {
	fprintf (stderr, "nothing to rename\n");
	exit(0);
    }

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
	n = menu(RENAME);
    }

    if (n < 0)
	exit(0);

    while (1)
    {
	sprintf (buf, "enter %s file to be renamed", list[n].desc[0]);
	if(!ask_in_mapset (n, buf, old))
	    break;
	if (ask_new (n, "", new))
	{
	    sprintf (buf, "Grename '%s' '%s' '%s'", list[n].element[0],
		old, new);
	    system (buf);
	}
    }
}
