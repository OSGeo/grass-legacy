/* %W% %G% */

#define MAIN
#include "list.h"

main (argc,argv) char *argv[];
{
    int n;
    char buf[300];


    G_gisinit ("list");

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

	if (n >= 0)
	    do_list (n,"");

	exit(0) ;
    }

    for(;;)
    {
	n = menu(LIST);

	if (n >= 0)
	    do_list (n,"");
	else
	    exit(0) ;
    }
}
