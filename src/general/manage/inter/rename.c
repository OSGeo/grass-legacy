#define MAIN
#include "list.h"

main (argc,argv) char *argv[];
{
    int n;
    char new[300], old[300];
    char prompt[300];
    char question[400];


    G_gisinit (argv[0]);

/* read element list from etc */
    if(!read_list(1))
    {
	fprintf (stderr, "nothing to rename\n");
	exit(0);
    }

    while ((n = menu(RENAME)) >= 0)
    {
	sprintf (prompt, "enter %s file to be renamed", list[n].desc[0]);
	while(ask_in_mapset (n, prompt, old))
	{
	    if (ask_new (n, "", new))
	    {
		sprintf (question, "Ok to rename [%s] to [%s]? ", old, new);
		if (G_yes(question,-1))
		    do_rename (n, old, new);
	    }
	}
    }
}
