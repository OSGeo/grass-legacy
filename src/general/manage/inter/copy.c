#define MAIN
#include "list.h"

main (argc,argv) char *argv[];
{
    int n;
    char new[300], old[300];
    char *mapset;
    char prompt[300];
    char question[400];


    G_gisinit (argv[0]);

    read_list(0);	/* read element list from etc */

    while ((n = menu(COPY)) >= 0)
    {
	sprintf (prompt, "enter %s file to be copied", list[n].desc[0]);
	while(mapset = ask_old (n, prompt, old))
	    if (ask_new (n, "", new))
	    {
		sprintf (question, "Ok to copy [%s] to [%s]? ", old, new);
		if (G_yes(question,-1))
		    do_copy (n, old, mapset, new);
	    }
    }
}
