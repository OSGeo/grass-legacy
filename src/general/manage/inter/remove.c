#define MAIN
#include "list.h"

main (argc,argv) char *argv[];
{
    int n;
    char name[300];
    char prompt[300];
    char question[400];


    G_gisinit (argv[0]);

/* read element list from etc */
    if(!read_list(1))
    {
	fprintf (stderr, "nothing to remove\n");
	exit(0);
    }

    while((n = menu(REMOVE)) >= 0)
    {
	sprintf (prompt, "enter %s file to be removed", list[n].desc[0]);
	while(ask_in_mapset (n, prompt, name))
	{
	    sprintf (question, "Ok to remove [%s]? ", name);
	    if (G_yes(question,-1))
		do_remove (n, name);
	}
    }
}
