/* nice menus for futil commands */

#include <stdio.h>
#include "gis.h"

#


char *get_name();

main(argc, argv) char *argv[];
{
    char choice[100];

    G_gisinit(argv[0]);
    while (1)
    {
	G_clear_screen();
	fprintf (stderr, "                    FUTIL MENU\n");
	fprintf (stderr, "\n\n");
	fprintf (stderr, "     1 - merge two ascii files\n");
	fprintf (stderr, "     2 - re-arrange the column order of an ascii file\n");
	fprintf (stderr, "     3 - Divide a single record ascii file intomultiple records\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "RETURN - quit\n");
	fprintf (stderr, "\n> ");

	if (!G_gets(choice))
	    continue;
	G_strip (choice);

	if (*choice == 0)
	    exit (0);
	else if (strcmp (choice, "1") == 0)
	    run("merge");
	else if (strcmp (choice, "2") == 0)
	    run("rearng");
	else if (strcmp (choice, "3") == 0)
	    run("rewrite");

    }
}

run(pgm)
    char *pgm;
{
    char command[1024];
    char *getenv();

    sprintf (command, "%s/etc/%s", G_gisbase(), pgm);
    return system(command);
}
