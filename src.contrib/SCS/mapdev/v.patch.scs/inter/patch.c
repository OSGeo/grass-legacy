/* %W% %G% */

#include "gis.h"

static char *intro[] =
{
"",
"",
0};

main (argc, argv) char *argv[];
{
    char command[1024];
    char name[40];
    char *mapset;
    int nfiles;
    char prompt[80];
    int i;

    G_gisinit (argv[0]);
    G_clear_screen ();

    printf ("                      VECTOR PATCH\n");
    printf ("This program allows you to patch 2 or more digit files together.\n");

    nfiles = 0;
    sprintf (command, "v.patch input="); 
    for (;;)
    {
	if (nfiles == 0)
	    sprintf (prompt, "Enter first vector(digit) map");
	else if (nfiles == 1)
	    sprintf (prompt, "Enter second map");
	else
	{
	    G_set_ask_return_msg ("if all vector patch maps have been entered");
	    sprintf (prompt, "Enter map %d (optional)", nfiles+1);
	}
        mapset = G_ask_vector_old(prompt, name) ;
	if (mapset == NULL)
	    break; 
	if (nfiles > 0) strcat (command, ",");
	strcat (command, name);
	nfiles++;
    }
    if (nfiles < 2) exit(0);

    G_clear_screen ();
    printf ("                      VECTOR PATCH\n");

    if (G_ask_vector_new ("Enter name for resultant map", name) == NULL)
	exit(0);
    strcat (command, " output=");
    strcat (command, name);
    printf ("\n\npatching ... "); fflush (stdout);

    system (command);
    exit(0);
}
