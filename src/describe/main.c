/* %W% %G% */

#include "gis.h"

main(argc,argv) char *argv[];
{
    char name[50], *mapset;
    char command[1024];
    char options[10];
    int range;
    int windowed;
    int full;

    G_gisinit (argv[0]);
    mapset = G_ask_cell_old ("enter cell file to be described", name);
    if (mapset == NULL)
	exit(0);
    full = 1;
    while(1)
    {
	while(1)
	{
	    if (full)
	    {
	    printf ("\nPlease select the report desired\n");
	    printf ("    1.  Data range only (relatively fast)\n");
	    printf ("    2.  Full data list (relatively slow)\n");
	    }
	    printf ("> ");
	    if (G_gets(command)) break;
	    full = 1;
	}
	G_strip (command);

	full = 0;
	if (*command == 0) continue;
	full = 1;

	if (strcmp (command,"1") == 0)
	{
	    range = 1;
	    break;
	}
	if (strcmp (command,"2") == 0)
	{
	    range = 0;
	    break;
	}
    }

    full = 1;
    while(1)
    {
	while(1)
	{
	    if (full)
	    {
	    printf ("\nWould you like the cell file read\n");
	    printf ("    1.  directly\n");
	    printf ("    2.  within the current window\n");
	    }
	    printf ("> ");
	    if (G_gets(command)) break;
	    full = 1;
	}
	G_strip (command);
	full = 0;
	if (*command == 0) continue;
	full = 1;
	if (strcmp (command,"1") == 0)
	{
	    windowed = 0;
	    break;
	}
	if (strcmp (command,"2") == 0)
	{
	    windowed = 1;
	    break;
	}
    }

    *options = 0;
    if (range || windowed)
	sprintf (options, "-%s%s", range?"r":"", windowed?"w":"");
    printf ("one moment...\n");
    sprintf (command, "Gdescribe %s '%s in %s'", options ,name, mapset);
    execlp ("/bin/sh", "sh", "-c", command, 0);
}
