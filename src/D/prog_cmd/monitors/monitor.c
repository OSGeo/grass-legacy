/* nice menus for monitor commands */

#include <stdio.h>
#include "gis.h"

#


char *get_name();
char *monitor;

main(argc, argv) char *argv[];
{
    char choice[100];

    G_gisinit(argv[0]);
    get_monitor();
    while (1)
    {
	G_clear_screen();
	fprintf (stderr, "                    MONITOR MENU\n");
	fprintf (stderr, "\n\n");
	fprintf (stderr, "Making sure that the graphics monitor is running\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "     1 - start a graphics monitor\n");
	fprintf (stderr, "     2 - stop a graphics monitor\n");
	fprintf (stderr, "     3 - list the available graphics monitors\n");
	fprintf (stderr, "     4 - print the current status for available graphics monitors\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "Choosing a graphics monitor for your graphics\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "     5 - select a graphics device for output\n");
	if (monitor != NULL)
	{
	    fprintf (stderr, "         (currently selected monitor: %s)\n", monitor);
	    fprintf (stderr, "\n");
	    fprintf (stderr, "     6 - relinquish control of the graphics device\n");
	    fprintf (stderr, "         (let someone else use it)\n");
	}
	fprintf (stderr, "\n");
	fprintf (stderr, "RETURN - quit\n");
	fprintf (stderr, "\n> ");

	if (!G_gets(choice))
	    continue;
	G_strip (choice);

	if (*choice == 0)
	    return 0;
	else if (strcmp (choice, "1") == 0)
	    start_mon();
	else if (strcmp (choice, "2") == 0)
	    stop_mon();
	else if (strcmp (choice, "3") == 0)
	    list_mon();
	else if (strcmp (choice, "4") == 0)
	    status_mon();
	else if (strcmp (choice, "5") == 0)
	    select_mon();
	else if (strcmp (choice, "6") == 0 && monitor != NULL)
	    release_mon();
    }
}

list_mon()
{
    fprintf (stderr, "\nMonitors:\n\n");
    system ("Dlist.mon");
    hitreturn();
}

status_mon()
{
    fprintf (stderr, "\nMonitor status:\n\n");
    system ("Dstatus.mon");
    hitreturn();
}

select_mon()
{
    char buf[128], *name;
    while ((name = get_name("select")) != NULL)
    {
	sprintf (buf, "Dselect.mon %s", name);
	system (buf);
	hitreturn();
	get_monitor();
	if (monitor != NULL)
	    break;
    }
}

start_mon()
{
    char buf[128], *name;
    if ((name = get_name("start")) != NULL)
    {
	sprintf (buf, "Dstart.mon %s", name);
	system (buf);
	hitreturn();
    }
}

stop_mon()
{
    char buf[128], *name;
    if ((name = get_name("stop")) != NULL)
    {
	sprintf (buf, "Dstop.mon %s", name);
	system (buf);
	hitreturn();
    }
}

release_mon()
{
    system("Drelease.mon");
    hitreturn();
    get_monitor();
}

/* get_name - get name of monitor; returns NULL if none given */

char *
get_name (action)
    char *action;
{
    static char name[100];

    fprintf (stderr, "\n");
    while (1)
    {
	if (monitor)
	    fprintf (stderr, "Currently select graphics monitor: %s\n\n", monitor);
	fprintf (stderr, "Enter name of graphics monitor to %s\n", action);
	fprintf (stderr, "Enter \"list\" for a list of known monitors\n");
	fprintf (stderr, "Hit RETURN to return to menu\n");
	fprintf (stderr, "> ");
	if (!G_gets(name))
	    continue;
	G_strip (name);
	if (*name == 0)
	    return ( (char *) NULL);
	if (strcmp (name, "list") == 0)
	{
	    list_mon();
	    continue;
	}
	return(name);
    }
}

hitreturn()
{
    char buf[512];

    fprintf (stderr, "\nhit RETURN -> ");
    G_gets(buf);
}
get_monitor()
{
    G__read_env();
    monitor = G__getenv ("MONITOR");
}
