/* nice menus for monitor commands */

#include <stdio.h>
#include "gis.h"

#


char *get_name();
char *monitor, monitor_sv[50];

main(argc, argv) char *argv[];
{
    char choice[100];

    G_gisinit(argv[0]);
    get_monitor();
    if(monitor!=NULL)strcpy(monitor_sv, monitor);
    while (1)
    {
	G_clear_screen();
	fprintf (stderr, "                    MONITOR MENU\n");
	fprintf (stderr, "\n\n");
	fprintf (stderr, "Making sure that the graphics monitor is running\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "     1 - start a graphics monitor\n");
	fprintf (stderr, "     2 - stop a graphics monitor\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "Choosing a graphics monitor for your graphics\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "     3 - select a graphics device for output\n");
	if (monitor != NULL)
	{
	    fprintf (stderr, "         (currently selected monitor: %s)\n", monitor);
	    fprintf (stderr, "\n");
	    fprintf (stderr, "     4 - relinquish control of the graphics device\n");
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
	    select_mon();
	else if (strcmp (choice, "4") == 0 && monitor != NULL)
	    release_mon();
    }
}

list_mon()
{
    fprintf (stderr, "\nMonitors:\n\n");
    system ("d.mon -l");
    hitreturn();
}

status_mon()
{
    fprintf (stderr, "\nMonitors:\n\n");
    system ("d.mon -L");
    hitreturn();
}

select_mon()
{
    char buf[128], *name;
    while ((name = get_name("select")) != NULL)
    {
	sprintf (buf, "d.mon select=%s", name);
	system (buf);
	hitreturn();
	get_monitor();
	if (monitor != NULL)
	    break;
        else 
           {
              G__setenv("MONITOR", monitor_sv);
              G__write_env();
              /* restore old environment */
            }
    }
}

start_mon()
{
    char buf[128], *name;
    if ((name = get_name("start")) != NULL)
    {
	sprintf (buf, "d.mon start=%s", name);
	system (buf);
	get_monitor();
	if(monitor==NULL) 
	   {
	     G__setenv("MONITOR", monitor_sv);
	     G__write_env();
	     /* restore old environment */
           }
	hitreturn();
    }
}

stop_mon()
{
    char buf[128], *name;
    if ((name = get_name("stop")) != NULL)
    {
	sprintf (buf, "d.mon stop=%s", name);
	system (buf);
	get_monitor();
	hitreturn();
    }
}

release_mon()
{
    system("d.mon -r");
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
	    fprintf (stderr, "Currently selected graphics monitor: %s\n\n", monitor);
	fprintf (stderr, "Enter name of graphics monitor to %s\n", action);
	fprintf (stderr, "Enter \"list\" for a list of known monitors\n");
	fprintf (stderr, "Enter \"list -f\" for a list with current status\n");
	fprintf (stderr, "Hit RETURN to return to menu\n");
	fprintf (stderr, "> ");
	if (!G_gets(name))
	    continue;
	G_squeeze (name);
	if (*name == 0)
	    return ( (char *) NULL);
	if (strcmp (name, "list") == 0)
	{
	    list_mon();
	    continue;
	}
	if (strcmp (name, "list -f") == 0)
	{
	    status_mon();
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
