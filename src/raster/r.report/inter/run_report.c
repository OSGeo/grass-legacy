#include "global.h"
#define REPORT "r.report"
#define PAGE_LENGTH 66

static int z_option = 0;

run_report(full)
{
    char command[1024];
    char name[256];
    char temp[256];
    static int pl = PAGE_LENGTH;

    if(!full)
    {
	if(G_yes(
         "\nWould you like to filter out zero category data?", -1))
          z_option=1;
    }
    build_command(command,full,0,"|","more");
    if(!run(command)) exit(1);
    if (!full) return;
    while (G_yes("\nWould you like to save this report in a file? ", -1))
    {
askfile:
	do
	    fprintf (stderr, "Enter file name: ");
	while (!G_gets(name));
	if (*name == 0) continue;
	if (access (name, 0) == 0)
	{
	    sprintf (temp, "%s exists. Ok to overwrite? ", name);
	    if (!G_yes(temp,-1)) goto askfile;
	}
	do
	    fprintf (stderr, "If you want the report paginated, enter the number of lines per page.\nOtherwise just hit RETURN\n>  ");
	while (!G_gets(temp));

	if (sscanf(temp, "%d", &pl) != 1 || pl < 0)
	    pl = 0;
	build_command(command,full,pl,">",name);
	if(run(command))
	    fprintf (stderr, "Report saved in <%s>\n", name);
	break;
    }
    if (pl <= 0) pl=PAGE_LENGTH;
    if (G_yes("\nWould you like to print this report? ", -1))
    {
	do
	    fprintf (stderr, "Enter the number of lines per page [%d] > ", pl);
	while (!G_gets(temp));
	sscanf (temp, "%d", &pl);
	if (pl < 0) pl = 0;

	do
	    fprintf (stderr, "Enter the printer command [lpr] > ");
	while (!G_gets(name));
	G_strip (name);
	if (*name == 0) strcpy (name, "lpr");

	build_command(command, full, pl, "|", name);
	run(command);
    }
}

static
build_command (command, full, pl, redirect, where)
    char *command;
    char *redirect;
    char *where;
{
    int i;
    int any;
    int format_needed=0;

    sprintf (command, "%s '%s%s' pl=%d 'map=", REPORT, full?"<":">",stats_file,pl);
    for (i=0; i < nlayers; i++)
    {
	if (i) strcat (command, ",");
	strcat (command, G_fully_qualified_name(layer[i].name, layer[i].mapset));
    }
    strcat (command, "'");
    if (!full)
    {
        if(z_option) strcat (command, " -z");
	return;
    }

    any = 0;
    for (i = 0; units[i].name; i++)
    {
	if (units[i].marked[0])
	{
	    if (any++)
		strcat (command, ",");
	    else
		strcat (command, " units=");
	    strcat (command, units[i].code);
	    if(i<=4) format_needed = 1;
	}
    }
    if(format_needed) 
    if(G_yes(
      "\nWould you like to use scientific notation for very large numbers?",
      -1))
          strcat (command, " -e ");
    strcat (command, redirect);
    strcat (command, where);
}

static
run(command)
    char *command;
{
    if(system(command))
    {
	fprintf (stderr, "ERROR running %s\n", REPORT);
	return 0;
    }
    return 1;
}
