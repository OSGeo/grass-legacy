#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "global.h"

#define REPORT "v.report"
#define PAGE_LENGTH 66

static int build_command ( char *,int,int, char *, char *);
static int run( char *);

int run_report(int full)
{
    int word_cnt;
    char command[1024];
    char name[256];
    char temp[256], *temp_file;
    static int pl = PAGE_LENGTH;
    FILE *wc;

    temp_file = G_tempfile();
    build_command(command,full,0," > ",temp_file);

    if(!run(command)) exit(1);

    fprintf(stderr,"Generating Report, Please wait\n");
	/* see if a report was generated */
    sprintf(temp,"wc %s",temp_file);
    if (wc = popen(temp,"r"))
      {
      while(fgets(temp, sizeof temp, wc))
        sscanf(temp,"%d%[^\n]",&word_cnt, name);
      pclose(wc);
      if (!word_cnt) exit(1);
      }
    
    sprintf(name,"$GRASS_PAGER %s",temp_file);

    run(name);
    unlink (temp_file);

    if (!full) return -1;
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
#ifdef SYSV
	    fprintf (stderr, "Enter the printer command [lp] > ");
#else
	    fprintf (stderr, "Enter the printer command [lpr] > ");
#endif
	while (!G_gets(name));
	G_strip (name);
#ifdef SYSV
	if (*name == 0) strcpy (name, "lp");
#else
	if (*name == 0) strcpy (name, "lpr");
#endif

	build_command(command, full, pl, "|", name);
	run(command);
    }

    return 0;
}

static int build_command (
    char *command,int full,int pl,
    char *redirect,
    char *where)
{
    int i;
    int any;

    strcat (command, "'");
    sprintf (command, "%s -q pl=%d 'map=", REPORT, pl);
    for (i=0; i < nlayers; i++)
    {
	if (i) strcat (command, ",");
	strcat (command, layer[i].name);
    }
    strcat (command, "'");

    strcat (command, " 'type=");
    for (i=0; i < nlayers; i++)
    {
	if (i) strcat (command, ",");
        strcat (command, layer[i].type);
    }
    strcat (command, "'");

    if (!full)
	return -1;

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
	}
    }
    strcat (command, redirect);
    strcat (command, where);

    return 0;
}

static int run( char *command)
{
int ret; 

    ret = system(command);
    if(ret)
    {
	fprintf (stderr, "ERROR running %s\n", REPORT);
	return 0;
    }
    return 1;
}
