#include "global.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#define REPORT "r.report"
#define PAGE_LENGTH 66

static int no_nulls = 0;
static int no_nulls_all = 0;
static int run( char *);
static int build_command(char *,int,int,char *,char *,char *,int,int,int,int);

int run_report(int full)
{
    char command[1024];
    char name[256];
    char temp[256];
    static char nv[256];
    static int pl = PAGE_LENGTH;
    static int fp = 0;
    static int as_int = 0;
    static int cat_ranges = 0;
    static int nsteps=255;
    int i;

    for (i=0; i < nlayers; i++)
    {
	fp = G_raster_map_is_fp(layer[i].name, layer[i].mapset);
	if(fp) break;
    }
    if(!full)
    {
	if(G_yes(
         "\nWould you like to filter out all NULL data?", -1))
          no_nulls=1;
        else
	   if(G_yes(
            "\nWould you like to filter out cells where all input maps have NULL data?", -1))
             no_nulls_all=1;

	do
	    fprintf (stderr, "Enter the string representing no data cells [*] > ");
	while (!G_gets(nv));
	G_strip(nv);
	if (*nv == 0) strcpy (nv, "*");
	if(fp)
	{
 	   if(G_yes("\nWould you like to read floating point maps as int ?", 0))
	        as_int = 1;
           else
	   {
	       fprintf (stderr,"\n");
	       fprintf (stderr,
		  "For floating point maps stats can be reported either for\n");
	       fprintf (stderr,
		  "fp ranges specified and labeled in cats file\n");
               fprintf (stderr,
		  "or for n subdivisions of floating point data range of\n");
	       fprintf (stderr,
		  "the map, where n is specified by user\n");
	      while(1)
	      {
	         fprintf (stderr,"\n");
	         fprintf (stderr,"Please enter 0 for report on cats fp ranges\n");
	         fprintf (stderr,"or enter int number of steps to divide fp range into> ");
	         if(G_gets(temp) && sscanf(temp, "%d", &nsteps)==1) 
	         {
		     if(nsteps==0)
		     {
	  	        cat_ranges=1;
			nsteps=255;
			break;
                     }
	             else if(nsteps < 0)
	                fprintf(stderr, "only non-negative numbers allowed!\n");
                     else
		         break;
                 }
              }
           } /* read fp maps as fp */
	} /* some maps are fp */
    }
    build_command(command,full,0,"|","$GRASS_PAGER",nv, as_int, cat_ranges, nsteps, fp);
    if(!run(command)) exit(1);
    if (!full) return 0;
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
	build_command(command,full,pl,">",name,nv, as_int, cat_ranges, nsteps, fp);
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

	build_command(command, full, pl, "|", name, nv, as_int, cat_ranges, nsteps, fp);
	run(command);
    }
    return 0;
}

static int build_command (
    char *command, int full,int pl,
    char *redirect,
    char *where,
    char *nv, int as_int,
    int cat_ranges,
    int nsteps,
    int fp)
{
    int i;
    int any;
    int format_needed=0;
    char tmp[50];

    sprintf (command, "%s '%s%s' pl=%d null=%s 'map=", REPORT, full?"<":">",stats_file,pl,nv);
    for (i=0; i < nlayers; i++)
    {
	if (i) strcat (command, ",");
	strcat (command, G_fully_qualified_name(layer[i].name, layer[i].mapset));
    }
    strcat (command, "'");
    if(as_int) 
           strcat (command, " -i");
    else if(cat_ranges)
           strcat (command, " -C");
    else if(fp)
    {
       sprintf(tmp, " nsteps=%d", nsteps);
       strcat(command, tmp);
    }
    if (!full)
    {
        if(no_nulls) strcat (command, " -n");
        if(no_nulls_all) strcat (command, " -N");
	return 0;
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

    return 0;
}

static int run( char *command)
{
    if(system(command))
    {
	fprintf (stderr, "ERROR running %s\n", REPORT);
	return 0;
    }
    return 1;
}
