#include <string.h>
#include <unistd.h>
#include "gis.h"
#ifndef FILE
#include <stdio.h>
#endif

extern FILE *out;

int open_outfile (char *file)
{
    char *home;
    char name[200];
    int print_prompt;


    if (!(home = G_home()))
    {
	fprintf (stdout,"cant find home directory\n");
	exit(-1);
    }
    print_prompt = 1;

    while (1)
    {
	if (print_prompt)
	{
	fprintf (stdout,"\nenter the name of a file to contain S source commands\n");
	fprintf (stdout,"(this file will be relative to your home directory)\n");
	fprintf (stdout,"or hit RETURN to cancel\n");
	fprintf (stdout,"\nfile> ");
	}
	if (!G_gets(file))
	{
	    print_prompt = 1;
	    continue;
	}
	print_prompt = 0;

	if (sscanf (file, "%s", name) != 1)
	    exit(-1);

	if (*name != '/')
	    sprintf(file,"%s/%s", home, name);
	else
	    strcpy (file, name);

	if (access (file, 0) == 0)
	{
	    fprintf (stdout,"<<%s>> file exists. please choose another file\n", file);
	    continue;
	}

	if (out = fopen (file,"w"))
	    return -1;
    }

    return -1;
}
