#include "gis.h"
#ifndef FILE
#include <stdio.h>
#endif

extern FILE *out;

open_outfile (file)

    char *file;
{
    char *home;
    char name[200];
    int print_prompt;


    if (!(home = G_home()))
    {
	printf("cant find home directory\n");
	exit(-1);
    }
    print_prompt = 1;

    while (1)
    {
	if (print_prompt)
	{
	printf("\nenter the name of a file to contain S source commands\n");
	printf("(this file will be relative to your home directory)\n");
	printf("or hit RETURN to cancel\n");
	printf("\nfile> ");
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
	    printf("<<%s>> file exists. please choose another file\n", file);
	    continue;
	}

	if (out = fopen (file,"w"))
	    return;
    }
}
