#include "gis.h"
dump_report (report_file, filter)
    char *report_file;
    char *filter;
{
    char *home;
    char name[200];
    char file[300];
    char command[1024];
    int print_prompt;

    new_report_screen();

    if (!(home = G_home ()))
    {
	printf("cant find home directory\n");
	return 0;
    }

    print_prompt = 1;
    while (1)
    {
	if (print_prompt)
	{
	    printf("\nenter the name of a file to hold report\n");
	    printf("(this file will be relative to your home directory)\n");
	    printf("or hit RETURN to cancel\n");
	}
	printf("\nfile> ");
	print_prompt = 1;
	if (!G_gets(file))
	    continue;
	print_prompt = 0;
	if (sscanf (file, "%s", name) != 1)
	    return 1;

	if (*name != '/')
	    sprintf(file,"%s/%s", home, name);
	else
	    strcpy (file, name);

	if (access (file, 0) == 0)
	{
	    printf("<<%s>> file exists. please choose another file\n", file);
	    continue;
	}

	sprintf (command, "%s < %s > %s", filter, report_file, file);
	system (command);
	printf("report dumped to <<%s>>\n", file);
	return 0;
    }
}
