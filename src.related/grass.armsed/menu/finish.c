#define EXTERN extern

#include "menu.h"

finish()
{

    FILE *fd;
    char *mapset;
    char answer;
    char get_answer();

    printf("Do you really want to quit now? [y/n] ");
    answer = get_answer();

    if (answer == 'y')
    {
	fd = G_fopen_new("multsed/project",proj_name);
	if (!fd)
	{
	    fprintf(stderr,"Warning -- unable to store current parameters\n");
	    exit(-1);
	}

	write_mult(fd);
	fclose(fd);

        exit(0);
    }

    return;

}
