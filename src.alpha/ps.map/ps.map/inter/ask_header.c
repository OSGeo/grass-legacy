#include "gis.h"

ask_header(fd)
FILE *fd;
{
    char buf[1024];
    char file[300];
    FILE *in;

    printf("\nHEADER\n");
    if (!yes("do you have a header file to be printed above the map")) return;

    while (1)
    {
    	printf("\nEnter the name of an existing header file\n");
    	printf("Press RETURN to cancel request\n> ");
    	gets(buf);
    	G_strip(buf);
    	if (strlen(buf) == 0) return;
    	if (sscanf(buf, "%s", file) != 1) 
	{
	    printf("** invalid file name **\n");
	    continue;
	}
    	if ((in = fopen(file, "r")) == NULL)
	{
	    printf("\nCan't open %s\n", file);
	    continue;
	}
    	fclose(in);
	break;
    }
    fprintf(fd, "header %s\n", file);
    fprintf(fd, "  end\n");
}
