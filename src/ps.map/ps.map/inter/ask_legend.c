#include "gis.h"

ask_legend (fd, cf)
    FILE *fd;
{
    char buf[1024];
    char file[300];
    FILE *in;

    printf("\nLEGEND\n");
    if (cf && yes("would you like a full color table included in the legend"))
	fprintf (fd, "colortable y\n  end\n");
    if (!yes("do you have a file of comments to be included in the legend"))
    {
	return;
    }

    while (1)
    {
    	printf("\nEnter the name of an existing comment file\n");
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
    fprintf(fd, "comments %s\n", file);
    fprintf(fd, "  end\n");
}
