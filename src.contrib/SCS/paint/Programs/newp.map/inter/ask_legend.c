#include "gis.h"

ask_legend (fd,cf)
    FILE *fd;
{
    char buf[1024];
    char file[300];
    FILE *in;

    printf ("\nLEGEND\n");
    if (cf && yes("would you like a full color table included in the legend"))
	fprintf (fd,"colortable y\n");
    if (!yes("do you have any comments to be included in the legend"))
	return;
    printf ("enter comments line by line. type 'end' when complete.\n");
    printf ("if you have comments in a file, type '<file'\n");

    fprintf (fd, "comments\n");
    while (1)
    {
	printf (">");
	if (!(gets(buf) && strcmp (buf,"end")))
	    break;
	if (*buf == '<')
	{
	    if (sscanf (buf+1, "%s", file) != 1)
		printf ("** no file specified **\n");
	    else if ((in = fopen(file,"r")) == NULL)
		perror (file);
	    else
	    {
		while (G_getl (buf, sizeof buf, in))
		    fprintf (fd, "\\%s\n", buf);
		fclose (in);
	    }
	    printf ("if you have more comments, enter them line by line. type 'end' when complete.\n");
	    printf ("if you have more comments in another file, type '<file'\n");
	}
	else
	    fprintf (fd, "\\%s\n", buf);
    }
    fprintf (fd, "end\n");
}
