#include "gis.h"
#include "local_proto.h"

int 
ask_legend (FILE *fd, int cf)
{
    char buf[1024];
    char file[300];
    FILE *in;

    fprintf (stdout,"\nLEGEND\n");
    if (cf && yes("would you like a full color table included in the legend"))
	fprintf (fd,"colortable y\n");
    if (!yes("do you have any comments to be included in the legend"))
	return 1;
    fprintf (stdout,"enter comments line by line. type 'end' when complete.\n");
    fprintf (stdout,"if you have comments in a file, type '<file'\n");

    fprintf (fd, "comments\n");
    while (1)
    {
	fprintf (stdout,">");
	if(!(fgets(buf,1024,stdin)&&strcmp (buf,"end")))
	    break;
	if (*buf == '<')
	{
	    if (sscanf (buf+1, "%s", file) != 1)
		fprintf (stdout,"** no file specified **\n");
	    else if ((in = fopen(file,"r")) == NULL)
		perror (file);
	    else
	    {
		while (G_getl (buf, sizeof buf, in))
		    fprintf (fd, "\\%s\n", buf);
		fclose (in);
	    }
	    fprintf (stdout,"if you have more comments, enter them line by line. type 'end' when complete.\n");
	    fprintf (stdout,"if you have more comments in another file, type '<file'\n");
	}
	else
	    fprintf (fd, "\\%s\n", buf);
    }
    fprintf (fd, "end\n");

    return 0;
}
