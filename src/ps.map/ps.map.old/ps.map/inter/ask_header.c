#include <string.h>
#include "gis.h"
#include "ps_map.h"

int ask_header (FILE *fd)
{
    char buf[1024];
    char file[300];
    FILE *in;

    fprintf (stdout,"\nHEADER\n");
    if (!yes("do you have a header file to be printed above the map"))
        return 1;

    while (1)
    {
    	fprintf (stdout,"\nEnter the name of an existing header file\n");
    	fprintf (stdout,"Press RETURN to cancel request\n> ");
    	fgets(buf,1024,stdin);
    	G_strip(buf);
    	if (strlen(buf) == 0) return 1;
    	if (sscanf(buf, "%s", file) != 1) 
	{
	    fprintf (stdout,"** invalid file name **\n");
	    continue;
	}
    	if ((in = fopen(file, "r")) == NULL)
	{
	    fprintf (stdout,"\nCan't open %s\n", file);
	    continue;
	}
    	fclose(in);
	break;
    }
    fprintf(fd, "header %s\n", file);
    fprintf(fd, "  end\n");

    return 0;
}
