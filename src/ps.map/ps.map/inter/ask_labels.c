#include "gis.h"

ask_labels (fd)
    FILE *fd;
{
    char name[50], *mapset;
    char fullname[100];
    char *prompt;
    int any;

    printf ("\nLABELS\n");
    begin_record ("LABELS:");
    any = 0;
    prompt = "do you want to paint any labels files";
    while (yes(prompt))
	if ((mapset = G_ask_old("",name,"paint/labels","labels")) != NULL)
	{
	    sprintf (fullname, "%s in %s", name, mapset);
	    fprintf (fd, "labels %s\n", name);
    	    fprintf (fd, " end\n");
	    add_record (fullname);
	    prompt = "\ndo you want to paint any more labels files";
	    any = 1;
	}
    if (!any)
	add_record ("(none)");
    end_record();
}
