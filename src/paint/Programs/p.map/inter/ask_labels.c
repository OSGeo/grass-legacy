#include "gis.h"
#include "local_proto.h"

int 
ask_labels (FILE *fd)
{
    char name[50], *mapset;
    char fullname[100];
    char *prompt;
    int any;

    fprintf (stdout,"\nLABELS\n");
    begin_record ("LABELS:");
    any = 0;
    prompt = "do you want to paint any labels files";
    while (yes(prompt))
	if ((mapset = G_ask_old("",name,"paint/labels","labels")) != NULL)
	{
	    sprintf (fullname, "%s in %s", name, mapset);
	    fprintf (fd, "labels %s\n", G_fully_qualified_name(name,mapset));
	    add_record (fullname);
	    prompt = "\ndo you want to paint any more labels files";
	    any = 1;
	}
    if (!any)
	add_record ("(none)");
    end_record();

  return 0;
}
