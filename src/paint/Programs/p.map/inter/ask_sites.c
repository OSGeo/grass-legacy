#include "gis.h"

ask_sites (fd)
    FILE *fd;
{
    char name[50], *mapset;
    char fullname[100];
    char *prompt;
    int any;

    printf ("\nSITES\n");
    begin_record ("SITES:");
    any = 0;
    prompt = "do you want to paint any site files";
    while (yes(prompt))
	if ((mapset = G_ask_sites_old("",name)) != NULL)
	{
	    sprintf (fullname, "%s in %s", name, mapset);
	    fprintf (fd, "sites %s\n", G_fully_qualified_name(name,mapset));
	    ask_color (name, fd);
	    ask_for_icon (name, fd);
	    if (yes("should the site labels be printed as well?"))
		fprintf (fd, "  desc y\n");
	    fprintf (fd, "  end\n");
	    add_record (fullname);
	    prompt = "\ndo you want to paint any more site files";
	    any = 1;
	}
    if (!any)
	add_record ("(none)");
    end_record ();
}
