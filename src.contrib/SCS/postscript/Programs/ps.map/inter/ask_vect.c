#include "gis.h"

ask_vectors (fd)
    FILE *fd;
{
    char name[50], *mapset;
    char fullname[100];
    char *prompt;
    int any;

    printf ("\nVECTORS\n");
    begin_record ("VECTORS:");
    any = 0;
    prompt = "do you want to paint any vector files";
    while (yes(prompt))
	if ((mapset = G_ask_vector_old("",name)) != NULL)
	{
	    sprintf (fullname, "%s in %s", name, mapset);
	    fprintf (fd, "vect %s\n", name);
	    ask_color (name, fd);
	    ask_width (name, fd);
	    ask_masked (name, fd);
	    fprintf (fd, "  end\n");
	    add_record (fullname);
	    prompt = "\ndo you want to paint any more vector files";
	    any = 1;
	}
    if (!any)
	add_record ("(none)");
    end_record ();
}
