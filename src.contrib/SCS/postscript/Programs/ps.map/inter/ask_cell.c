#include "gis.h"
ask_cell (fd)
    FILE *fd;
{
    char name[50], *mapset;
    char fullname[100];
    char answer [100];
    char *mode;

/* ask for cell file */
    printf ("\nRASTER\n");
    begin_record("RASTER:");
    do
    {
	if(!yes("do you want to paint a raster file"))
	{
	    add_record ("(none)");
	    end_record();
	    return 0;
	}
    }
    while ((mapset = G_ask_cell_old ("",name)) == NULL) ;

    sprintf (fullname, "%s in %s", name, mapset);
    printf("color or greyscale ");
    while (1)
    {
	printf("(c/g) ");
	input (answer);
	switch (*answer)
	{
	    case 'c': case 'C': fprintf (fd, "rast %s\n", name); break;
	    case 'g': case 'G': fprintf (fd, "greyrast %s\n", name); break;
	    default: continue;
	}
	break;
    }
    add_record (fullname);
    end_record();

    return 1;
}
