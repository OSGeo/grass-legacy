#include "gis.h"
ask_cell (fd)
    FILE *fd;
{
    char name[50], *mapset;
    char fullname[100];
    char *mode;

/* ask for cell file */
    printf ("\nCELL\n");
    begin_record("CELL:");
    do
    {
	if(!yes("do you want to paint a cell file"))
	{
	    add_record ("(none)");
	    end_record();
	    return 0;
	}
    }
    while ((mapset = G_ask_cell_old ("",name)) == NULL) ;

    sprintf (fullname, "%s in %s", name, mapset);
    fprintf (fd, "cell %s\n", fullname);
    add_record (fullname);
    end_record();

/* ask for colormode */
    printf ("\nCOLORMODE\n");
    begin_record("COLORMODE:");
    do
    {
	printf("select the method for coloring the map\n");
	printf("\n");
	printf("  1.  best colors (dithering to get a lot of colors)\n");
	printf("  2.  approximate (convert colors to nearest printer color)\n");
	printf("\n");
	printf("> ");
	if (!G_gets(fullname)) continue;
	G_strip (fullname);
    } while (strcmp (fullname, "1") && strcmp (fullname,"2"));

    mode = "best";
    if (*fullname == '2')
	mode = "approx";
    add_record(mode);
    end_record();
    fprintf (fd, "colormode %s\n", mode);
    return 1;
}
