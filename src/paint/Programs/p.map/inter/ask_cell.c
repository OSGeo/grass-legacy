#include "gis.h"
#include "local_proto.h"
int 
ask_cell (FILE *fd)
{
    char name[50], *mapset;
    char fullname[100];
    char *mode;

/* ask for cell file */
    fprintf (stdout,"\nRASTER\n");
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
    fprintf (fd, "rast %s\n", G_fully_qualified_name (name, mapset));
    add_record (fullname);
    end_record();

/* ask for colormode */
    fprintf (stdout,"\nCOLORMODE\n");
    begin_record("COLORMODE:");
    do
    {
	fprintf (stdout,"select the method for coloring the map\n");
	fprintf (stdout,"\n");
	fprintf (stdout,"  1.  best colors (dithering to get a lot of colors)\n");
	fprintf (stdout,"  2.  approximate (convert colors to nearest printer color)\n");
	fprintf (stdout,"\n");
	fprintf (stdout,"> ");
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
