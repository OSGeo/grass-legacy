
#include "gis.h"
#include "nfiles.h"

static char *intro[] =
{
"",
"The first data layer specified has \"holes\" to be filled, and",
"the other layers you specify have the data to fill the holes.",
"Specifically, the zero data in the first layer are replaced by",
"non-zero values from the second. Zero data values which remain",
"are replaced by non-zero from the third layer, etc.",
"",
0};

int 
main (int argc, char *argv[])
{
    char command[1024];
    char name[40];
    char *mapset;
    int nfiles;
    char prompt[80];
    int i;

    G_gisinit (argv[0]);
    G_clear_screen ();
    fprintf (stdout,"                           PATCH\n");

    fprintf (stdout,"This program allows you to patch from 2 to %d files together.\n",
	MAXFILES);
    for (i = 0; intro[i]; i++)
	fprintf (stdout,"%s\n", intro[i]);

    sprintf (command, "r.patch ");
    for (nfiles = 0; nfiles < MAXFILES; nfiles++)
    {
	if (nfiles == 0)
	    sprintf (prompt, "Enter first layer (ie, layer with holes to be filled)");
	else if (nfiles == 1)
	    sprintf (prompt, "Enter second layer");
	else
	{
	    G_set_ask_return_msg ("if all patch layers have been entered");
	    sprintf (prompt, "Enter layer %d (optional)", nfiles+1);
	}
	mapset = G_ask_cell_old (prompt, name);
	if (mapset == NULL)
	    break;
	strcat (command, "'");
	strcat (command, G_fully_qualified_name(name,mapset));
	strcat (command, "' ");
    }
    if (nfiles < 2) exit(0);

    G_clear_screen ();
    fprintf (stdout,"                           PATCH\n");

    if (G_ask_cell_new ("Enter name for resultant data layer", name) == NULL)
	exit(0);
    strcat (command, name);
    fprintf (stdout,"\n\npatching ... "); fflush (stdout);
    system (command);
    exit(0);
}
